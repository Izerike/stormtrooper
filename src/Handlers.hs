{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
 
module Handlers where
import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import Text.Lucius
import Yesod.Form.Jquery

import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "home.lucius")
     
widgetLogin :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetLogin x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "login.hamlet")
     toWidget $(luciusFile "home.lucius")

widgetTemplate ::  Widget
widgetTemplate = do
     $(whamletFile "home.hamlet")
     toWidget $(luciusFile "home.lucius")
widgetEquipe ::  Widget
widgetEquipe = do
     $(whamletFile "equipe.hamlet")
     toWidget $(luciusFile "home.lucius")
widgetAdm ::  Widget
widgetAdm = do
     $(whamletFile "adm.hamlet")
     toWidget $(luciusFile "home.lucius")
     


instance YesodJquery Sitio where
    



formCombo :: Form Combo
formCombo = renderDivs $ Combo <$>
             areq (selectField forns) "Serv" Nothing <*>
             areq (selectField servs) "Forn" Nothing <*>
             areq doubleField "Valor" Nothing <*>
             lift (liftIO getCurrentTime) <*>
             lift (liftIO $ return False)
servs = do
       entidades <- runDB $ selectList [] [Asc ServicoNome] 
       optionsPairs $ fmap (\ent -> (servicoNome $ entityVal ent, entityKey ent)) entidades

forns = do
       entidades <- runDB $ selectList [] [Asc FornecedorNome] 
       optionsPairs $ fmap (\ent -> (fornecedorNome $ entityVal ent, entityKey ent)) entidades

formServ :: Form Servico
formServ = renderDivs $ Servico <$>
             areq textField "Nome" Nothing <*>
             areq textField "Desc" Nothing <*>
             aopt (jqueryDayField def { jdsChangeYear = True 
                 , jdsYearRange = "1980:2015" 
                  }) "Chegada" Nothing <*>
             areq intField "Valor" Nothing
formForn :: Form Fornecedor
formForn = renderDivs $ Fornecedor <$>
             areq textField "Nome" Nothing
getServR :: Handler Html
getServR = do
    (wid,enc) <- generateFormPost formServ
    defaultLayout $ widgetForm ServR enc wid "Cadastro de Serviços" "Cadastrar"

formUsu :: Form Usuario
formUsu = renderDivs $ Usuario <$>
    areq textField FieldSettings{fsLabel = "",
                                 fsId=Just "hident22",
                                 fsTooltip= Nothing,
                                 fsName= Nothing,
                                 fsAttrs=[("placeholder","Usuario")]} Nothing <*>
    areq passwordField FieldSettings{fsLabel = "",
                                 fsId=Just "hident23",
                                 fsTooltip= Nothing,
                                 fsName= Nothing,
                                 fsAttrs=[("placeholder","Senha")]} Nothing
getUsuarioR :: Handler Html
getUsuarioR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm UsuarioR enc wid "Cadastro de Usuarios" "Cadastrar"

getEmpresaR :: Handler Html
getEmpresaR = do
    (wid,enc) <- generateFormPost formForn
    defaultLayout $ widgetForm EmpresaR enc wid "Cadastro de Empresas" "Cadastrar"

    
getComboR :: Handler Html
getComboR = do
    (wid,enc) <- generateFormPost formCombo
    defaultLayout $ widgetForm ComboR enc wid "Cadastro de Combo" "Cadastrar"    



getWelcomeR :: Handler Html
getWelcomeR = do
     defaultLayout $ widgetAdm 

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetLogin LoginR enc wid "" "Log in"

postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            
            usuario <- runDB $ selectFirst [UsuarioNome ==. usuarioNome usr, UsuarioPass ==. usuarioPass usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (usuarioNome usr)
                    redirect WelcomeR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR 
        _ -> redirect LoginR

postServR :: Handler Html
postServR = do
    ((result,_),_) <- runFormPost formServ
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Servico inserido com sucesso! |]
            redirect ServR
        _ -> redirect ServR

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR

postEmpresaR :: Handler Html
postEmpresaR = do
    ((result,_),_) <- runFormPost formForn
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Empresa inserida com sucesso! |]
            redirect EmpresaR
        _ -> redirect EmpresaR
        

        
postComboR :: Handler Html
postComboR = do
    ((result,_),_) <- runFormPost formCombo
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Combo inserido com sucesso! |]
            redirect ComboR
        _ -> redirect ComboR

getHomeR :: Handler Html
getHomeR = defaultLayout $ widgetTemplate 

getEquipeR :: Handler Html
getEquipeR = defaultLayout $ widgetEquipe

getListUserR :: Handler Html
getListUserR = do
                 combos <- runDB $ (rawSql "SELECT ??, ??, ?? \
                                   \FROM combo INNER JOIN servico \
                                   \ON combo.serv_id=servico.id INNER JOIN fornecedor \
                                   \ON combo.forn_id=fornecedor.id" [])::Handler [(Entity Combo, Entity Servico, Entity Fornecedor)]
                 defaultLayout $ do 
                      toWidget  $(luciusFile "home.lucius")
                      $(whamletFile "list.hamlet") 


getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout [whamlet| Saindo... |]

getAdminR :: Handler Html
getAdminR = do
        defaultLayout $ widgetAdm 


connStr = "dbname=d4nnmskudjtloa host=ec2-107-21-223-110.compute-1.amazonaws.com user=npraiqxlmbrgar password= Y-Rx12gmawtZuCsFLfqOYs0UNr port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)