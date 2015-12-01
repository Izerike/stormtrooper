{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Handlers where
import Import
import Yesod
import Yesod.Static
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Text.Lucius


import Database.Persist.Postgresql

mkYesodDispatch "Sitio" pRoutes

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "teste.lucius")
widgetTemplate ::  Widget
widgetTemplate = do
     $(whamletFile "home.hamlet")
     toWidget $(luciusFile "home.lucius")
widgetEquipe ::  Widget
widgetEquipe = do
     $(whamletFile "equipe.hamlet")
     toWidget $(luciusFile "home.lucius")
--widgetServico ::  Widget
--widgetServico = do
   --  $(whamletFile "list.hamlet")
   --  toWidget $(luciusFile "home.lucius")

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

getImgR :: Handler Html
getImgR = defaultLayout [whamlet| 
    <img src=@{StaticR empolgou_jpg}> 
|]

getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
     defaultLayout [whamlet|
        $maybe m <- usr
            <h1> Welcome #{m}
     |]

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formUsu
    defaultLayout $ widgetForm LoginR enc wid "" "Log in"

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

postUsuarioR :: Handler Html
postUsuarioR = do
    ((result,_),_) <- runFormPost formUsu
    case result of
        FormSuccess usr -> do
            runDB $ insert usr
            setMessage $ [shamlet| <p> Usuario inserido com sucesso! |]
            redirect UsuarioR
        _ -> redirect UsuarioR


getHomeR :: Handler Html
getHomeR = defaultLayout $ widgetTemplate 

getEquipeR :: Handler Html
getEquipeR = defaultLayout $ widgetEquipe


getListUserR :: Handler Html
getListUserR = do
    listaU <- runDB $ selectList [] [Asc UsuarioNome]
    defaultLayout $ do 
        toWidget  $(luciusFile "home.lucius")
        $(whamletFile "list.hamlet") 
        
getByeR :: Handler Html
getByeR = do
    deleteSession "_ID"
    defaultLayout [whamlet| BYE! |]

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1> Bem-vindo ADMIN!! |]

connStr = "dbname=d4nnmskudjtloa host=ec2-107-21-223-110.compute-1.amazonaws.com user=npraiqxlmbrgar password= Y-Rx12gmawtZuCsFLfqOYs0UNr port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)