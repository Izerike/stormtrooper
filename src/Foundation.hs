{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Import
import Yesod
import Yesod.Static
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import qualified Data.Text as T
import Data.Time
import Text.Lucius
import Yesod.Form.Jquery
import Data.Text
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio { connPool :: ConnectionPool,
                     getStatic :: Static }

staticFiles "."

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Usuario
   nome Text
   pass Text
   deriving Show
Fornecedor
   nome Text
   deriving Show

Servico
   nome Text sqltype=varchar(20)
   descricao Text
   dia Day Maybe
   estoque Int

Combo
   fornId FornecedorId
   servId ServicoId
   valor Double
   data UTCTime default=now()
   processado Bool
   UniqueFornServ fornId servId   

|]

mkYesodData "Sitio" pRoutes

instance YesodPersist Sitio where
   type YesodPersistBackend Sitio = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

instance Yesod Sitio where
    authRoute _ = Just $ LoginR
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized LoginR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized EquipeR _ = return Authorized
    isAuthorized ListUserR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser
--comentario
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Sitio FormMessage where
    renderMessage _ _ = defaultFormMessage
