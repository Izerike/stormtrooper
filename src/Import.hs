{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
 
pRoutes = [parseRoutes|
   /user UsuarioR GET POST
   /listar ListUserR GET
   /static StaticR Static getStatic
   /ima ImgR GET
   /login LoginR GET POST
   /a/ WelcomeR GET
   /bye ByeR GET
   /admin AdminR GET
   / HomeR GET
   /equipe EquipeR GET
|]