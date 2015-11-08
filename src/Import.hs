{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
 
pRoutes = [parseRoutes|
   
   / CadastroR GET POST
   /listar ListarR GET
   /pessoa/#PessoaId PessoaR GET
   /depto DeptoR GET POST
   /cdusr/ UsuarioR GET POST
|]