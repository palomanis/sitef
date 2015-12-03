{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Import where

import Yesod
import Yesod.Static
 
pRoutes = [parseRoutes|
   / HomeR GET
   /cadastro CadastroR GET POST
   /listar ListarR GET
   /listarfilmes ListarfR GET
   /pessoa/#PessoaId PessoaR GET
   /filme/#FilmeId FilmeR GET
   /autor AutorR GET
   /static StaticR Static getStatic
   /cadastrofilme CadastrofR GET POST
   /listarfavorito ListarFavoritoR GET
   /genero GeneroR GET POST
   /admin AdminR GET
   /favorito FavoritoR GET POST
   /login LoginR GET POST
   /welcome WelcomeR GET
|]