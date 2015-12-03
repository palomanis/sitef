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


formGenero :: Form Genero
formGenero = renderDivs $ Genero <$>
             areq textField "Genero" Nothing

formFilme :: Form Filme
formFilme = renderDivs $ Filme <$>
             areq textField "Nome" Nothing <*>
             areq textField "Diretor" Nothing <*>
             areq (selectField genero) "Genero" Nothing <*>
             areq textField FieldSettings{fsId=Just "hident22",
                            fsLabel="Sinopse",
                            fsTooltip=Nothing,
                            fsName= Nothing,
                            fsAttrs=[("maxlength","800")]} Nothing


formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Usuario" Nothing <*>
             areq passwordField "Senha" Nothing 

formFavorito :: Form Favorito
formFavorito = renderDivs $ Favorito <$>
             areq (selectField filme) "Nome" Nothing <*>
             areq (selectField pessoa) "Usuario" Nothing <*>
             areq intField "Nota" Nothing <*>
             lift (liftIO $ return False)

genero = do
        entities <- runDB $ selectList [] [Asc GeneroGenero]
        optionsPairs $ Prelude.map (\en -> (generoGenero $ entityVal en, entityKey en)) entities

filme = do
       entidades <- runDB $ selectList [] [Asc FilmeNome] 
       optionsPairs $ fmap (\ent -> (filmeNome $ entityVal ent, entityKey ent)) entidades

pessoa = do
       entidades <- runDB $ selectList [] [Asc PessoaUsuario] 
       optionsPairs $ fmap (\ent -> (pessoaUsuario $ entityVal ent, entityKey ent)) entidades

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "cs.lucius")

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
<body bgcolor="black">
   <img aling="center" src=@{StaticR ccharliechaplin_jpg}>
     <h1 align="center"> <FONT color="white"> Bem Vindo ao site Em cena</h1>
         <h2 align="center"> <FONT color="white"> Seu site de informações sobre cinema</h2>
           <h3> <FONT color="blue"> Faça seu cadastro <a href=cadastro>aqui</a></h3>
             <h3> <FONT color="blue"> Se ja possui um, faça seu login <a href=login>aqui</a></h3>
|]

getAutorR :: Handler Html
getAutorR = defaultLayout [whamlet|
    <body bgcolor="black">
          <h1> <FONT color="white"> Autor: Lucas Palomanis Gomes</h1>
                     <h3> <FONT color="white">Voltar ao menu <a href=@{WelcomeR}>principal</a>
|]

ww :: Widget
ww = toWidgetHead [hamlet|
<link rel="author" href=@{AutorR} title="Sobre...">
|]

getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
     defaultLayout [whamlet|
<body bgcolor="black">
<div bgcolor="#A9A9A9">

      <div style="background-color:black; padding: 2px;">
                 <a href="@{LoginR}" title="Menu Principal" style="color:whitesmoke; text-decoration: none; text-align:left;"> Menu Principal 
   
               <div style="background-color:black; padding: 5px; text-align: left;">
                 <a href="@{HomeR}" title="Página Inicial" style="color:whitesmoke; text-decoration: none;"> Home // 
                 <a href="@{WelcomeR}" title="Home" style="color:whitesmoke; text-decoration: none;"> Welcome // 
                 <a href="@{CadastrofR}" title="Cadastrar Filme" style="color:whitesmoke; text-decoration: none;">Cadastro Filme //
                 <a href="@{GeneroR}" title="Cadastrar Genero" style="color:whitesmoke; text-decoration: none;"> Cadastro Genero //
                  <a href="@{ListarfR}" title="Filmes Favoritos " style="color:whitesmoke; text-decoration: none;"> Filmes //   
                 <a href="@{ListarFavoritoR}" title="Filmes Favoritos " style="color:whitesmoke; text-decoration: none;"> Filmes Favoritos //
                 <a href="@{AutorR}" title="Autor" style="color:whitesmoke; text-decoration: none;"> Autor //
        $maybe m <- usr
               <h1> Welcome #{m}</h1>
            <img align="center" src=@{StaticR ccharliechaplin_jpg}>
     |]

getListarfR :: Handler Html
getListarfR = do
             listaF <- runDB $ selectList [] [Asc FilmeNome]
             defaultLayout [whamlet|
                <body bgcolor="black">
                 <h1> <FONT color="white"> Filmes cadastrados:
                 $forall Entity pid filme <- listaF
                    <h3> <a href=@{FilmeR pid}> #{filmeNome filme} <br><br> |]

getCadastrofR :: Handler Html
getCadastrofR = do 
    (wid,enc) <- generateFormPost formFilme
    defaultLayout $ widgetForm CadastrofR enc wid "Cadastro de Filme" "Cadastrar Filme"

getCadastroR :: Handler Html
getCadastroR = do
             (widget, enctype) <- generateFormPost formPessoa
             defaultLayout $ widgetForm CadastroR enctype widget "Cadastro de Usuario" "Cadastrar"

getGeneroR :: Handler Html
getGeneroR = do
         (widget, enctype) <- generateFormPost formGenero
         defaultLayout $ widgetForm GeneroR enctype widget "Cadastro de Genero" "Cadastrar"

getPessoaR :: PessoaId -> Handler Html
getPessoaR pid = do
             pessoa <- runDB $ get404 pid 
             defaultLayout [whamlet| 
                 <h1> Seja bem-vindx #{pessoaUsuario pessoa}
             |]

getFilmeR :: FilmeId -> Handler Html
getFilmeR pid = do
             filme <- runDB $ get404 pid
             defaultLayout [whamlet|
                 <body bgcolor="black">
                       <h1> <FONT color="white"> Nome: #{filmeNome filme}</h1>
                                  <h2> <FONT color="white"> Diretor: #{filmeDiretor filme}</h2>
                                             <h3> <FONT color="white"> Sinopse: #{filmeSinopse filme}</h3>
                     <h4>  <FONT color="white"> Deseja marcar como "favorito"? clique <a href=@{FavoritoR}>aqui</a></h3>
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaUsuario]
             defaultLayout [whamlet|
                 <body bgcolor="black">
                       <h1> Usuarios cadastrados:</h1>
                 $forall Entity pid pessoa <- listaP
                     <h3><a href=@{PessoaR pid}> #{pessoaUsuario pessoa} <br> <br>
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet|
                        <body bgcolor="black">
                        <h1> <FONT color="white"> #{pessoaUsuario pessoa} Inseridx com sucesso</h1><br>
                              <h3> <FONT color="white">Voltar ao menu <a href=@{WelcomeR}>principal</a>
                       |]
                    _ -> redirect CadastroR



postCadastrofR :: Handler Html
postCadastrofR = do
                ((result, _), _) <- runFormPost formFilme
                case result of
                    FormSuccess filme -> do
                       runDB $ insert filme
                       defaultLayout [whamlet|
                        <body bgcolor="black">
                        <h1> <FONT color="white"> #{filmeNome filme} Inserido com sucesso.</h1><br>
                               <h3> <FONT color="white"> Voltar ao menu <a href=@{WelcomeR}>principal</a></h3
                       |]
                    _ -> redirect CadastrofR

postGeneroR :: Handler Html
postGeneroR = do
                ((result, _), _) <- runFormPost formGenero
                case result of
                    FormSuccess genero -> do
                       runDB $ insert genero
                       defaultLayout [whamlet|<body bgcolor="black">
                            <h1> <FONT color="white"> #{generoGenero genero} Inserido com sucesso</h1><br>
                               <h3> <FONT color="white">Voltar ao menu <a href=@{WelcomeR}>principal</a></h3>
                       |]
                    _ -> redirect GeneroR

getLoginR :: Handler Html
getLoginR = do
    (wid,enc) <- generateFormPost formPessoa
    defaultLayout $ widgetForm LoginR enc wid "" "Log in"


postLoginR :: Handler Html
postLoginR = do
    ((result,_),_) <- runFormPost formPessoa
    case result of
        FormSuccess usr -> do
            usuario <- runDB $ selectFirst [PessoaUsuario ==. pessoaUsuario usr, PessoaSenha ==. pessoaSenha usr ] []
            case usuario of
                Just (Entity uid usr) -> do
                    setSession "_ID" (pessoaUsuario usr)
                    redirect WelcomeR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR
        _ -> redirect LoginR

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1> Bem-vindo ADMIN!! |]

getFavoritoR :: Handler Html
getFavoritoR = do
           (widget, enctype) <- generateFormPost formFavorito
           defaultLayout $ widgetForm FavoritoR enctype widget "Favorito" "Favoritar"

postFavoritoR :: Handler Html
postFavoritoR = do
            ((result,_),_) <- runFormPost formFavorito
            case result of
                FormSuccess x -> (runDB $ insert x) >> defaultLayout [whamlet|<body bgcolor="black"> <h1> Favoritado com sucesso</h1> 
                                                        <h3> <FONT color="white">Voltar ao menu <a href=@{WelcomeR}>principal</a></h3> |]
                _ -> redirect FavoritoR

getListarFavoritoR :: Handler Html
getListarFavoritoR = do
                 favoritos <- runDB $ (rawSql "SELECT ??, ??, ?? \
                                   \FROM favorito INNER JOIN pessoa \
                                   \ON favorito.pessoa_id=pessoa.id \
                                   \INNER JOIN filme \
                                   \ON favorito.filme_id=filme.id " [])::Handler [(Entity Favorito, Entity Pessoa, Entity Filme)]
                 defaultLayout [whamlet|
                      <h1> Lista de seus filmes Favoritos
                      $forall (Entity oq favorito, Entity _ np, Entity _ fn) <- favoritos
                          <p> Filmes favoritos #{fromSqlKey oq}: 
                            <p> Usuario: #{pessoaUsuario np} 
                               <p> Filme: #{filmeNome fn}

                    |]

connStr = "dbname=daabv190v9phsh host=ec2-54-204-13-220.compute-1.amazonaws.com user=uyktppkeqynryq password=7UILwm-sO9N5Vidyu2halCInzQ port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)


