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
                            fsAttrs=[("maxlength","2")]} Nothing


formPessoa :: Form Pessoa
formPessoa = renderDivs $ Pessoa <$>
             areq textField "Usuario" Nothing <*>
             areq passwordField "Senha" Nothing 

genero = do
        entities <- runDB $ selectList [] [Asc GeneroGenero]
        optionsPairs $ Prelude.map (\en -> (generoGenero $ entityVal en, entityKey en)) entities

widgetForm :: Route Sitio -> Enctype -> Widget -> Text -> Text -> Widget
widgetForm x enctype widget y val = do
     msg <- getMessage
     $(whamletFile "form.hamlet")
     toWidget $(luciusFile "teste.lucius")

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|
    <h1> Bem Vindo ao site Em cena
        <h2> Seu site de informações sobre cinema
        <h3> Faça seu cadastro <a href=cadastro>aqui</a>
                                  <h3> Se ja possui um, faça seu login <a href=login>aqui</a>
                                    <img src=@{StaticR cinema_jpg}>
|]

getAutorR :: Handler Html
getAutorR = defaultLayout [whamlet|
   <h1> Autor: Lucas Palomanis
|]

ww :: Widget
ww = toWidgetHead [hamlet|
<link rel="author" href=@{AutorR} title="Sobre...">
|]

getWelcomeR :: Handler Html
getWelcomeR = do
     usr <- lookupSession "_ID"
     defaultLayout [whamlet|
        $maybe m <- usr
            <h1> Welcome #{m}
     |]

getListarfR :: Handler Html
getListarfR = do
             listaF <- runDB $ selectList [] [Asc FilmeNome]
             defaultLayout [whamlet|
                 <h1> Filmes cadastrados:
                 $forall Entity pid filme <- listaF
                     <a href=@{FilmeR pid}> #{filmeNome filme} <br>
             |]

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
                 <h1> Nome: #{filmeNome filme}
                     <h2>Diretor: #{filmeDiretor filme}
             |]

getListarR :: Handler Html
getListarR = do
             listaP <- runDB $ selectList [] [Asc PessoaUsuario]
             defaultLayout [whamlet|
                 <h1> Usuarios cadastrados:
                 $forall Entity pid pessoa <- listaP
                     <a href=@{PessoaR pid}> #{pessoaUsuario pessoa} <br>
             |]

postCadastroR :: Handler Html
postCadastroR = do
                ((result, _), _) <- runFormPost formPessoa
                case result of
                    FormSuccess pessoa -> do
                       runDB $ insert pessoa 
                       defaultLayout [whamlet| 
                           <h1> #{pessoaUsuario pessoa} Inseridx com sucesso. 
                       |]
                    _ -> redirect CadastroR



postCadastrofR :: Handler Html
postCadastrofR = do
                ((result, _), _) <- runFormPost formFilme
                case result of
                    FormSuccess filme -> do
                       runDB $ insert filme
                       defaultLayout [whamlet|
                           <h1> #{filmeNome filme} Inserido com sucesso. 
                       |]
                    _ -> redirect CadastrofR

postGeneroR :: Handler Html
postGeneroR = do
                ((result, _), _) <- runFormPost formGenero
                case result of
                    FormSuccess genero -> do
                       runDB $ insert genero
                       defaultLayout [whamlet|
                           <h1> #{generoGenero genero} Inserido com sucesso. 
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
                    redirect ListarfR
                Nothing -> do
                    setMessage $ [shamlet| Invalid user |]
                    redirect LoginR
        _ -> redirect LoginR

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet| <h1> Bem-vindo ADMIN!! |]

connStr = "dbname=daabv190v9phsh host=ec2-54-204-13-220.compute-1.amazonaws.com user=uyktppkeqynryq password=7UILwm-sO9N5Vidyu2halCInzQ port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       s <- static "."
       warpEnv (Sitio pool s)


