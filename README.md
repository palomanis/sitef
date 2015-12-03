# sitehask
Site Haskell

Site em cena - site com informções sobre cinema, onde você pode cadastrar seus filmes e classifica-los como favorito e adicionando uma nota a ele.

   / HomeR GET
   Menu principal para cadastra ou Login
   
   /cadastro CadastroR GET POST
   Tela para cadastro de novos usuarios
   
   /listar ListarR GET
   Listar usuarios cadastrados
   
   /listarfilmes ListarfR GET
   Listar filmes cadastrados
   
   /pessoa/#PessoaId PessoaR GET
   Tela inicial dos usuarios
   
   /filme/#FilmeId FilmeR GET
   Tela do filme com suas respectivas informações
   
   /autor AutorR GET
   Nome do autor
   

   /cadastrofilme CadastrofR GET POST
  Tela para cadastro dos filmes
  
   /listarfavorito ListarFavoritoR GET
   Lista dos filmes favoritos do usuarios
   
   /genero GeneroR GET POST
   Tela de cadastro de genero
   
   /admin AdminR GET
   
   /favorito FavoritoR GET POST
   Tela para avaliar seu filme favorito
   
   /login LoginR GET POST
   Tela de login
   
   /welcome WelcomeR GET
   Tela de boas-vindas para o usuário
