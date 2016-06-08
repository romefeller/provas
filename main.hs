{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Yesod.Form
import Yesod.Static
import Text.Lucius
import Text.Julius
import Control.Applicative
import Data.Text (Text, pack)
import System.Directory
import Control.Monad.Logger (runStdoutLoggingT)
import System.FilePath
import Data.Time (utctDay, getCurrentTime)
import Database.Persist.Sqlite
import qualified Data.Text as T
import Import

data HelloForms = HelloForms {getStatic :: Static, connPool :: ConnectionPool}

instance RenderMessage HelloForms FormMessage where
    renderMessage _ _ = defaultFormMessage

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Imagens 
   nome Arquivo 
   deriving Show

Users json
   nome Text
   login Text
   senha Text
   deriving Show
|]

staticFiles "static"

mkYesod "HelloForms" [parseRoutes|
/file FileR GET POST
/imagem/#ImagensId ImageR GET
/erro ErroR GET
/static StaticR Static getStatic
/login LoginR GET POST
/perfil/#UsersId PerfilR GET
/cadastro/usuario UsuarioR GET POST
/logout LogoutR GET
|]

instance Yesod HelloForms where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser admin para entrar aqui"

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance YesodPersist HelloForms where
    type YesodPersistBackend HelloForms = SqlBackend
    runDB action = do
        HelloForms _ pool <- getYesod
        runSqlPool action pool

getImageR :: ImagensId -> Handler Html
getImageR iid = do
    img <- runDB $ get404 iid
    defaultLayout $ [whamlet|
        <img class="card-media" src=@{StaticR $ StaticRoute [nome $ imagensNome img] [] }>
    |]

widgetForm :: Route HelloForms -> Enctype -> Widget -> Text -> Widget
widgetForm x enctype widget y = $(whamletFile "templates/form.hamlet")

fileForm :: Form Imagens
fileForm = renderTable $ Imagens <$>
           (fmap ((Arquivo "") . Just) $ fileAFormReq "Required file")

getErroR :: Handler Html
getErroR = defaultLayout [whamlet| Erro |]

getFileR = do
    (widget, enctype) <- generateFormPost fileForm
    defaultLayout $ widgetForm FileR enctype widget "Arquivo"

postFileR :: Handler Html
postFileR = do
    ((res, form), enctype) <- runFormPost fileForm
    case res of
        FormSuccess img -> (writeToServer $ arq $ imagensNome img) >> (runDB $ insert img) >> redirect FileR
        _ -> redirect ErroR

getLoginR :: Handler Html
getLoginR = do
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ $(whamletFile "templates/login.hamlet")

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR


getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]

formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

uploadDirectory :: FilePath
uploadDirectory = "static"

writeToServer :: Maybe FileInfo -> Handler FilePath
writeToServer (Just file) = do
    let filename = T.unpack $ fileName file
        path = imageFilePath filename
    liftIO $ fileMove file path
    return filename
writeToServer Nothing = undefined

imageFilePath :: String -> FilePath
imageFilePath f = uploadDirectory </> f

main :: IO ()
main = do
    pool <- runStdoutLoggingT $ createSqlitePool "testing.db3" 10
    runSqlPersistMPool (runMigration migrateAll) pool
    t@(Static settings) <- static "static"
    warp 8080 (HelloForms t pool)
