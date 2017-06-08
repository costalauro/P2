{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Front where
import Yesod
import Foundation
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Handlers
import Database.Persist.Postgresql

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Cadastro de Treinamentos"
    addStylesheet $ StaticR teste_css
    sess <- lookupSession "_USER"
    toWidgetHead [hamlet|
        <meta name="keywords" content="Teste, Haskell">
    |]
    toWidgetHead [lucius|
            ul {
                list-style: none;
                align: center;
            }
            li {
                float:left;
                padding:5px;
            }
        |]
    [whamlet|
        <section>
            <header>
                <h1>
                    _{MsgHello}
                    <br>
                    
                        
            <footer>
                <div>
                    $maybe sess' <- sess
                        <form action=@{LogoutR} method=post>
                            <input type="submit" value="Logout">
    |]