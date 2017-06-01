{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}

module Profissao where

import Yesod
import Data.Text hiding (replace)
import Database.Persist.Postgresql
import Foundation
import GHC.Generics
import Network.HTTP.Types.Status


postProfissaoR       
postProfissaoR :: Handler TypedContent
postProfissaoR = do
    profissao <- requireJsonBody :: Handler Profissao -- leia um profissao
    aid <- runDB $ insert profissao -- insira profissao
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey aid)]) -- retorna
    
getBuscarProfissaoR :: ProfissaoId -> Handler TypedContent
getBuscarProfissaoR aid = do
    profissao <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["resp" .= profissao])
    
getListProfissaoR :: Handler TypedContent
getListProfissaoR = do
    profissaos <- runDB $ selectList [] [Asc ProfissaoNome]
    sendStatusJSON ok200 (object ["resp" .= profissaos])

deleteApagarProfissaoR :: ProfissaoId -> Handler TypedContent
deleteApagarProfissaoR aid = do
    _ <- runDB $ get404 aid
    runDB $ delete aid
    sendStatusJSON noContent204 (object ["resp" .= ("DELETED " ++ show (fromSqlKey aid))])

putAlterarProfissaoR :: ProfissaoId -> Handler TypedContent
putAlterarProfissaoR aid = do
    _ <- runDB $ get404 aid
    profissao <- requireJsonBody :: Handler Profissao
    runDB $ replace aid profissao
    sendStatusJSON noContent204 (object ["resp" .= ("UPDATED " ++ show (fromSqlKey aid))])

