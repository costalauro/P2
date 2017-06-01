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

module Funcionario where

import Yesod
import Data.Text hiding (replace)
import Database.Persist.Postgresql
import Foundation
import GHC.Generics
import Network.HTTP.Types.Status

postFuncionarioR :: Handler TypedContent
postFuncionarioR = do
    funcionario <- requireJsonBody :: Handler Funcionario -- leia um funcionario
    aid <- runDB $ insert funcionario -- insira funcionario
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey aid)]) -- retorna

getBuscarFuncionarioR :: FuncionarioId -> Handler TypedContent
getBuscarFuncionarioR aid = do
    funcionario <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["resp" .= funcionario])

getListFuncionarioR :: Handler TypedContent
getListFuncionarioR = do
    funcionarios <- runDB $ selectList [] [Asc FuncionarioNome]
    sendStatusJSON ok200 (object ["resp" .= funcionarios])

deleteApagarFuncionarioR :: FuncionarioId -> Handler TypedContent
deleteApagarFuncionarioR aid = do
    _ <- runDB $ get404 aid
    runDB $ delete aid
    sendStatusJSON noContent204 (object ["resp" .= ("DELETED " ++ show (fromSqlKey aid))])

putAlterarFuncionarioR :: FuncionarioId -> Handler TypedContent
putAlterarFuncionarioR aid = do
    _ <- runDB $ get404 aid
    funcionario <- requireJsonBody :: Handler Funcionario
    runDB $ replace aid funcionario
    sendStatusJSON noContent204 (object ["resp" .= ("UPDATED " ++ show (fromSqlKey aid))])