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

module Treinamento where

import Yesod
import Data.Text hiding (replace)
import Database.Persist.Postgresql
import Foundation
import GHC.Generics
import Network.HTTP.Types.Status

postTreinamentoR :: Handler TypedContent
postTreinamentoR = do
    treinamento <- requireJsonBody :: Handler Treinamento -- leia um treinamento
    aid <- runDB $ insert treinamento -- insira treinamento
    sendStatusJSON created201 (object ["resp" .= (fromSqlKey aid)]) -- retorna

getBuscarTreinamentoR :: TreinamentoId -> Handler TypedContent
getBuscarTreinamentoR aid = do
    treinamento <- runDB $ get404 aid
    sendStatusJSON ok200 (object ["resp" .= treinamento])    
 
getListTreinamentoR :: Handler TypedContent
getListTreinamentoR = do
    treinamentos <- runDB $ selectList [] [Asc TreinamentoNome]
    sendStatusJSON ok200 (object ["resp" .= treinamentos])   

deleteApagarTreinamentoR :: TreinamentoId -> Handler TypedContent
deleteApagarTreinamentoR aid = do
    _ <- runDB $ get404 aid
    runDB $ delete aid
    sendStatusJSON noContent204 (object ["resp" .= ("DELETED " ++ show (fromSqlKey aid))])

putAlterarTreinamentoR :: TreinamentoId -> Handler TypedContent
putAlterarTreinamentoR aid = do
    _ <- runDB $ get404 aid
    treinamento <- requireJsonBody :: Handler Treinamento
    runDB $ replace aid treinamento
    sendStatusJSON noContent204 (object ["resp" .= ("UPDATED " ++ show (fromSqlKey aid))])




