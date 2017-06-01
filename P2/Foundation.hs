{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TypeFamilies               #-}
module Foundation where

import Yesod
import Data.Text
import Database.Persist
import Database.Persist.Postgresql
import Data.Time.Calendar

data App = App
    {
        connPool       :: ConnectionPool
    }

share [mkPersist sqlSettings, mkMigrate "migrateAll"][persistLowerCase|
    Funcionario json
        nome     Text
        dataNasc Day
    
    Profissao json
        nome     Text
        sigla    Text
    
    Treinamento json
        nome     Text
        sigla    Text
        cargaHoraria    Int
    
    FuncionarioProfissao json
        funcionarioid  FuncionarioId
        profissaoid   ProfissaoId
        UniqueFuncionarioTreinamento funcionarioid profissaoid
    
    FuncionarioTreinamento json
        funcionarioid  FuncionarioId
        treinamentoid   TreinamentoId
        UniqueFuncionarioTreinamento funcionarioid treinamentoid
|]


mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App

instance YesodPersist App where
   type YesodPersistBackend App = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool