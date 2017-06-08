{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleContexts,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns #-}
module Foundation where
import Yesod
import Yesod.Static
import Data.Text
import Data.Time.Calendar
import Database.Persist.Postgresql
    ( ConnectionPool, SqlBackend, runSqlPool, runMigration )

data Sitio = Sitio {getStatic :: Static, connPool :: ConnectionPool }

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Departamento
   nome Text
   sigla Text sqltype=varchar(3)
   deriving Show


Treinamento
   nome Text
   responsavel Text
   sala Text
   sigla Text sqltype=varchar(10)
   qtdPessoas Int
   dataVal Day
   dataAplic Day
   deriving Show

Funcionario
   nome Text
   idade Int
   salario Double
   dataNasc Day
   deriving Show
