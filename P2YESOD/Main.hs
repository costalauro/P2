{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Yesod
import Yesod.Static
import Foundation
import Application
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d9rjhf6gepl7dm host=ec2-54-227-237-223.compute-1.amazonaws.com user=mzmzsqkoohqmcz password=5e373c8ad434f2214672eddc8c9144f8708cb4f5d1b57626528f2170e49c9ee6 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 3000  (Sitio t pool)