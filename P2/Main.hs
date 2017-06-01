{-# LANGUAGE OverloadedStrings          #-}
import Application () -- for YesodDispatch instance
import Foundation
import Yesod
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql
import Data.Text
import Data.Text.Encoding

connStr :: Text
connStr = "dbname=d9rjhf6gepl7dm host=ec2-54-227-237-223.compute-1.amazonaws.com user=mzmzsqkoohqmcz password=5e373c8ad434f2214672eddc8c9144f8708cb4f5d1b57626528f2170e49c9ee6 port=5432"

-- codenvy 3000
-- warp 8080 App
main :: IO ()
main = runStdoutLoggingT $ withPostgresqlPool (encodeUtf8 connStr) 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool 
    warp 3000 (App pool)
