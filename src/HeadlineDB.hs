module HeadlineDB (connect) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when)
import Data.List

import DeclareDB

nounTable = Table "noun" [
    Column "noun_id" Integer [NonNull, PrimaryKey, AutoIncrement],
    Column "noun" Text [Unique, NonNull]
    ]

headlineTable = Table "headline" [
    Column "headline_id" Integer [NonNull, PrimaryKey, AutoIncrement],
    Column "headline" Text [NonNull]
    ]

connect :: FilePath -> IO Connection
connect path = do
    dbh <- connectSqlite3 path
    initializeTables dbh
    return dbh

initializeTables :: IConnection conn => conn -> IO ()
initializeTables dbh = do
    tables <- getTables dbh
    when (not ("noun" `elem` tables)) $ do
        run dbh (sqlCreateTable nounTable) []
        return ()
    when (not ("headline" `elem` tables)) $ do
        run dbh (sqlCreateTable headlineTable) []
        return ()
    commit dbh
