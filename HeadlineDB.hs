module HeadlineDB where

import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Monad (when)
import Data.List

connect :: FilePath -> IO Connection
connect path = do
    dbh <- connectSqlite3 path
    initializeTables dbh
    return dbh

initializeTables :: IConnection conn => conn -> IO ()
initializeTables dbh = do
    tables <- getTables dbh
    when (not ("nouns" `elem` tables)) $ do
        run dbh (createTableSQL "nouns" "noun_id" ["noun NOT NULL UNIQUE"]) []
        return ()
    commit dbh

createTableSQL :: String -> String -> [String] -> String
createTableSQL name pk columns = createStmt ++ " (" ++ columnStmt ++ ")" where
        createStmt = "CREATE TABLE " ++ name
        columnStmt = intercalate "," $
            [pk ++ " INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT"] ++ columns
