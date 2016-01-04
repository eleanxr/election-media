module DeclareDB where

import Data.List (intercalate)
import Control.Monad

data DBTable = Table {
    tableName :: String,
    tableColumns :: [DBColumn]
} deriving (Show)

data DBColumn = Column {
    columnName :: String,
    columnType :: DBColumnType,
    columnConstraints :: [DBColumnConstraint]
} deriving (Show)

-- More to come here (discriminated union)
data DBColumnType =
    Text |
    Integer
    deriving (Show)

-- More to come here (discriminated union)
data DBColumnConstraint =
    PrimaryKey |
    AutoIncrement |
    Unique |
    NonNull
    deriving (Show)

sqlCreateTable :: DBTable -> String
sqlCreateTable table = createStmt ++ "(" ++ columnStmt ++ ")" where
    createStmt = "CREATE TABLE " ++ tableName table
    columnStmt = intercalate ", " $ map defineColumn (tableColumns table)

defineColumn :: DBColumn -> String
defineColumn column = intercalate " " $
    [columnName column] ++
    [mapType $ columnType column] ++
    (map mapConstraint $ columnConstraints column)

mapType Text = "TEXT"
mapType Integer = "INTEGER"

mapConstraint PrimaryKey = "PRIMARY KEY"
mapConstraint Unique = "UNIQUE"
mapConstraint NonNull = "NOT NULL"
mapConstraint AutoIncrement = "AUTOINCREMENT"
