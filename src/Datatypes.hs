{-# LANGUAGE
    RecordWildCards,
    DuplicateRecordFields,
    NamedFieldPuns,
    OverloadedRecordDot
#-}

module Datatypes where

import Data.List
import Text.Read
import Control.Monad (foldM)

data Value
    = VNull
    | VInt Int
    | VFloat Double
    | VBool Bool
    | VString String
    | VError String
    deriving (Eq)

data Table = Table
    { columnNames :: [String]
    , contents :: [[Value]]
    }

instance Show Value where
    show VNull = "null"
    show (VInt i) = show i
    show (VFloat f) = show f
    show (VBool b) = show b
    show (VString s) = s
    show (VError e) = "Error: " ++ e

instance Show Table where
    show Table {..} = unwords columnNames ++ "\n"
        ++ unlines [unwords [show val | val <- row] | row <- contents]

tableFromRows :: [String] -> [[Value]] -> Table
tableFromRows = Table

tableColumnNames :: Table -> [String]
tableColumnNames = columnNames

tableToRows :: Table -> [[Value]]
tableToRows = contents

tableToColumns :: Table -> [[Value]]
tableToColumns = transpose . contents

unsafeIndexTable :: (Int, Maybe Int) -> Table -> Value
unsafeIndexTable (row,Just col) Table {..} =
    let entireRow = contents !! row
        value = entireRow !! col
    in  value

indexTable :: (Int,String) -> Table -> Either String Value
indexTable (row,col) table@Table {..} =
    let rowExists = length contents > row && row >= 0
        colIndex = elemIndex col columnNames
        colExists = colIndex /= Nothing
    in  if not rowExists
            then Left $ "Row " ++ show row ++ " not found in table"
        else if not colExists
            then Left $ "Column " ++ col ++ " not found in table"
        else
            Right (unsafeIndexTable (row,colIndex) table)

valueToInt :: Value -> Either String Int
valueToInt VNull = Left "null"
valueToInt (VInt i) = Right i
valueToInt (VFloat f) = Right $ floor f
valueToInt (VBool b) = Right $ if b then 1 else 0
valueToInt (VString s) = case readEither s of
    Left _ -> Left $ s ++ " could not be converted to int"
    Right i -> Right i
valueToInt (VError e) = Left e

valueToDouble :: Value -> Either String Double
valueToDouble VNull = Left "null"
valueToDouble (VInt i) = Right $ fromIntegral i
valueToDouble (VFloat f) = Right f
valueToDouble (VBool b) = Right $ if b then 1.0 else 0.0
valueToDouble (VString s) = case readEither s of
    Left _ -> Left $ s ++ " could not be converted to number"
    Right f -> Right f
valueToDouble (VError e) = Left e

valueConvertToInt :: Value -> Value
valueConvertToInt VNull = VNull
valueConvertToInt v@(VInt _) = v
valueConvertToInt (VFloat f) = VInt $ floor f
valueConvertToInt (VBool b) = VInt $ if b then 1 else 0
valueConvertToInt (VString s) = case readEither s of
    Left _ -> VError $ s ++ " could not be converted to int"
    Right i -> VInt i
valueConvertToInt v@(VError e) = v

valueConvertToDouble :: Value -> Value
valueConvertToDouble VNull = VNull
valueConvertToDouble (VInt i) = VFloat $ fromIntegral i
valueConvertToDouble v@(VFloat f) = v
valueConvertToDouble (VBool b) = VFloat $ if b then 1.0 else 0.0
valueConvertToDouble (VString s) = case readEither s of
    Left _ -> VError $ s ++ " could not be converted to number"
    Right i -> VFloat i
valueConvertToDouble v@(VError e) = v

tableSkipRows :: Int -> Table -> Table
tableSkipRows i table = table { contents = drop i table.contents }

tablePromoteHeaders :: Table -> Either String Table
tablePromoteHeaders table =
    case uncons table.contents of
        Just (h,t) -> Right table
            { columnNames = map show h
            , contents = t
            }
        Nothing -> Left "[tablePromoteHeaders] empty table"

tableSelectColumns :: [String] -> Table -> Either String Table
tableSelectColumns names table =
    let missingColumns = filter (not . (`elem` table.columnNames)) names
        ix = map (`elem` names) table.columnNames
        select ix row = map snd $ filter fst $ zip ix row
        newNames = select ix table.columnNames
        newContents = map (select ix) table.contents
    in  if null missingColumns
        then Right table { columnNames = newNames, contents = newContents }
        else Left $ "[tableSelectColumns] missing columns: " ++ intercalate ", " missingColumns ++ "(found: " ++ intercalate ", " table.columnNames ++ ")"

{-
byRecords :: Table -> (String -> Value -> Value) -> Table
byRecords table f = table { contents = map (zipWith f table.columnNames) table.contents }
-}

tableTransformColumns :: [(String, Value -> Value)] -> Table -> Either String Table
tableTransformColumns fs table = foldM (flip tableTransformColumn) table fs

tableTransformColumn :: (String, Value -> Value) -> Table -> Either String Table
tableTransformColumn (name, f) table =
    case elemIndex name table.columnNames of
        Nothing -> Left $ "[tableTransformColumn] missing column: " ++ name
        Just ix ->
            let cols = tableToColumns table
                newCols = zipWith (\i c -> if i == ix then map f c else c) [0..] cols
                newTable = table { contents = transpose newCols }
            in  Right newTable

tableSelectRows :: ([Value] -> Bool) -> Table -> Table
tableSelectRows f table = table { contents = filter f table.contents }
