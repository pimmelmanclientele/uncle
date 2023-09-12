{-# LANGUAGE NamedFieldPuns #-}

module Excel where

import Codec.Xlsx hiding (Table)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Text (unpack)

import Datatypes

valueFromCell :: Cell -> Value
valueFromCell Cell { _cellValue } = case _cellValue of
    Nothing -> VNull
    Just (CellText s) -> VString $ unpack s
    Just (CellDouble f) -> VFloat f
    Just (CellBool b) -> VBool b
    Just (CellRich _) -> VError "Rich text"
    Just (CellError e) -> VError $ show e

getSheet :: Worksheet -> Table
getSheet Worksheet { _wsCells } =
    let rows = toRows _wsCells
        cells = [ [ valueFromCell v
                    | (_, v) <- row
                    ]
                  | (_, row) <- rows
                  ]
        colNames = if null cells
            then []
            else take (length (head cells)) $ map (\i -> "Column" ++ show i) [1..]
        table = tableFromRows colNames cells
    in  table

readExcel :: ByteString -> [(String, Table)]
readExcel bs =
    let Xlsx { _xlSheets } = toXlsx bs
        tables = map (\(n, s) -> (unpack n, getSheet s)) _xlSheets
    in  tables

readExcelFile :: FilePath -> IO [(String, Table)]
readExcelFile path = do
    fileData <- BS.readFile path
    return $ readExcel fileData
