{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
module Main where

import System.Environment
import Control.Monad
import Data.Char

import Paradox
import Excel
import Datatypes

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <Input file>\nWhere <Input file> is an excel file (.xlsx or .xlsm) containing .flk tables to be created."
    else do
        sheets <- readExcelFile (head args)
        forM_ sheets $ \(shName, shData) ->
            case flkFromSheet shData of
                Left err -> putStrLn $ "Skipped " ++ shName ++ "; " ++ err
                Right (path, table) -> do
                    putStrLn $ "Creating " ++ shName ++ " at\n" ++ path
                    writePxFlk table path

checkCell :: (Int,Int) -> Value -> Table -> Either String ()
checkCell (row,col) val table = do
    is <- indexTable (row, "Column" ++ show (col + 1)) table
    when (is /= val) $ Left $ show is ++ " /= " ++ show val

(<?>) :: Either String a -> String -> Either String a
r@(Right _) <?> _ = r
Left s <?> msg = Left $ msg ++ ": " ++ s
infixl 1 <?>

flkFromSheet :: Table -> Either String (FilePath, Table)
flkFromSheet sheet = do
    checkCell (0,0) (VString "ToImport:") sheet                     <?> "A1"
    checkCell (0,1) (VString "Yes") sheet                           <?> "B1"
    checkCell (1,0) (VString "TypeImport:") sheet                   <?> "A2"
    checkCell (1,1) (VString "Function") sheet                      <?> "B2"
    checkCell (2,0) (VString "SaveImportAs:") sheet                 <?> "A3"
    saveImportAs <- show <$> indexTable (2,"Column2") sheet         <?> "B3"
    checkCell (3,0) (VString "NumberInputs:") sheet                 <?> "A4"
    numberInputs <- indexTable (3,"Column2") sheet >>= valueToInt   <?> "B4"
    checkCell (4,0) (VString "NumberOutputs:") sheet                <?> "A5"
    numberOutputs <- indexTable (4,"Column2") sheet >>= valueToInt  <?> "B5"
    let expectedInputs = [ "Input " ++ show i | i <- [1..numberInputs] ]
        expectedOutputs = [ "Output " ++ show i | i <- [1..numberOutputs] ]
        expectedHeaders = expectedInputs ++ expectedOutputs
        typeTransformations = map (,valueConvertToInt) expectedInputs
                           ++ map (,valueConvertToDouble) expectedOutputs
    table <- tablePromoteHeaders (tableSkipRows 5 sheet)
         >>= tableSelectColumns expectedHeaders
         >>= tableTransformColumns typeTransformations
         >>= return . tableSelectRows (\(a:_) -> a /= VNull)
    Right (saveImportAs, table)



