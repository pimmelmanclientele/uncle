module Main where

import Data.ByteString.Lazy as BS
import Data.Binary.Get
import Data.Binary.Put
import Text.Pretty.Simple (pPrint)

import Paradox
import Excel

sample = "sample\\CAP.flk"
sampleIx = "sample\\CAP.fl1"
out = "sample\\CAP2.flk"
outIx = "sample\\CAP2.fl1"
sampleDel = "sample\\Del99999.del"
outDel = "sample\\Del99999_2.del"
sampleExcel = "sample\\Basys Unmet Assumptions (flk tables).xlsm"

main = testExcel

testExcel = do
  sampleData <- BS.readFile sampleExcel
  let table = readExcel sampleData
  pPrint table

testReadDB = do
  sampleData <- BS.readFile sample
  pPrint $ runGet getPxDatabase sampleData

testDB = do
  sampleData <- BS.readFile sample
  case readPxTable sampleData of
    Left err -> putStrLn err
    Right table -> do
      let outTable = createPxDel table
          outerTable = writePxDel table
      pPrint outTable
      BS.writeFile out outerTable
      case runGetOrFail getPxDatabase outerTable of
        Left (_, offset, err) -> print $ "Read failed at " ++ show offset ++ ": " ++ err ++ "\n"
        Right (_, _, table) -> pPrint table

testIx = do
  sampleData <- BS.readFile sampleIx
  case runGetOrFail getPxIndex sampleData of
    Left err -> print err
    Right (_, _, table) -> do
      let outTable = runPut $ putPxIndex table
      BS.writeFile outIx outTable
      case runGetOrFail getPxIndex outTable of
        Left (_, offset, err) -> print $ "Read failed at " ++ show offset ++ ": " ++ err ++ "\n"
        Right (_, _, table) -> pPrint table

testCreateIx = do
  sampleData <- BS.readFile sample
  case readPxTable sampleData of
    Left err -> putStrLn err
    Right table -> do
      let newTable = createPxFlk table
          (newTable', newIndex) = createIndex newTable 1
          newBinary = runPut $ putPxDatabase newTable'
          newIxBinary = runPut $ putPxIndex newIndex
      BS.writeFile out newBinary
      BS.writeFile outIx newIxBinary
      pPrint newIndex

testCreateDel = do
  sampleData <- BS.readFile sampleDel
  case readPxTable sampleData of
    Left err -> putStrLn err
    Right table -> do
      let newTable = createPxDel table
          newBinary = runPut $ putPxDatabase newTable
      BS.writeFile outDel newBinary
