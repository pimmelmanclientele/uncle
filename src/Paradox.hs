{-# LANGUAGE
    RecordWildCards,
    DuplicateRecordFields,
    NamedFieldPuns,
    OverloadedRecordDot,
    NoFieldSelectors,
    InstanceSigs,
    NegativeLiterals,
    LambdaCase
#-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Paradox where

import Data.Bits
import Data.Int
import Data.Word
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString.Lazy (ByteString, pack)
import qualified Data.ByteString.Lazy as BS
import Data.List (intercalate, isPrefixOf)
import Data.Array (Array, array, (!))
import qualified Data.Array as Array
import Control.Monad (zipWithM_)
import System.FilePath

import Datatypes

data FldInfoRec = FldInfoRec
    { fType :: Word8
    , fSize :: Word8
    } deriving (Show)

data PxCommonHeader = PxCommonHeader
    { recordSize :: Word16
    , headerSize :: Word16
    , fileType :: Word8
    , maxTableSize :: Word8
    , numRecords :: Word32
    , usedBlocks :: Word16
    , fileBlocks :: Word16
    , firstBlock :: Word16
    , lastBlock :: Word16
    , unknown1 :: Word16
    , modifiedFlags1 :: Word8
    , indexFieldNumber :: Word8
    , primaryIndexWorkspace :: Word32
    , unknown2 :: Word32
    , indexRoot :: Word16
    , numIndexLevels :: Word8
    , numFields :: Word16
    , primaryKeyFields :: Word16
    , encryption1 :: Word32
    , sortOrder :: Word8
    , modifiedFlags2 :: Word8
    , unknown3 :: Word16
    , changeCount1 :: Word8
    , changeCount2 :: Word8
    , unknown4 :: Word8
    , tableNamePtrPtr :: Word32
    , fldInfoPtr :: Word32
    , writeProtected :: Word8
    , fileVersionID :: Word8
    , maxBlocks :: Word16
    , unknown5 :: Word8
    , auxPasswords :: Word8
    , unknown6 :: Word16
    , cryptInfoStartPtr :: Word32
    , cryptInfoEndPtr :: Word32
    , unknown7 :: Word8
    , autoInc :: Word32
    , firstFreeBlock :: Word16
    , indexUpdateRequired :: Word8
    , unknown8 :: Word8
    , realHeaderSize :: Word16
    , unknown9 :: Word16
    , refIntegrity :: Word8
    , unknown10 :: Word16
    } deriving (Show)

data PxDataHeader = PxDataHeader
    { fileVerID3 :: Word16
    , fileVerID4 :: Word16
    , encryption2 :: Word32
    , fileUpdateTime :: Word32
    , hiFieldID :: Word16
    , hiFieldIDinfo :: Word16
    , sometimesNumFields :: Word16
    , dosCodePage :: Word16
    , unknown11 :: Word16
    , extraHeaderSize :: Word16
    , changeCount4 :: Word16
    , unknown12 :: [Word8]
    } deriving (Show)

data PxDataBlockHeader = PxDataBlockHeader
    { nextBlock :: Word16
    , prevBlock :: Word16
    , addDataSize :: Word16
    } deriving (Show)

data PxFieldType
    = PxfReserved1
    | PxfAlpha
    | PxfDate
    | PxfShort
    | PxfLong
    | PxfCurrency
    | PxfNumber
    | PxfReserved2
    | PxfReserved3
    | PxfLogical
    | PxfReserved4
    | PxfReserved5
    | PxfMemoBlob
    | PxfBlob
    | PxfFmtMemoBlob
    | PxfOLE
    | PxfGraphic
    | PxfReserved6
    | PxfReserved7
    | PxfReserved8
    | PxfTime
    | PxfTimestamp
    | PxfAutoInc
    | PxfBCD
    | PxfBytes
    deriving (Enum, Show)

data PxFieldInfo = PxFieldInfo
    { fieldName :: String
    , fieldType :: PxFieldType
    , fieldSize :: Word8
    } deriving (Show)

data PxField
    = FAlpha String
    | FShort Int16
    | FLong Int32
    | FNumber Double
    | FLogical Bool

instance Show PxField where
    show :: PxField -> String
    show (FAlpha s) = s
    show (FShort i) = show i
    show (FLong i) = show i
    show (FNumber n) = show n
    show (FLogical l) = show l

newtype PxRecord = PxRecord
    { values :: [PxField]
    }

instance Show PxRecord where
    show PxRecord {..} = "{" ++ intercalate "; " (map show values) ++ "}"

data PxIxRecord = PxIxRecord
    { keys :: [PxField]
    , blockNum :: Int16
    , countRecords :: Int16
    , unknown :: Int16
    } deriving (Show)

data PxDataBlock = PxDataBlock
    { pxDataBlockHeader :: PxDataBlockHeader
    , records :: [PxRecord]
    } deriving (Show)

data PxIxDataBlock = PxIxDataBlock
    { pxDataBlockHeader :: PxDataBlockHeader
    , records :: [PxIxRecord]
    } deriving (Show)

data PxDatabase = PxDatabase
    { tPxHeader :: PxCommonHeader
    , tPxDataHeader :: PxDataHeader
    , tableName :: String
    , fieldInfo :: [PxFieldInfo]
    , rawBlocks :: [PxDataBlock]
    } deriving (Show)

data PxIndex = PxIndex
    { tPxHeader :: PxCommonHeader
    , tableName :: String
    , fieldInfo :: [PxFieldInfo]
    , rawBlocks :: [PxIxDataBlock]
    } deriving (Show)

getPxCommonHeader :: Get PxCommonHeader
getPxCommonHeader = do
    recordSize <- getWord16le
    headerSize <- getWord16le
    fileType <- getWord8
    maxTableSize <- getWord8
    numRecords <- getWord32le
    usedBlocks <- getWord16le
    fileBlocks <- getWord16le
    firstBlock <- getWord16le
    lastBlock <- getWord16le
    unknown1 <- getWord16le
    modifiedFlags1 <- getWord8
    indexFieldNumber <- getWord8
    primaryIndexWorkspace <- getWord32le
    unknown2 <- getWord32le
    indexRoot <- getWord16le
    numIndexLevels <- getWord8
    numFields <- getWord16le
    primaryKeyFields <- getWord16le
    encryption1 <- getWord32le
    sortOrder <- getWord8
    modifiedFlags2 <- getWord8
    unknown3 <- getWord16le
    changeCount1 <- getWord8
    changeCount2 <- getWord8
    unknown4 <- getWord8
    tableNamePtrPtr <- getWord32le
    fldInfoPtr <- getWord32le
    writeProtected <- getWord8
    fileVersionID <- getWord8
    maxBlocks <- getWord16le
    unknown5 <- getWord8
    auxPasswords <- getWord8
    unknown6 <- getWord16le
    cryptInfoStartPtr <- getWord32le
    cryptInfoEndPtr <- getWord32le
    unknown7 <- getWord8
    autoInc <- getWord32le
    firstFreeBlock <- getWord16le
    indexUpdateRequired <- getWord8
    unknown8 <- getWord8
    realHeaderSize <- getWord16le
    unknown9 <- getWord16le
    refIntegrity <- getWord8
    unknown10 <- getWord16le
    return PxCommonHeader {..}

getPxDataHeader :: Get PxDataHeader
getPxDataHeader = do
    fileVerID3 <- getWord16le
    fileVerID4 <- getWord16le
    encryption2 <- getWord32le
    fileUpdateTime <- getWord32le
    hiFieldID <- getWord16le
    hiFieldIDinfo <- getWord16le
    sometimesNumFields <- getWord16le
    dosCodePage <- getWord16le
    unknown11 <- getWord16le
    extraHeaderSize <- getWord16le
    changeCount4 <- getWord16le
    unknown12 <- getCount 6 getWord8
    return PxDataHeader {..}

getFldInfoRec :: Get FldInfoRec
getFldInfoRec = do
    fType <- getWord8
    fSize <- getWord8
    return FldInfoRec {..}

getPtr :: Get Word32
getPtr = getWord32le

getPxDataBlockHeader :: Get PxDataBlockHeader
getPxDataBlockHeader = do
    nextBlock <- getWord16le
    prevBlock <- getWord16le
    addDataSize <- getWord16le
    return PxDataBlockHeader {..}

getField :: PxFieldInfo -> Get PxField
getField fieldInfo =
    case fieldInfo.fieldType of
        PxfAlpha -> do
            raw <- getByteString (fromIntegral fieldInfo.fieldSize)
            let val = clean . read . show $ raw
            return $ FAlpha val
        PxfShort -> do
            raw <- getInt16be
            let val = complementBit raw $ finiteBitSize raw - 1
            return $ FShort val
        PxfLong -> do
            raw <- getInt32be
            let val = complementBit raw $ finiteBitSize raw - 1
            return $ FLong val
        PxfNumber -> do
            raw <- getInt64be
            let raw' = if raw < 0 then complementBit raw $ finiteBitSize raw - 1 else complement raw
            let val = runGet getDoublebe (runPut $ putInt64be raw')
            return $ FNumber val
        PxfLogical -> do
            val <- (/=0) <$> getWord8
            return $ FLogical val
        _ -> error $ "PxField type " ++ show fieldInfo.fieldType ++ " not supported."

getRecord :: [PxFieldInfo] -> Get PxRecord
getRecord fieldInfo = do
    values <- mapM getField fieldInfo
    return PxRecord {..}

getIxRecord :: [PxFieldInfo] -> Get PxIxRecord
getIxRecord fieldInfo = do
    keys <- mapM getField fieldInfo
    FShort blockNum <- getField f
    FShort countRecords <- getField f
    FShort unknown <- getField f
    return PxIxRecord {..}
    where f = PxFieldInfo { fieldName = "", fieldType = PxfShort, fieldSize = 2 }

getPxDataBlock :: Int -> [PxFieldInfo] -> Int -> Get PxDataBlock
getPxDataBlock recordSize fieldInfo blockSize = do
    pxDataBlockHeader <- getPxDataBlockHeader
    let numRecs = fromIntegral pxDataBlockHeader.addDataSize `div` recordSize + 1
    records <- getCount numRecs (getRecord fieldInfo)
    bytes <- bytesRead
    skip $ blockSize - fromIntegral bytes
    return PxDataBlock {..}

getPxIxDataBlock :: Int -> [PxFieldInfo] -> Int -> Get PxIxDataBlock
getPxIxDataBlock recordSize fieldInfo blockSize = do
    pxDataBlockHeader <- getPxDataBlockHeader
    let numRecs = fromIntegral pxDataBlockHeader.addDataSize `div` recordSize + 1
    records <- getCount numRecs (getIxRecord fieldInfo)
    bytes <- bytesRead
    skip $ blockSize - fromIntegral bytes
    return PxIxDataBlock {..}

getPxDatabase :: Get PxDatabase
getPxDatabase = do
    tPxHeader <- getPxCommonHeader
    tPxDataHeader <- getPxDataHeader
    tFldInfoRecs <- getCount (fromIntegral tPxHeader.numFields) getFldInfoRec
    getPtr --table name pointer
    getCount (fromIntegral tPxHeader.numFields) getPtr --field name pointers
    let tableNameLen = case tPxHeader.fileVersionID of
            12 -> 261
            _ -> 79
    tableName <- clean . read . show <$> getByteString tableNameLen
    fieldNames <- getCount (fromIntegral tPxHeader.numFields) (read . show <$> getLazyByteStringNul)
    let addName FldInfoRec {..} n = PxFieldInfo
            { fieldName = n
            , fieldType = toEnum (fromIntegral fType)
            , fieldSize = fSize
            }
        fieldInfo = zipWith addName tFldInfoRecs fieldNames
    getCount (fromIntegral tPxHeader.numFields) getWord16le --fieldIndices
    getLazyByteStringNul --sort order
    skip (fromIntegral tPxHeader.headerSize - fromIntegral tPxHeader.realHeaderSize)
    rawBlocks <- getCount (fromIntegral tPxHeader.fileBlocks)
               $ isolate (fromIntegral tPxHeader.maxTableSize * 1024)
               $ getPxDataBlock (fromIntegral tPxHeader.recordSize) fieldInfo (fromIntegral tPxHeader.maxTableSize * 1024)
    check <- isEmpty
    if not check
        then fail "Did not consume all input"
        else return PxDatabase {..}

getPxIndex :: Get PxIndex
getPxIndex = do
    tPxHeader <- getPxCommonHeader
    tFldInfoRecs <- getCount (fromIntegral tPxHeader.numFields) getFldInfoRec
    getPtr --table name pointer
    let tableNameLen = case tPxHeader.fileVersionID of
            12 -> 261
            _ -> 79
    tableName <- clean . read . show <$> getByteString tableNameLen
    let addName FldInfoRec {..} n = PxFieldInfo
            { fieldName = n
            , fieldType = toEnum (fromIntegral fType)
            , fieldSize = fSize
            }
        fieldInfo = map (`addName` "") tFldInfoRecs
    skip (fromIntegral tPxHeader.headerSize - fromIntegral tPxHeader.realHeaderSize)
    rawBlocks <- getCount (fromIntegral tPxHeader.fileBlocks)
               $ isolate (fromIntegral tPxHeader.maxTableSize * 1024)
               $ getPxIxDataBlock (fromIntegral tPxHeader.recordSize) fieldInfo (fromIntegral tPxHeader.maxTableSize * 1024)
    check <- isEmpty
    if not check
        then fail "Did not consume all input"
        else return PxIndex {..}

order :: [PxDataBlock] -> [PxDataBlock]
order [] = []
order rawBlocks =
    let blockArr = array (1, fromIntegral $ length rawBlocks) $ zip [1..] rawBlocks
        order1 block
            | block.pxDataBlockHeader.nextBlock == 0 = [block]
            | otherwise = block : order1 (blockArr ! block.pxDataBlockHeader.nextBlock)
    in  order1 $ blockArr ! 1

valueFromPxField :: PxField -> Value
valueFromPxField f = case f of
    FAlpha s -> VString s
    FShort i -> if i == -0x8000
        then VNull
        else VInt (fromIntegral i)
    FLong i -> if i == -0x80000000
        then VNull
        else VInt (fromIntegral i)
    FNumber f -> VFloat f
    FLogical b -> VBool b

readPxTable :: ByteString -> Either String Table
readPxTable bs =
    case runGetOrFail getPxDatabase bs of
        Left (_, offset, err) -> Left $ "Read failed at " ++ show offset ++ ": " ++ err ++ "\n"
        Right (_, _, database) -> Right $
            let fieldNames = map (.fieldName) database.fieldInfo
                orderedBlocks = order database.rawBlocks
                recs = [ [ valueFromPxField f
                         | f <- record.values]
                       | block <- orderedBlocks
                       , record <- block.records ]
            in  tableFromRows fieldNames recs

getCount :: Int -> Get a -> Get [a]
getCount i _ | i <= 0 = return []
getCount i get = do
    d <- get
    ds <- getCount (i-1) get
    return (d:ds)

clean :: String -> String
clean = filter (/= '\0')



putPxCommonHeader :: PxCommonHeader -> Put
putPxCommonHeader PxCommonHeader {..} = do
    putWord16le recordSize
    putWord16le headerSize
    putWord8 fileType
    putWord8 maxTableSize
    putWord32le numRecords
    putWord16le usedBlocks
    putWord16le fileBlocks
    putWord16le firstBlock
    putWord16le lastBlock
    putWord16le unknown1
    putWord8 modifiedFlags1
    putWord8 indexFieldNumber
    putWord32le primaryIndexWorkspace
    putWord32le unknown2
    putWord16le indexRoot
    putWord8 numIndexLevels
    putWord16le numFields
    putWord16le primaryKeyFields
    putWord32le encryption1
    putWord8 sortOrder
    putWord8 modifiedFlags2
    putWord16le unknown3
    putWord8 changeCount1
    putWord8 changeCount2
    putWord8 unknown4
    putWord32le tableNamePtrPtr
    putWord32le fldInfoPtr
    putWord8 writeProtected
    putWord8 fileVersionID
    putWord16le maxBlocks
    putWord8 unknown5
    putWord8 auxPasswords
    putWord16le unknown6
    putWord32le cryptInfoStartPtr
    putWord32le cryptInfoEndPtr
    putWord8 unknown7
    putWord32le autoInc
    putWord16le firstFreeBlock
    putWord8 indexUpdateRequired
    putWord8 unknown8
    putWord16le realHeaderSize
    putWord16le unknown9
    putWord8 refIntegrity
    putWord16le unknown10

putPxDataHeader :: PxDataHeader -> Put
putPxDataHeader PxDataHeader {..} = do
    putWord16le fileVerID3
    putWord16le fileVerID4
    putWord32le encryption2
    putWord32le fileUpdateTime
    putWord16le hiFieldID
    putWord16le hiFieldIDinfo
    putWord16le sometimesNumFields
    putWord16le dosCodePage
    putWord16le unknown11
    putWord16le extraHeaderSize
    putWord16le changeCount4
    mapM_ putWord8 unknown12

putFldInfoRec :: FldInfoRec -> Put
putFldInfoRec FldInfoRec {..} = do
    putWord8 fType
    putWord8 fSize

putPtr :: Word32 -> Put
putPtr = putWord32le

putPxDataBlockHeader :: PxDataBlockHeader -> Put
putPxDataBlockHeader PxDataBlockHeader {..} = do
    putWord16le nextBlock
    putWord16le prevBlock
    putWord16le addDataSize

putField :: PxFieldInfo -> PxField -> Put
putField fieldInfo field =
    case field of
        FAlpha raw -> do
            let val = pad (fromIntegral fieldInfo.fieldSize) '\0' raw
            putStringUtf8 val
        FShort raw -> do
            let val = complementBit raw $ finiteBitSize raw - 1
            putInt16be val
        FLong raw -> do
            let val = complementBit raw $ finiteBitSize raw - 1
            putInt32be val
        FNumber raw -> do
            let raw' = runGet getInt64be (runPut $ putDoublebe raw)
            let val = if raw' > 0 then complementBit raw' $ finiteBitSize raw' - 1 else complement raw'
            putInt64be val
        FLogical raw -> do
            putWord8 $ fromIntegral $ fromEnum raw

putRecord :: [PxFieldInfo] -> PxRecord -> Put
putRecord fieldInfo PxRecord {..} = do
    zipWithM_ putField fieldInfo values

putIxRecord :: [PxFieldInfo] -> PxIxRecord -> Put
putIxRecord fieldInfo PxIxRecord {..} = do
    zipWithM_ putField fieldInfo keys
    putField f (FShort blockNum)
    putField f (FShort countRecords)
    putField f (FShort unknown)
    where f = PxFieldInfo { fieldName = "", fieldType = PxfShort, fieldSize = 2 }

putPxDataBlock :: Int -> [PxFieldInfo] -> Int -> PxDataBlock -> Put
putPxDataBlock recordSize fieldInfo blockSize PxDataBlock {..} = do
    putPxDataBlockHeader pxDataBlockHeader
    mapM_ (putRecord fieldInfo) records
    let bytes = fromIntegral pxDataBlockHeader.addDataSize + recordSize + 6
    putCount (blockSize - fromIntegral bytes) (putWord8 0)

putPxIxDataBlock :: Int -> [PxFieldInfo] -> Int -> PxIxDataBlock -> Put
putPxIxDataBlock recordSize fieldInfo blockSize PxIxDataBlock {..} = do
    putPxDataBlockHeader pxDataBlockHeader
    mapM_ (putIxRecord fieldInfo) records
    let bytes = fromIntegral pxDataBlockHeader.addDataSize + recordSize + 6
    putCount (blockSize - fromIntegral bytes) (putWord8 0)

putPxDatabase :: PxDatabase -> Put
putPxDatabase PxDatabase {..} = do
    putPxCommonHeader tPxHeader
    putPxDataHeader tPxDataHeader
    let removeName PxFieldInfo {..} = FldInfoRec
            { fType = fromIntegral (fromEnum fieldType)
            , fSize = fieldSize
            }
        tFldInfoRecs = map removeName fieldInfo
        fieldNames = map (.fieldName) fieldInfo
    mapM_ putFldInfoRec tFldInfoRecs
    putPtr 0 --table name pointer
    putCount (fromIntegral tPxHeader.numFields) (putPtr 0) --field name pointers
    let tableNameLen = case tPxHeader.fileVersionID of
            12 -> 261
            _ -> 79
    putStringUtf8 $ pad tableNameLen '\0' tableName
    mapM_ (putStringUtf8 . (++"\0")) fieldNames
    mapM_ putWord16le $ take (fromIntegral tPxHeader.numFields) [1..] --fieldIndices
    putStringUtf8 "DBWINUS0\0" --sort order
    putCount (fromIntegral tPxHeader.headerSize - fromIntegral tPxHeader.realHeaderSize) (putWord8 0)
    mapM_ (putPxDataBlock (fromIntegral tPxHeader.recordSize) fieldInfo (fromIntegral tPxHeader.maxTableSize * 1024)) rawBlocks

putPxIndex :: PxIndex -> Put
putPxIndex PxIndex {..} = do
    putPxCommonHeader tPxHeader
    let removeName PxFieldInfo {..} = FldInfoRec
            { fType = fromIntegral (fromEnum fieldType)
            , fSize = fieldSize
            }
        tFldInfoRecs = map removeName fieldInfo
    mapM_ putFldInfoRec tFldInfoRecs
    putPtr 0 --table name pointer
    let tableNameLen = case tPxHeader.fileVersionID of
            12 -> 261
            _ -> 79
    putStringUtf8 $ pad tableNameLen '\0' tableName
    putCount (fromIntegral tPxHeader.headerSize - fromIntegral tPxHeader.realHeaderSize) (putWord8 0)
    mapM_ (putPxIxDataBlock (fromIntegral tPxHeader.recordSize) fieldInfo (fromIntegral tPxHeader.maxTableSize * 1024)) rawBlocks

putCount :: Int -> Put -> Put
putCount i _ | i <= 0 = return ()
putCount i put = do
    put
    putCount (i-1) put

pad :: Int -> Char -> String -> String
pad i c str =
    let len = length str
        rem = i - len
        rem' = max rem 0
        padding = replicate rem' c
    in  str ++ padding



chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n  = l `c` splitter (drop i l) c n
    build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
    build g = g (:) []

createIndex :: PxDatabase -> Int -> (PxDatabase, PxIndex)
createIndex pxDatabase numIndices =
    let numFields = fromIntegral numIndices
        numRecords = fromIntegral $ pxDatabase.tPxHeader.fileBlocks

        fieldInfo = take numIndices pxDatabase.fieldInfo

        recordSize = (+6) $ fromIntegral $ sum $ map (fromIntegral . (.fieldSize)) fieldInfo :: Word16

        realHeaderSize
            = 88 --common header
            + numFields * 2 --field info
            + 4 --table name pointer
            + 261 --table name

        headerSize = (realHeaderSize `div` 2048 + 1) * 2048

        allRecords = map f pxDatabase.rawBlocks
            where f PxDataBlock {..} =
                    let indexedRec = (head records).values
                        indexedFields = take numIndices indexedRec
                        blockNum = fromIntegral pxDataBlockHeader.prevBlock + 1
                        countRecords = fromIntegral pxDataBlockHeader.addDataSize `div` fromIntegral pxDatabase.tPxHeader.recordSize + 1
                        unknown = 0
                        indexRec = PxIxRecord
                            { keys = indexedFields
                            , blockNum
                            , countRecords
                            , unknown
                            }
                    in  indexRec

        maxRecs = fromIntegral $ 2042 `div` recordSize
        
        usedBlocks = if numRecords > maxRecs
            then error $ "Total blocks (" ++ show numRecords ++ ") is more than 1-level index can fit (" ++ show maxRecs ++ ")"
            else 1

        rawBlocks = [ PxIxDataBlock
            { pxDataBlockHeader = PxDataBlockHeader
                { nextBlock = 0
                , prevBlock = 0
                , addDataSize = (fromIntegral numRecords - 1) * recordSize
                }
            , records = allRecords
            }]

        pxCommonHeader = PxCommonHeader
            { recordSize
            , headerSize
            , fileType = 1
            , maxTableSize = 2
            , numRecords
            , usedBlocks
            , fileBlocks = usedBlocks
            , firstBlock = 1
            , lastBlock = usedBlocks
            , unknown1 = 100
            , modifiedFlags1 = 0
            , indexFieldNumber = 0
            , primaryIndexWorkspace = 0
            , unknown2 = 0
            , indexRoot = 1
            , numIndexLevels = 1
            , numFields
            , primaryKeyFields = 0
            , encryption1 = 0
            , sortOrder = 76
            , modifiedFlags2 = 0
            , unknown3 = 14592
            , changeCount1 = 0
            , changeCount2 = 0
            , unknown4 = 0
            , tableNamePtrPtr = 0
            , fldInfoPtr = 0
            , writeProtected = 0
            , fileVersionID = 12
            , maxBlocks = usedBlocks
            , unknown5 = 0
            , auxPasswords = 0
            , unknown6 = 3871
            , cryptInfoStartPtr = 0
            , cryptInfoEndPtr = 0
            , unknown7 = 0
            , autoInc = 3
            , firstFreeBlock = 0
            , indexUpdateRequired = 0
            , unknown8 = 0
            , realHeaderSize
            , unknown9 = 0
            , refIntegrity = 0
            , unknown10 = 32
            }

        pxIndex = PxIndex
            { tPxHeader = pxCommonHeader
            , tableName = "Function.PX"
            , fieldInfo
            , rawBlocks
            }
        
        indexedDatabase :: PxDatabase
        indexedDatabase = pxDatabase
            { tPxHeader = pxDatabase.tPxHeader
                { primaryKeyFields = fromIntegral numIndices
                , fileType = 0
                }
            }

    in  (indexedDatabase, pxIndex)

createPxFlk :: Table -> PxDatabase
createPxFlk table =
    let fieldNames = tableColumnNames table
        rows = tableToRows table

        sampleRow = head rows
        numFields = fromIntegral $ length sampleRow
        numRecords = fromIntegral $ length rows

        fieldInfo = zipWith (\n val -> case val of
                VInt i -> PxFieldInfo
                    { fieldName = n
                    , fieldType = PxfLong
                    , fieldSize = 4
                    }
                VFloat f -> PxFieldInfo
                    { fieldName = n
                    , fieldType = PxfNumber
                    , fieldSize = 8
                    }
                _ -> error $ "Field [" ++ n ++ " = " ++ show val ++ "] has invalid type for .flk file"
            ) fieldNames sampleRow

        recordSize = fromIntegral $ sum $ map (fromIntegral . (.fieldSize)) fieldInfo :: Word16

        realHeaderSize
            = 88 --common header
            + 32 --data header
            + numFields * 2 --field info
            + (numFields + 1) * 4 --table name and field name pointers
            + 261 --table name
            + fromIntegral (sum (map ((+1) . length) fieldNames)) --field names plus null separators
            + numFields * 2 --field indices
            + 9 --sort order

        hiFieldIDinfo
            = 32 --data header
            + numFields * 2 --field info
            + (numFields + 1) * 4 --table name and field name pointers
            + 261 --table name
            + fromIntegral (sum (map ((+1) . length) fieldNames)) --field names plus null separators

        extraHeaderSize
            = 32 --data header
            + numFields * 2 --field info
            + (numFields + 1) * 4 --table name and field name pointers
            + 261 --table name
            + fromIntegral (sum (map ((+1) . length) fieldNames)) --field names plus null separators
            + numFields * 2 --field indices

        headerSize = (realHeaderSize `div` 2048 + 1) * 2048

        allRecords = [ PxRecord [ f val
                             | val <- row ]
                  | row <- rows ]
                  where
                    f (VInt i) = FLong $ fromIntegral i
                    f (VFloat v) = FNumber v

        maxRecs = fromIntegral $ 2042 `div` recordSize
        recBlocks = chunksOf maxRecs allRecords
        usedBlocks = fromIntegral $ length recBlocks :: Word16

        rawBlocks =
            let nextBlocks = take (fromIntegral usedBlocks - 1) [2..] ++ [0]
                prevBlocks = take (fromIntegral usedBlocks) [0..]
                f nextBlock prevBlock recBlock =
                    let addDataSize = (fromIntegral (length recBlock) - 1) * recordSize
                        pxDataBlockHeader = PxDataBlockHeader {..}
                    in  PxDataBlock {records = recBlock, ..}
            in  zipWith3 f nextBlocks prevBlocks recBlocks

        pxCommonHeader = PxCommonHeader
            { recordSize
            , headerSize
            , fileType = 2
            , maxTableSize = 2
            , numRecords
            , usedBlocks
            , fileBlocks = usedBlocks
            , firstBlock = 1
            , lastBlock = usedBlocks
            , unknown1 = 99
            , modifiedFlags1 = 0
            , indexFieldNumber = 0
            , primaryIndexWorkspace = 0
            , unknown2 = 0
            , indexRoot = 0
            , numIndexLevels = 0
            , numFields
            , primaryKeyFields = 0
            , encryption1 = 4278255360
            , sortOrder = 76
            , modifiedFlags2 = 0
            , unknown3 = 0
            , changeCount1 = 234
            , changeCount2 = 232
            , unknown4 = 0
            , tableNamePtrPtr = 0
            , fldInfoPtr = 0
            , writeProtected = 0
            , fileVersionID = 12
            , maxBlocks = usedBlocks
            , unknown5 = 0
            , auxPasswords = 0
            , unknown6 = 3871
            , cryptInfoStartPtr = 0
            , cryptInfoEndPtr = 0
            , unknown7 = 0
            , autoInc = 0
            , firstFreeBlock = 0
            , indexUpdateRequired = 0
            , unknown8 = 0
            , realHeaderSize
            , unknown9 = 0
            , refIntegrity = 0
            , unknown10 = 32
            }

        pxDataHeader = PxDataHeader
            { fileVerID3 = 268
            , fileVerID4 = 268
            , encryption2 = 0
            , fileUpdateTime = 0
            , hiFieldID = numFields + 1
            , hiFieldIDinfo
            , sometimesNumFields = 0
            , dosCodePage = 1252
            , unknown11 = 257
            , extraHeaderSize
            , changeCount4 = 1
            , unknown12 = replicate 6 0
            }

        pxDatabase = PxDatabase
            { tPxHeader = pxCommonHeader
            , tPxDataHeader = pxDataHeader
            , tableName = "function.DB"
            , fieldInfo
            , rawBlocks
            }

    in  pxDatabase

writePxFlk :: Table -> FilePath -> IO ()
writePxFlk table path = do
    let indexedColumns = length $ takeWhile (isPrefixOf "Input") $ table.columnNames
        database = createPxFlk table
        (indexedDatabase, index) = createIndex database indexedColumns
        databaseBinary = runPut $ putPxDatabase indexedDatabase
        indexBinary = runPut $ putPxIndex index
        databasePath = path -<.> "flk"
        indexPath = path -<.> "fl1"
    BS.writeFile databasePath databaseBinary
    BS.writeFile indexPath indexBinary

createPxDel :: Table -> PxDatabase
createPxDel table =
    let fieldNames = tableColumnNames table
        rows = tableToRows table

        sampleRow = head rows
        numFields = fromIntegral $ length sampleRow
        numRecords = fromIntegral $ length rows

        fieldInfo = zipWith (\n val -> case val of
                VInt i -> PxFieldInfo
                    { fieldName = n
                    , fieldType = PxfShort
                    , fieldSize = 2
                    }
                VString s -> PxFieldInfo
                    { fieldName = n
                    , fieldType = PxfAlpha
                    , fieldSize = fSizeOf n
                    }
                VNull -> PxFieldInfo
                    { fieldName = n
                    , fieldType = PxfShort
                    , fieldSize = 2
                    }
                _ -> error $ "Field [" ++ n ++ " = " ++ show val ++ "] has invalid type for .del file"
            ) fieldNames sampleRow
            where fSizeOf n
                    | "Field" `isPrefixOf` n = 50
                    | "Condition" `isPrefixOf` n = 4
                    | "Text" `isPrefixOf` n = 100
                    | otherwise = error $ "Field [" ++ n ++ "] has invalid name for .del file"

        recordSize = fromIntegral $ sum $ map (fromIntegral . (.fieldSize)) fieldInfo :: Word16

        realHeaderSize
            = 88 --common header
            + 32 --data header
            + numFields * 2 --field info
            + (numFields + 1) * 4 --table name and field name pointers
            + 261 --table name
            + fromIntegral (sum (map ((+1) . length) fieldNames)) --field names plus null separators
            + numFields * 2 --field indices
            + 9 --sort order

        hiFieldIDinfo
            = 32 --data header
            + numFields * 2 --field info
            + (numFields + 1) * 4 --table name and field name pointers
            + 261 --table name
            + fromIntegral (sum (map ((+1) . length) fieldNames)) --field names plus null separators

        extraHeaderSize
            = 32 --data header
            + numFields * 2 --field info
            + (numFields + 1) * 4 --table name and field name pointers
            + 261 --table name
            + fromIntegral (sum (map ((+1) . length) fieldNames)) --field names plus null separators
            + numFields * 2 --field indices

        headerSize = (realHeaderSize `div` 2048 + 1) * 2048

        allRecords = [ PxRecord [ f val
                             | val <- row ]
                  | row <- rows ]
                  where
                    f (VInt i) = FShort $ fromIntegral i
                    f (VString s) = FAlpha s
                    f VNull = FShort -0x8000

        maxRecs = fromIntegral $ 2042 `div` recordSize
        recBlocks = chunksOf maxRecs allRecords
        usedBlocks = fromIntegral $ length recBlocks :: Word16

        rawBlocks =
            let nextBlocks = take (fromIntegral usedBlocks - 1) [2..] ++ [0]
                prevBlocks = take (fromIntegral usedBlocks) [0..]
                f nextBlock prevBlock recBlock =
                    let addDataSize = (fromIntegral (length recBlock) - 1) * recordSize
                        pxDataBlockHeader = PxDataBlockHeader {..}
                    in  PxDataBlock {records = recBlock, ..}
            in  zipWith3 f nextBlocks prevBlocks recBlocks

        pxCommonHeader = PxCommonHeader
            { recordSize
            , headerSize
            , fileType = 2
            , maxTableSize = 2
            , numRecords
            , usedBlocks
            , fileBlocks = usedBlocks
            , firstBlock = 1
            , lastBlock = usedBlocks
            , unknown1 = 99
            , modifiedFlags1 = 0
            , indexFieldNumber = 0
            , primaryIndexWorkspace = 0
            , unknown2 = 0
            , indexRoot = 0
            , numIndexLevels = 0
            , numFields
            , primaryKeyFields = 0
            , encryption1 = 4278255360
            , sortOrder = 76
            , modifiedFlags2 = 0
            , unknown3 = 0
            , changeCount1 = 234
            , changeCount2 = 232
            , unknown4 = 0
            , tableNamePtrPtr = 0
            , fldInfoPtr = 0
            , writeProtected = 0
            , fileVersionID = 12
            , maxBlocks = usedBlocks
            , unknown5 = 0
            , auxPasswords = 0
            , unknown6 = 3871
            , cryptInfoStartPtr = 0
            , cryptInfoEndPtr = 0
            , unknown7 = 0
            , autoInc = 0
            , firstFreeBlock = 0
            , indexUpdateRequired = 0
            , unknown8 = 0
            , realHeaderSize
            , unknown9 = 0
            , refIntegrity = 0
            , unknown10 = 32
            }

        pxDataHeader = PxDataHeader
            { fileVerID3 = 268
            , fileVerID4 = 268
            , encryption2 = 0
            , fileUpdateTime = 0
            , hiFieldID = numFields + 1
            , hiFieldIDinfo
            , sometimesNumFields = 0
            , dosCodePage = 1252
            , unknown11 = 257
            , extraHeaderSize
            , changeCount4 = 1
            , unknown12 = replicate 6 0
            }

        pxDatabase = PxDatabase
            { tPxHeader = pxCommonHeader
            , tPxDataHeader = pxDataHeader
            , tableName = "Deleter.DB"
            , fieldInfo
            , rawBlocks
            }

    in  pxDatabase

writePxDel :: Table -> ByteString
writePxDel = runPut . putPxDatabase . createPxDel
