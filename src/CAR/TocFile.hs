{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module CAR.TocFile
    ( IndexedCborPath(..)
    , IndexedCbor
      -- * Building index
    , createIndex
      -- * Opening index
    , open
    , openInMem
      -- * Queries
    , lookup
    , toList
    , keys
    ) where

import Control.Exception (Exception, throw)
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Data.Foldable hiding (toList)
import qualified Data.Binary.Serialise.CBOR.Read as CBOR.Read
import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import System.IO.MMap
import CAR.Types
import System.FilePath
import Prelude hiding (lookup)

type Offset = Int

readValuesWithOffsets :: forall a. CBOR.Serialise a
                      => BSL.ByteString -> [(Offset, a)]
readValuesWithOffsets = \bs -> runST $ start 0 $ BSL.toChunks bs
  where
    start :: Offset  -- ^ current offset
          -> [BS.ByteString]
          -> ST s [(Offset, a)]
    start _offset []  = return []
    start offset  bss =
        go offset offset CBOR.deserialiseIncremental bss

    go :: Offset          -- ^ offset of beginning of current chunk
       -> Offset          -- ^ start offset of thing currently being decoded
       -> ST s (CBOR.IDecode s a)
       -> [BS.ByteString] -- ^ remaining chunks
       -> ST s [(Offset, a)]
    go !currOff !startOff decoder bss0 = do
        r <- decoder
        case (r, bss0) of
          (CBOR.Partial f,     [])     -> go currOff startOff (f Nothing) []
          (CBOR.Partial f,     bs:bss) -> go currOff startOff (f (Just bs)) bss
          (CBOR.Done bs off x, bss)    -> do
              let !currOff' = currOff + fromIntegral off
                  bss' | BS.null bs = bss
                       | otherwise  = bs : bss
              rest <- unsafeInterleaveST $ start currOff' bss'
              return $ (startOff, x) : rest
          (CBOR.Fail _rest _ err, _)   -> error $ show err

newtype IndexedCborPath i a = IndexedCborPath FilePath
                            deriving (Show)

buildIndex :: (Hashable i, Eq i, CBOR.Serialise a)
           => (a -> i) -> FilePath -> IO (HM.HashMap i Offset)
buildIndex toIndex path = do
    xs <- readValuesWithOffsets <$> BSL.readFile path
    let addElem acc (offset, x) = HM.insert (toIndex x) offset acc
    return $ foldl' addElem mempty xs

createIndex :: (Hashable i, Eq i, CBOR.Serialise i, CBOR.Serialise a)
           => (a -> i) -> FilePath -> IO (IndexedCborPath i a)
createIndex toIndex path = do
    toc <- buildIndex toIndex path
    BSL.writeFile tocPath $ CBOR.serialise toc
    return $ IndexedCborPath path
  where tocPath = path <.> "toc"

data IndexedCbor i a = IndexedCbor (HM.HashMap i Offset) BS.ByteString String

open :: (Hashable i, Eq i, CBOR.Serialise i)
     => IndexedCborPath i a -> IO (IndexedCbor i a)
open (IndexedCborPath fname) = do
    cbor <- mmapFileByteString fname Nothing
    toc <- either onError id . CBOR.Read.deserialiseFromBytes CBOR.decode
           <$> BSL.readFile tocName
    return $ IndexedCbor toc cbor fname
  where
    onError err =
        error $ "Deserialisation error while deserialising TOC "++show tocName++": "++show err
    tocName = fname <.> "toc"

-- | Build a TOC in-memory
openInMem :: (Hashable i, Eq i, CBOR.Serialise a)
          => (a -> i) -> FilePath -> IO (IndexedCbor i a)
openInMem toIndex fname = do
    cbor <- mmapFileByteString fname Nothing
    toc <- buildIndex toIndex fname
    return $ IndexedCbor toc cbor fname

lookup :: (Hashable i, Eq i, CBOR.Serialise a)
       => i -> IndexedCbor i a -> Maybe a
lookup i (IndexedCbor toc bs source) = deser <$> HM.lookup i toc
  where deser offset =
          case CBOR.Read.deserialiseFromBytes CBOR.decode $ BSL.fromStrict $ BS.drop offset bs of
            Left err -> throw $ DeserialiseFailure source err
            Right x -> x

data DeserialiseFailure = DeserialiseFailure String CBOR.DeserialiseFailure
                        deriving (Show)
instance Exception DeserialiseFailure

toList :: (CBOR.Serialise a) => IndexedCbor i a -> [a]
toList (IndexedCbor _ bs _) = decodeCborList $ BSL.fromStrict bs

keys :: IndexedCbor i a -> [i]
keys (IndexedCbor toc _ _) = HM.keys toc
