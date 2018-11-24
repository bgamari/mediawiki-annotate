{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}

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
import Data.Foldable hiding (toList)
import Control.DeepSeq
import qualified Codec.CBOR.Read as CBOR.Read
import qualified Codec.CBOR.Decoding as CBOR (decodeListLenIndef)
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.Serialise as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import System.IO.MMap
import System.FilePath
import Prelude hiding (lookup)

import SimplIR.Utils.Compact
import CAR.Types.CborList

type Offset = Int

readValuesWithOffsets :: forall a. CBOR.Serialise a
                      => BSL.ByteString -> [(Offset, a)]
readValuesWithOffsets = go 0
  where
    go :: Offset  -- ^ current offset
       -> BSL.ByteString
       -> [(Offset, a)]
    go !offset !bs
      | BSL.null bs = []
      | otherwise   =
          case CBOR.Read.deserialiseFromBytesWithSize breakOrElement bs of
            Left err                  -> error $ show err
            Right (_   , _ , Nothing) -> []
            Right (rest, sz, Just x)  -> (offset, x) : go (offset + fromIntegral sz) rest

newtype IndexedCborPath i a = IndexedCborPath FilePath
                            deriving (Show)

buildIndex :: (Hashable i, Eq i, CBOR.Serialise a)
           => (a -> i) -> FilePath -> IO (HM.HashMap i Offset)
buildIndex toIndex cborPath = do
    -- Ignore header
    bs <- BSL.readFile cborPath
    let decodeHeader = do
            _ <- CBOR.decode @CBOR.Term
            _ <- CBOR.decodeListLenIndef
            return ()
        (rest, headerSize) =
            case CBOR.Read.deserialiseFromBytesWithSize decodeHeader bs of
              Right (bs', off, _) -> (bs', fromIntegral off)
              Left _              -> (bs, 0) -- header probably doesn't exist

    let xs = readValuesWithOffsets rest
    let addElem acc (offset, x) = HM.insert (toIndex x) (headerSize + offset) acc
    return $ foldl' addElem mempty xs

createIndex :: (Hashable i, Eq i, CBOR.Serialise i, CBOR.Serialise a)
           => (a -> i) -> FilePath -> IO (IndexedCborPath i a)
createIndex toIndex path = do
    toc <- buildIndex toIndex path
    BSL.writeFile tocPath $ CBOR.serialise toc
    return $ IndexedCborPath path
  where tocPath = path <.> "toc"

data IndexedCbor i a = IndexedCbor (HM.HashMap i Offset) BS.ByteString String

open :: (Hashable i, Eq i, CBOR.Serialise i, NFData i)
     => IndexedCborPath i a -> IO (IndexedCbor i a)
open (IndexedCborPath fname) = do
    cbor <- mmapFileByteString fname Nothing
    toc <- either onError snd . CBOR.Read.deserialiseFromBytes CBOR.decode
           <$> BSL.readFile tocName
    return $ IndexedCbor (inCompact toc) cbor fname
  where
    onError err =
        error $ "Deserialisation error while deserialising TOC "++show tocName++": "++show err
    tocName = fname <.> "toc"

-- | Build a TOC in-memory (does not read from file)
openInMem :: (Hashable i, Eq i, CBOR.Serialise a, NFData i)
          => (a -> i) -> FilePath -> IO (IndexedCbor i a)
openInMem toIndex fname = do
    cbor <- mmapFileByteString fname Nothing
    toc <- inCompactM $ buildIndex toIndex fname
    return $ IndexedCbor toc cbor fname

lookup :: (Hashable i, Eq i, CBOR.Serialise a)
       => i -> IndexedCbor i a -> Maybe a
lookup i (IndexedCbor toc bs source) = deser <$> HM.lookup i toc
  where deser offset =
          case CBOR.Read.deserialiseFromBytes CBOR.decode $ BSL.fromStrict $ BS.drop offset bs of
            Left err    -> throw $ DeserialiseFailure source err
            Right (_,x) -> x

data DeserialiseFailure = DeserialiseFailure String CBOR.DeserialiseFailure
                        deriving (Show)
instance Exception DeserialiseFailure

toList :: forall i a. (CBOR.Serialise a) => IndexedCbor i a -> [a]
toList (IndexedCbor _ bs _) =
    snd $ decodeCborList @CBOR.Term @a $ BSL.fromStrict bs

keys :: IndexedCbor i a -> [i]
keys (IndexedCbor toc _ _) = HM.keys toc
