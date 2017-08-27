{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module CAR.Types.CborList
    ( decodeCborList, readCborList, readCborList'
    , encodeCborList, writeCborList
      -- * Exceptions
    , CborListHeaderDeserialiseError(..)
    , CborListDeserialiseError(..)
      -- * Utilities
    , breakOrElement
    ) where

import Control.Exception
import Data.Semigroup
import qualified Codec.Serialise.Class as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Codec.CBOR.Read as Read
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import System.IO

decodeCborList :: forall hdr a. (CBOR.Serialise hdr, CBOR.Serialise a)
               => BSL.ByteString -> (hdr, [a])
decodeCborList = decodeCborList' "<decodeCborList>"

decodeCborList' :: forall hdr a. (CBOR.Serialise hdr, CBOR.Serialise a)
                => String  -- ^ an error description
                -> BSL.ByteString
                -> (hdr, [a])
decodeCborList' desc = \bs ->
    case Read.deserialiseFromBytes (CBOR.decode <* CBOR.decodeListLenIndef) bs of
      Right (rest, hdr) -> (hdr, go rest)
      Left err          -> throw $ CborListHeaderDeserialiseError desc err
  where
    go xs
      | BSL.null xs = []
      | otherwise   =
            case Read.deserialiseFromBytes breakOrElement xs of
              Left err             -> throw $ CborListDeserialiseError desc err
              Right (_, Nothing)   -> []
              Right (rest, Just x) -> x : go rest

breakOrElement :: CBOR.Serialise a => CBOR.Decoder s (Maybe a)
breakOrElement = do
    ty <- CBOR.peekTokenType
    case ty of
      CBOR.TypeBreak -> return Nothing
      _              -> Just <$> CBOR.decode

data CborListHeaderDeserialiseError = CborListHeaderDeserialiseError String Read.DeserialiseFailure
                              deriving (Show)
instance Exception CborListHeaderDeserialiseError

data CborListDeserialiseError = CborListDeserialiseError String Read.DeserialiseFailure
                              deriving (Show)
instance Exception CborListDeserialiseError

readCborList :: forall a. (CBOR.Serialise a)
             => FilePath -> IO [a]
readCborList fname = snd <$> readCborList' @CBOR.Term @a fname

readCborList' :: forall hdr a. (CBOR.Serialise hdr, CBOR.Serialise a)
              => FilePath -> IO (hdr, [a])
readCborList' fname = decodeCborList' fname <$> BSL.readFile fname

encodeCborList :: (CBOR.Serialise hdr, CBOR.Serialise a) => hdr -> [a] -> BSB.Builder
encodeCborList hdr xs = CBOR.toBuilder $
    CBOR.encode hdr
    <> CBOR.encodeListLenIndef
    <> foldMap CBOR.encode xs
    <> CBOR.encodeBreak

writeCborList :: (CBOR.Serialise hdr, CBOR.Serialise a) => FilePath -> hdr -> [a] -> IO ()
writeCborList out hdr xs =
    withFile out WriteMode $ \h -> do
    BSB.hPutBuilder h $ encodeCborList hdr xs
