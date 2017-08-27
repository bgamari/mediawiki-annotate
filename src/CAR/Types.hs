{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CAR.Types
    ( -- * AST
      module CAR.Types.AST
      -- * Pretty printing
    , module CAR.Types.AST.Pretty
      -- * Miscellaneous Utilities
    , decodeCborList, readCborList
    , encodeCborList, writeCborList
    ) where

import Control.Exception
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import qualified Codec.Serialise.Class as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import System.IO

import Data.MediaWiki.Markup
import CAR.Types.AST
import CAR.Types.AST.Pretty

decodeCborList :: forall a. CBOR.Serialise a => BSL.ByteString -> [a]
decodeCborList = decodeCborList' (CborListDeserialiseError "<decodeCborList>")

decodeCborList' :: forall a. CBOR.Serialise a
                => (CBOR.DeserialiseFailure -> CborListDeserialiseError)
                -> BSL.ByteString -> [a]
decodeCborList' deserialiseFail = \bs -> runST $ start $ BSL.toChunks bs
  where
    start xs
      | all BS.null xs = return []
      | otherwise      = go (CBOR.deserialiseIncremental CBOR.decode) xs

    go :: ST s (CBOR.IDecode s a) -> [BS.ByteString] -> ST s [a]
    go action xs = do
        r <- action
        case (r, xs) of
          (CBOR.Partial f, [])       -> go (f Nothing) []
          (CBOR.Partial f, bs : bss) -> go (f (Just bs)) bss
          (CBOR.Done bs _ x, bss)    -> do
              rest <- unsafeInterleaveST $ start (bs : bss)
              return (x : rest)
          (CBOR.Fail _rest _ err, _) -> throw $ deserialiseFail err

data CborListDeserialiseError = CborListDeserialiseError FilePath CBOR.DeserialiseFailure
                              deriving (Show)
instance Exception CborListDeserialiseError

readCborList :: CBOR.Serialise a => FilePath -> IO [a]
readCborList fname =
    decodeCborList' (CborListDeserialiseError fname) <$> BSL.readFile fname

encodeCborList :: CBOR.Serialise a => [a] -> BSB.Builder
encodeCborList = CBOR.toBuilder . foldMap CBOR.encode

writeCborList :: CBOR.Serialise a => FilePath -> [a] -> IO ()
writeCborList out xs =
    withFile out WriteMode $ \h -> do
    BSB.hPutBuilder h $ encodeCborList xs
