{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}


-- Utilities for reading CAR cbor files with headers. Use this module rather than CborList
module CAR.Types.Files
    ( Provenance(..)
      -- * File types
    , File
    , writeCarFile
    , readCarFile
      -- ** Pages
    , readPagesFile, readPagesFileWithProvenance
      -- ** Paragraphs
    , readParagraphsFile
      -- ** Outlines
    , readOutlinesFile
      -- * Header type
    , FileType(..)
    , Header(..)
    ) where

import Data.Proxy
import GHC.Generics
import Control.Monad
import Control.Exception
import Data.Semigroup
import System.IO
import qualified Codec.Serialise          as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.Serialise.Decoding as CBOR

import CAR.Types.AST
import CAR.Types.CborList

data FileType = PagesFile
              | OutlinesFile
              | ParagraphsFile
              deriving (Show, Eq, Ord, Enum, Bounded, Generic)
instance CBOR.Serialise FileType

class File a where
    fileType :: Proxy a -> FileType

instance File Page where
    fileType _ = PagesFile

instance File Stub where
    fileType _ = OutlinesFile

instance File Paragraph where
    fileType _ = ParagraphsFile

data Header = Header { headerType :: FileType
                     , provenance :: Provenance
                     }
            deriving (Show, Generic)

instance CBOR.Serialise Header where
    encode hdr =
           CBOR.encodeListLen 3
        <> CBOR.encode magicWord
        <> CBOR.encode (headerType hdr)
        <> CBOR.encode (provenance hdr)
    decode = do
        CBOR.decodeListLenOf 3
        magic <- CBOR.decode
        when (magic /= magicWord) $
            fail $ "Invalid magic word: "<>magic
        Header <$> CBOR.decode <*> CBOR.decode

magicWord :: String
magicWord = "CAR"

data Provenance = Provenance { wikiDumpDate     :: String
                             , wikiSite         :: SiteId
                             , dataReleaseName  :: String
                             , comments         :: String
                             , toolsCommit      :: String
                             }
                deriving (Show, Generic)
instance CBOR.Serialise Provenance

invalidProvenance :: Provenance
invalidProvenance =
    Provenance { wikiDumpDate    = ""
               , wikiSite        = SiteId ""
               , dataReleaseName = ""
               , comments        = ""
               , toolsCommit     = ""
               }

data NoHeader = NoHeader

instance CBOR.Serialise NoHeader where
    decode = return NoHeader
    encode = mempty

checkFileType :: FileType -> FileType -> IO ()
checkFileType expected actual =
    when (actual /= expected) $
        fail $ concat [ "Expected file type "
                      , show expected
                      , ", saw file type "
                      , show actual
                      ]

readCarFile :: forall a.
               (File a, CBOR.Serialise a)
            => FilePath -> IO (Provenance, [a])
readCarFile path = handle tryWithoutHeader $ do
        (hdr, xs) <- readCborList' path
        checkFileType (fileType $ Proxy @a) (headerType hdr)
        return (provenance hdr, xs)
  where
    tryWithoutHeader (CborListHeaderDeserialiseError _ _) = do
        hPutStrLn stderr $ "Failed to deserialise header of "++path++"; provenance unavailable."
        (NoHeader, xs) <- readCborList' path
        return (invalidProvenance, xs)

writeCarFile :: forall a.
                (File a, CBOR.Serialise a)
             => FilePath -> Provenance -> [a] -> IO ()
writeCarFile path prov xs = do
    let hdr = Header { headerType = fileType $ Proxy @a
                     , provenance = prov
                     }
    writeCborList path hdr xs

readPagesFile :: FilePath -> IO [Page]
readPagesFile = fmap snd . readPagesFileWithProvenance

readPagesFileWithProvenance :: FilePath -> IO (Provenance, [Page])
readPagesFileWithProvenance = readCarFile

readParagraphsFile :: FilePath -> IO [Paragraph]
readParagraphsFile = fmap snd . readCarFile

readOutlinesFile :: FilePath -> IO [Stub]
readOutlinesFile = fmap snd . readCarFile
