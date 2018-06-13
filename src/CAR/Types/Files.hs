{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities for reading CAR CBOR files with headers. Use this module rather than "CborList".
module CAR.Types.Files
    ( -- * Provenance
      Provenance(..)
    , Language(..)
    , SiteProvenance(..)
    , Transform(..)
    , transform
    , wikiSite
      -- * File types
    , File
    , writeCarFile
    , readCarFile
      -- ** Pages
    , readPagesFile, readPagesFileWithProvenance
      -- ** Paragraphs
    , readParagraphsFile, readParagraphsFileWithProvenance
      -- ** Outlines
    , readOutlinesFile
    , readPagesOrOutlinesAsPages, readPagesOrOutlinesAsPagesWithProvenance
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
import CAR.Types.Provenance



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




data NoHeader = NoHeader

instance CBOR.Serialise NoHeader where
    decode = return NoHeader
    encode = mempty

checkFileType :: FilePath-> FileType -> FileType -> IO ()
checkFileType fileName expected actual =
    when (actual /= expected) $
        fail $ "File " <> (show fileName) <> " is of file type " <> (show actual) <>
               ", but is expected to be of file type " <> (show expected)

readCarFile :: forall a.
               (File a, CBOR.Serialise a)
            => FilePath -> IO (Provenance, [a])
readCarFile path = handle tryWithoutHeader $ do
        (hdr, xs) <- readCborList path
        checkFileType path (fileType $ Proxy @a) (headerType hdr)
        return (provenance hdr, xs)
  where
    tryWithoutHeader (CborListHeaderDeserialiseError _ _) = do
        hPutStrLn stderr $ "Warning: Failed to deserialise header of "++path++"; provenance unavailable."
        xs <- readRawCborList path
        return (invalidProvenance, xs)

readCarPagesOrOutline :: FilePath -> IO (Either [Page] [Stub])
readCarPagesOrOutline path =
    snd <$> readCarPagesOrOutlineWithProvenance path

readCarPagesOrOutlineWithProvenance :: FilePath -> IO (Provenance, Either [Page] [Stub])
readCarPagesOrOutlineWithProvenance path = do
    hdr <- readCarHeader path
    res <- case fmap headerType hdr of
        Nothing -> Left <$> readPagesFile path
        Just PagesFile -> Left <$> readPagesFile path
        Just OutlinesFile -> Right <$> readOutlinesFile path
        Just other -> fail $ "File "<> (show path) <> " of type " <> (show other) <> ", but expected PagesFile or OutlinesFile here."
    return (maybe invalidProvenance provenance hdr, res)

readCarHeader :: FilePath -> IO (Maybe Header)
readCarHeader path = handle tryWithoutHeader (Just . fst <$> readCborList @Header @() path)
  where
    tryWithoutHeader (CborListHeaderDeserialiseError _ _) = do
        hPutStrLn stderr $ "Warning: Failed to deserialise header of "++path++"; provenance unavailable."
        return Nothing

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

readParagraphsFileWithProvenance :: FilePath -> IO (Provenance, [Paragraph])
readParagraphsFileWithProvenance = readCarFile


readOutlinesFile :: FilePath -> IO [Stub]
readOutlinesFile = fmap snd . readCarFile

readOutlinesFileWithProvenance :: FilePath -> IO (Provenance, [Stub])
readOutlinesFileWithProvenance = readCarFile

readPagesOrOutlinesAsPagesWithProvenance :: FilePath -> IO (Provenance, [Page])
readPagesOrOutlinesAsPagesWithProvenance path = do
    (prov, result) <- readCarPagesOrOutlineWithProvenance path
    case result of
        Left pages -> return (prov, pages)
        Right stubs -> return (prov, fmap stubToPage stubs)
  where stubToPage Stub{..} =
          Page { pageName = stubName
               , pageId = stubPageId
               , pageType = stubType
               , pageMetadata = stubMetadata
               , pageSkeleton = stubSkeleton
               }

readPagesOrOutlinesAsPages :: FilePath ->  IO [Page]
readPagesOrOutlinesAsPages path = do
    ( _ , pages ) <- readPagesOrOutlinesAsPagesWithProvenance path
    return pages