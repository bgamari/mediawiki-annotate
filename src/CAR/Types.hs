{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module CAR.Types
    ( -- * Identifiers
      PageName(..), unpackPageName
    , Link(..)
    , PageId(..), packPageId, unpackPageId, pageNameToId
    , SectionHeading(..)
    , HeadingId(..), unpackHeadingId, sectionHeadingToId
    , ParagraphId(..), unpackParagraphId, packParagraphId
    , SectionPath(..), escapeSectionPath
      -- * Documents
    , Paragraph(..), prettyParagraph
    , ParaBody(..), paraBodiesToId
    , PageSkeleton(..)
    , Page(..)
      -- * Entity
    , Entity(..), pageIdToName
      -- * Pretty printing
    , prettyPage, prettySkeleton
      -- ** Link styles
    , LinkStyle
    , withLink, anchorOnly
      -- * Miscellaneous Utilities
    , decodeCborList, readCborList
    , encodeCborList, writeCborList
    ) where

import Data.Foldable
import Data.Char (ord, chr)
import Data.List (intercalate)
import Control.Exception
import Control.Monad.ST
import Control.Monad.ST.Unsafe
import Control.DeepSeq
import GHC.Generics
import Data.Monoid
import qualified Codec.Serialise.Class as CBOR
import qualified Codec.CBOR.Write as CBOR
import qualified Codec.CBOR.Read as CBOR
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Binary
import Network.URI
import Crypto.Hash.SHA1 as SHA
import qualified Data.ByteString.Base16 as Base16
import Data.MediaWiki.Markup
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import Data.Hashable
import Data.String
import System.IO

-- SimplIR
import qualified Data.SmallUtf8 as Utf8

unpackSBS :: SBS.ShortByteString -> String
unpackSBS = map (chr . fromIntegral) . SBS.unpack

urlEncodeText :: String -> SBS.ShortByteString
urlEncodeText = SBS.pack . map (fromIntegral . ord) . escapeURIString isAllowedInURI


-- Orphans
deriving instance CBOR.Serialise PageName
deriving instance FromJSON PageName
instance NFData PageName
instance FromJSONKey PageName where
    fromJSONKey = fmap PageName fromJSONKey
deriving instance ToJSON PageName
instance ToJSONKey PageName where
    toJSONKey = contramapToJSONKeyFunction (\(PageName n) -> n) toJSONKey

instance CBOR.Serialise SBS.ShortByteString where
    encode = CBOR.encode . SBS.fromShort
    decode = SBS.toShort <$> CBOR.decode   -- FIXME: copy

unpackPageName :: PageName -> String
unpackPageName (PageName t) = T.unpack t

-- | An ASCII-only form of a page name.
newtype PageId = PageId Utf8.SmallUtf8
               deriving (Show, Eq, Ord, Generic, IsString, NFData,
                         FromJSON, FromJSONKey, ToJSON, ToJSONKey, Binary)
instance Hashable PageId
instance CBOR.Serialise PageId where
    encode (PageId p) = CBOR.encode (Utf8.toByteString p)
    decode = PageId . Utf8.unsafeFromByteString <$> CBOR.decode

pageNameToId :: PageName -> PageId
pageNameToId (PageName n) = PageId $ Utf8.unsafeFromShortByteString $ urlEncodeText $ T.unpack n

pageIdToName :: PageId -> PageName
pageIdToName (PageId pid) = PageName $  T.pack  $ unEscapeString $  Utf8.toString pid

packPageId :: String -> PageId
packPageId = PageId . Utf8.fromString

unpackPageId :: PageId -> String
unpackPageId (PageId s) = Utf8.toString s

-- | An ASCII-only form of a section heading.
newtype HeadingId = HeadingId SBS.ShortByteString
                  deriving (Show, Eq, Ord, Generic, Hashable, CBOR.Serialise, NFData)

sectionHeadingToId :: SectionHeading -> HeadingId
sectionHeadingToId (SectionHeading h) = HeadingId $ urlEncodeText $ T.unpack h

unpackHeadingId :: HeadingId -> String
unpackHeadingId (HeadingId s) = unpackSBS s

-- | The text of a section heading.
newtype SectionHeading = SectionHeading { getSectionHeading :: T.Text }
                       deriving (Show, Eq, Ord, Generic, Hashable, CBOR.Serialise, NFData)

data Paragraph = Paragraph { paraId :: !ParagraphId, paraBody :: [ParaBody] }
               deriving (Show, Generic)
instance CBOR.Serialise Paragraph
instance NFData Paragraph

newtype ParagraphId = ParagraphId SBS.ShortByteString -- Hash
                    deriving (Show, Read, Generic, Ord, Eq, CBOR.Serialise, Hashable, Binary)
instance NFData ParagraphId
instance Aeson.FromJSON ParagraphId where parseJSON o = packParagraphId <$> parseJSON o
instance Aeson.ToJSON ParagraphId where toJSON = toJSON . unpackParagraphId

unpackParagraphId :: ParagraphId -> String
unpackParagraphId (ParagraphId s) = unpackSBS s

-- | Not generally safe.
packParagraphId :: String -> ParagraphId
packParagraphId = ParagraphId . SBS.pack . map (fromIntegral . ord)

data PageSkeleton = Section !SectionHeading !HeadingId [PageSkeleton]
                  | Para !Paragraph
                  | Image T.Text [PageSkeleton]
                  deriving (Show, Generic)
instance CBOR.Serialise PageSkeleton

data Link = Link { linkTarget   :: !PageName
                 , linkSection  :: !(Maybe T.Text)
                 , linkTargetId :: !PageId
                 , linkAnchor   :: !T.Text
                 }
          deriving (Show, Generic)
instance CBOR.Serialise Link
instance NFData Link where
    rnf Link{..} = rnf linkSection `seq` ()

data ParaBody = ParaText !T.Text
              | ParaLink !Link
              deriving (Show, Generic)
instance CBOR.Serialise ParaBody
instance NFData ParaBody

-- | A logical entity of a knowledge base
data Entity = Entity { entityPageName :: !PageName
                     , entityPageId   :: !PageId
                     }
            deriving (Show, Generic)
instance CBOR.Serialise Entity
instance NFData Entity
instance Aeson.FromJSON Entity
instance Aeson.ToJSON Entity


-- | A page on Wikipedia (which coincides with an Entity in this case)
data Page = Page { pageName     :: !PageName
                 , pageId       :: !PageId
                 , pageSkeleton :: [PageSkeleton]
                 }
          deriving (Show, Generic)
instance CBOR.Serialise Page

-- | Path from heading to page title in a page outline
data SectionPath = SectionPath { sectionPathPageId   :: !PageId
                               , sectionPathHeadings :: [HeadingId]
                               }
               deriving (Show, Eq, Ord, Generic)
instance Hashable SectionPath
instance NFData SectionPath

escapeSectionPath :: SectionPath -> String
escapeSectionPath (SectionPath page headings) =
    intercalate "/" $ (unpackPageId page) : map unpackHeadingId headings

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

prettyPage :: LinkStyle -> Page -> String
prettyPage linkStyle (Page (PageName name) _ skeleton) =
    unlines $ [ T.unpack name, replicate (T.length name) '=', "" ]
            ++ map (prettySkeleton linkStyle) skeleton

type LinkStyle = Link -> String

prettySkeleton :: LinkStyle -> PageSkeleton -> String
prettySkeleton renderLink = go 1
  where
    go :: Int -> PageSkeleton -> String
    go n (Section (SectionHeading name) _ children) =
        unlines
        $ [ replicate n '#' ++ " " ++ T.unpack name]
          <> map (go (n+1)) children
          <> [""]
    go _ (Para para) = prettyParagraph renderLink para
    go _ (Image target children) =
        "![" ++ unlines (map (go 1) children) ++ "](" ++ T.unpack target ++ ")"

prettyParagraph :: LinkStyle -> Paragraph -> String
prettyParagraph renderLink (Paragraph paraId bodies) =
    "{" ++ unpackParagraphId paraId ++ "} " ++ concatMap go bodies ++ "\n"
  where
    go (ParaText t) = T.unpack t
    go (ParaLink l) = renderLink l

withLink :: LinkStyle
withLink (Link (PageName name) section _ anchor) =
    "["<>T.unpack anchor<>"]("<>T.unpack name<>msection<>")"
  where
    msection
      | Just s <- section = "#"<>T.unpack s
      | otherwise         = mempty

anchorOnly :: LinkStyle
anchorOnly l = T.unpack $ linkAnchor l

paraBodiesToId :: [ParaBody] -> ParagraphId
paraBodiesToId =
    ParagraphId . SBS.toShort . Base16.encode . SHA.finalize . foldl' SHA.update SHA.init . map toBS
  where
    toBS (ParaText t)  = TE.encodeUtf8 t
    toBS (ParaLink l)  = TE.encodeUtf8 $ linkAnchor l
