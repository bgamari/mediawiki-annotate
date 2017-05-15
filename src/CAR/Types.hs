{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module CAR.Types
    ( -- * Identifiers
      PageName(..)
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
      -- * Pretty printing
    , prettyPage, prettySkeleton
      -- ** Link styles
    , LinkStyle
    , withLink, anchorOnly
      -- * Miscellaneous Utilities
    , decodeCborList
    , encodeCborList
    ) where

import Data.Foldable
import Data.Char (ord, chr)
import Data.List (intercalate)
import Control.DeepSeq
import GHC.Generics
import Data.Monoid
import qualified Data.Binary.Serialise.CBOR.Class as CBOR
import qualified Data.Binary.Serialise.CBOR.Write as CBOR
import qualified Data.Binary.Serialise.CBOR.Read as CBOR
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI
import Crypto.Hash.SHA1 as SHA
import qualified Data.ByteString.Base16 as Base16
import Data.MediaWiki.Markup
import Data.Aeson.Types
import Data.Hashable
import Data.String

-- SimplIR
import qualified Data.SmallUtf8 as Utf8

unpackSBS :: SBS.ShortByteString -> String
unpackSBS = map (chr . fromIntegral) . SBS.unpack

urlEncodeText :: String -> SBS.ShortByteString
urlEncodeText = SBS.pack . map (fromIntegral . ord) . escapeURIString isAllowedInURI

-- Orphans
deriving instance CBOR.Serialise PageName
deriving instance FromJSON PageName
instance FromJSONKey PageName where
    fromJSONKey = fmap PageName fromJSONKey
deriving instance ToJSON PageName
instance ToJSONKey PageName where
    toJSONKey = contramapToJSONKeyFunction (\(PageName n) -> n) toJSONKey

instance CBOR.Serialise SBS.ShortByteString where
    encode = CBOR.encode . SBS.fromShort
    decode = SBS.toShort <$> CBOR.decode   -- FIXME: copy

-- | An ASCII-only form of a page name.
newtype PageId = PageId Utf8.SmallUtf8
               deriving (Show, Eq, Ord, Generic, IsString, NFData,
                         FromJSON, FromJSONKey, ToJSON, ToJSONKey)
instance Hashable PageId
instance CBOR.Serialise PageId where
    encode (PageId p) = CBOR.encode (Utf8.toByteString p)
    decode = PageId . Utf8.unsafeFromByteString <$> CBOR.decode

pageNameToId :: PageName -> PageId
pageNameToId (PageName n) = PageId $ Utf8.unsafeFromShortByteString $ urlEncodeText $ T.unpack n

packPageId :: String -> PageId
packPageId = PageId . Utf8.fromString

unpackPageId :: PageId -> String
unpackPageId (PageId s) = Utf8.toString s

-- | An ASCII-only form of a section heading.
newtype HeadingId = HeadingId SBS.ShortByteString
                  deriving (Show, Eq, Ord, Generic, Hashable, CBOR.Serialise)

sectionHeadingToId :: SectionHeading -> HeadingId
sectionHeadingToId (SectionHeading h) = HeadingId $ urlEncodeText $ T.unpack h

unpackHeadingId :: HeadingId -> String
unpackHeadingId (HeadingId s) = unpackSBS s

-- | The text of a section heading.
newtype SectionHeading = SectionHeading { getSectionHeading :: T.Text }
                       deriving (Show, Eq, Ord, Generic, Hashable, CBOR.Serialise)

data Paragraph = Paragraph { paraId :: !ParagraphId, paraBody :: [ParaBody] }
               deriving (Show, Generic)
instance CBOR.Serialise Paragraph

newtype ParagraphId = ParagraphId SBS.ShortByteString -- Hash
                    deriving (Show, Generic, Ord, Eq, CBOR.Serialise, Hashable)
instance NFData ParagraphId

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

data ParaBody = ParaText !T.Text
              | ParaLink !Link
              deriving (Show, Generic)
instance CBOR.Serialise ParaBody

data Page = Page { pageName     :: !PageName
                 , pageId       :: !PageId
                 , pageSkeleton :: [PageSkeleton]
                 }
          deriving (Show, Generic)
instance CBOR.Serialise Page

data SectionPath = SectionPath { sectionPathPageId :: PageId
                               , sectionPathHeadings :: [HeadingId]
                               }
               deriving (Show, Eq, Ord)

escapeSectionPath :: SectionPath -> String
escapeSectionPath (SectionPath page headings) =
    intercalate "/" $ (unpackPageId page) : map unpackHeadingId headings

decodeCborList :: CBOR.Serialise a => BSL.ByteString -> [a]
decodeCborList = start . BSL.toChunks
  where
    start xs
      | all BS.null xs = []
      | otherwise      = go (CBOR.deserialiseIncremental CBOR.decode) xs

    go (CBOR.Partial f) []         = go (f Nothing) []
    go (CBOR.Partial f) (bs : bss) = go (f (Just bs)) bss
    go (CBOR.Done bs _ x) bss      = x : start (bs : bss)
    go (CBOR.Fail _rest _ err) _   = error $ show err

encodeCborList :: CBOR.Serialise a => [a] -> BSB.Builder
encodeCborList = CBOR.toBuilder . foldMap CBOR.encode

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
