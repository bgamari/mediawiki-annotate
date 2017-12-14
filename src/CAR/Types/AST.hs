{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module CAR.Types.AST
    ( -- * Identifiers
      PageName(..), unpackPageName
    , SiteId(..)
    , Link(..)
    , PageId(..), packPageId, unpackPageId, pageNameToId
    , SectionHeading(..)
    , HeadingId(..), unpackHeadingId, sectionHeadingToId
    , ParagraphId(..), unpackParagraphId, packParagraphId
    , SectionPath(..), escapeSectionPath
      -- * Documents
    , Paragraph(..)
    , ParaBody(..), paraBodiesToId
    , PageSkeleton(..)
    , Page(..)
    , PageType(..)
    , PageMetadata(..)
    , emptyPageMetadata
      -- * Outline documents
    , Stub(..)
      -- * Entity
    , Entity(..), pageIdToName
    ) where

import Data.Foldable
import Data.Char (ord, chr)
import Data.List (intercalate)
import Data.Semigroup
import Control.DeepSeq
import Control.Monad (when)
import GHC.Generics
import qualified Codec.Serialise.Class as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Binary
import Network.URI
import Crypto.Hash.SHA1 as SHA
import qualified Data.ByteString.Base16 as Base16
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import Data.Hashable
import Data.String

import Data.MediaWiki.Markup
import CAR.Types.Orphans ()

-- SimplIR
import qualified Data.SmallUtf8 as Utf8

unpackSBS :: SBS.ShortByteString -> String
unpackSBS = map (chr . fromIntegral) . SBS.unpack

urlEncodeText :: String -> SBS.ShortByteString
urlEncodeText = SBS.pack . map (fromIntegral . ord) . escapeURIString isAllowedInURI

unpackPageName :: PageName -> String
unpackPageName (PageName t) = T.unpack t

-- | A Wikipedia site ID. e.g. @enwiki@, @itwiki@, or @envoyage@.
newtype SiteId = SiteId T.Text
               deriving (Show, Eq, Ord, Hashable, IsString,
                         FromJSON, ToJSON, FromJSONKey, CBOR.Serialise)

-- | An ASCII-only form of a page name.
newtype PageId = PageId {unPageId :: Utf8.SmallUtf8}
               deriving (Show, Eq, Ord, Generic, IsString, NFData,
                         FromJSON, FromJSONKey, ToJSON, ToJSONKey, Binary)
instance Hashable PageId
instance CBOR.Serialise PageId where
    encode (PageId p) = CBOR.encode (Utf8.toByteString p)
    decode = PageId . Utf8.unsafeFromByteString <$> CBOR.decode

pageNameToId :: SiteId -> PageName -> PageId
pageNameToId (SiteId s) (PageName n) =
    PageId
    $ Utf8.unsafeFromShortByteString
    $ urlEncodeText
    $ T.unpack s ++ ":" ++ T.unpack n

pageIdToName :: PageId -> PageName
pageIdToName (PageId pid) =
    PageName
    $ T.tail $ T.dropWhile (/=':')
    $ T.pack $ unEscapeString $ Utf8.toString pid

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
                  | List !Int Paragraph
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

paraBodiesToId :: [ParaBody] -> ParagraphId
paraBodiesToId =
    ParagraphId . SBS.toShort . Base16.encode . SHA.finalize . foldl' SHA.update SHA.init . map toBS
  where
    toBS (ParaText t)  = TE.encodeUtf8 t
    toBS (ParaLink l)  = TE.encodeUtf8 $ linkAnchor l

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
                 , pageMetadata :: !PageMetadata
                 , pageSkeleton :: [PageSkeleton]
                 }
          deriving (Show, Generic)

instance CBOR.Serialise Page where
    decode = do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeInt
        when (tag /= 0) $ fail "Serialise(Page): Tag indicates this is not a page."
        case len of
          5 -> do
              pageName <- CBOR.decode
              pageId <- CBOR.decode
              pageSkeleton <- CBOR.decode
              pageMetadata <- CBOR.decode
              return Page{..}
          4 -> do
              pageName <- CBOR.decode
              pageId <- CBOR.decode
              pageSkeleton <- CBOR.decode
              let pageMetadata = emptyPageMetadata
              return Page{..}
          _ -> fail "Serialise(Page): Unknown length"
    encode (Page{..}) =
           CBOR.encodeListLen 5
        <> CBOR.encodeInt 0
        <> CBOR.encode pageName
        <> CBOR.encode pageId
        <> CBOR.encode pageSkeleton
        <> CBOR.encode pageMetadata

data PageType = ArticlePage
              | CategoryPage
              | DisambiguationPage
              | RedirectPage PageId
              deriving (Show, Generic, Eq, Ord)
instance CBOR.Serialise PageType

-- data MetadataItem = RedirectNames [PageName]
--                   | RedirectIds [PageId]
--                   | DisambiguationNames [PageName]
--                   | DisambiguationIds [PageId]
--                   | CategoryNames [PageName]
--                   | CategoryIds [PageId]
--                   | InlinkPageNames [PageName]
--                   | InlinkPageIds [PageId]
--                   deriving (Show, Generic)
-- instance CBOR.Serialise MetadataItem

data PageMetadata = PageMetadata
    { pagemetaType                 :: PageType
      -- ^ what kind of page is this?
    , pagemetaRedirectNames        :: Maybe [PageName]
      -- ^ the names of pages that redirect here
    , pagemetaDisambiguationNames  :: Maybe [PageName]
      -- ^ the names of disambiguation pages that link here
    , pagemetaDisambiguationIds    :: Maybe [PageId]
      -- ^ the 'PageId's of disambiguation pages that link here
    , pagemetaCategoryNames        :: Maybe [PageName]
      -- ^ the names of the categories to which the page belongs
    , pagemetaCategoryIds          :: Maybe [PageId]
      -- ^ the 'PageId's of the categories to which the page belongs
    , pagemetaInlinkIds            :: Maybe [PageId]
      -- ^ the 'PageId's of 'ArticlePage's and 'CategoryPage's that link here
    }
    deriving (Show, Generic)
instance CBOR.Serialise PageMetadata

emptyPageMetadata :: PageMetadata
emptyPageMetadata = PageMetadata ArticlePage Nothing Nothing Nothing Nothing Nothing Nothing

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

-- | 'Stub' is like 'Page', but with the guarantee that there are no paragraphs
-- in the page skeleton
data Stub = Stub { stubName     :: PageName
                 , stubPageId   :: PageId
                 , stubMetadata :: PageMetadata
                 , stubSkeleton :: [PageSkeleton]
                 }
          deriving (Show, Generic)

instance CBOR.Serialise Stub where
    decode = do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeInt
        when (tag /= 1 && tag /=0) $ fail "Serialise(Stub): Tag indicates this is neither a stub nor a page"
        case len of
          5 -> do
              stubName <- CBOR.decode
              stubPageId <- CBOR.decode
              stubSkeleton <- CBOR.decode
              stubMetadata <- CBOR.decode
              return Stub{..}
          4 -> do
              stubName <- CBOR.decode
              stubPageId <- CBOR.decode
              stubSkeleton <- CBOR.decode
              let stubMetadata = emptyPageMetadata
              return Stub{..}
          _ -> fail "Serialise(Stub): Unknown length"
    encode (Stub{..}) =
           CBOR.encodeListLen 5
        <> CBOR.encodeInt 1
        <> CBOR.encode stubName
        <> CBOR.encode stubPageId
        <> CBOR.encode stubSkeleton
        <> CBOR.encode stubMetadata
