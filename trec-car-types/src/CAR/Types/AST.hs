{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module CAR.Types.AST
    ( -- * Identifiers
      PageName(..), packPageName, unpackPageName
    , SiteId(..)
    , Link(..)
    , PageId(..), packPageId, unpackPageId
    , pageNameToId
    , SectionHeading(..)
    , HeadingId(..), unpackHeadingId, packHeadingId, sectionHeadingToId
    , ParagraphId(..), unpackParagraphId, packParagraphId
    , SectionPath(..), escapeSectionPath, parseSectionPath
      -- * Documents
    , Paragraph(..)
    , ParaBody(..), paraBodiesToId
    , PageSkeleton(..)
    , Page(..)
    , PageType(..)
      -- ** Metadata
    , PageMetadata(..)
    , MetadataField
    , emptyPageMetadata
    , getMetadata
    , setMetadata
    , clearMetadata
      -- *** Fields
    , _RedirectNames
    , _DisambiguationNames
    , _DisambiguationIds
    , _CategoryNames
    , _CategoryIds
    , _InlinkIds
    , _InlinkAnchors
    , _UnknownMetadata
      -- * Outline documents
    , Stub(..)
      -- * Entity
    , Entity(..)
    , pageIdToName
    ) where

import Data.Foldable
import Data.Char (ord, chr)
import Data.List (intercalate)
import Data.Maybe
import Data.Semigroup
import Data.Functor.Contravariant
import Control.DeepSeq
import Control.Monad (when, replicateM)
import GHC.Generics
import qualified Codec.Serialise.Class as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import qualified Codec.Serialise.Encoding as CBOR
import qualified Codec.CBOR.Term as CBOR
import qualified Data.ByteString.Short as SBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Binary
import Network.URI
import Crypto.Hash.SHA1 as SHA
import qualified Data.ByteString.Base16 as Base16
import Data.Aeson.Types
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import Data.Hashable
import Data.String
import qualified Control.Lens as L
import Data.Text.Short (ShortText)
import qualified Data.Text.Short as Short

import Data.MediaWiki.Markup
import CAR.Types.Orphans ()

unpackSBS :: SBS.ShortByteString -> String
unpackSBS = map (chr . fromIntegral) . SBS.unpack

packSBS :: String -> SBS.ShortByteString
packSBS = SBS.pack . map (fromIntegral . ord)

urlEncodeText :: String -> SBS.ShortByteString
urlEncodeText = SBS.pack . map (fromIntegral . ord) . escapeURIString isAllowedInURI

unpackPageName :: PageName -> String
unpackPageName (PageName t) = T.unpack t

-- | A Wikipedia site ID. e.g. @enwiki@, @itwiki@, or @envoyage@.
newtype SiteId = SiteId T.Text
               deriving (Show, Eq, Ord, Hashable, IsString,
                         FromJSON, ToJSON, FromJSONKey, CBOR.Serialise)

-- | An ASCII-only form of a page name.
newtype PageId = PageId {unPageId :: ShortText}
               deriving (Show, Eq, Ord, Generic, IsString, NFData)
instance Hashable PageId
instance Binary PageId where
    get = PageId . Short.fromText <$> get
    put = put . Short.toText . unPageId
instance FromJSON PageId where
    parseJSON o = PageId . Short.fromText <$> parseJSON o
instance FromJSONKey PageId where
    fromJSONKey = fmap (PageId . Short.fromText) fromJSONKey
instance ToJSON PageId where
    toJSON = toJSON . Short.toText . unPageId
    toEncoding = toEncoding . Short.toText . unPageId
instance ToJSONKey PageId where
    toJSONKey = contramap (Short.toText . unPageId) toJSONKey
instance CBOR.Serialise PageId where
    encode (PageId p) = CBOR.encode (Short.toByteString p)
    decode = maybe err (pure . PageId) . Short.fromByteString =<< CBOR.decode
      where err = fail "Serialise.decode(PageId): invalid UTF-8 encoding"

{-# Deprecated pageNameToId "Use nameToId index" #-}
pageNameToId :: SiteId -> PageName -> PageId
pageNameToId (SiteId s) (PageName n) =
    maybe err PageId
    $ Short.fromShortByteString
    $ urlEncodeText
    $ T.unpack s ++ ":" ++ T.unpack n
  where err = error "pageNameToId: invalid UTF-8 encoding"

pageIdToName :: PageId -> PageName
pageIdToName (PageId pid) =
    PageName
    $ T.tail $ T.dropWhile (/=':')
    $ T.pack $ unEscapeString $ Short.toString pid

packPageName :: String -> PageName
packPageName = PageName . T.pack

packPageId :: String -> PageId
packPageId = PageId . Short.fromString

unpackPageId :: PageId -> String
unpackPageId (PageId s) = Short.toString s

-- | An ASCII-only form of a section heading.
newtype HeadingId = HeadingId SBS.ShortByteString
                  deriving (Show, Eq, Ord, Generic, Hashable, CBOR.Serialise, NFData)

instance ToJSON HeadingId where
    toJSON = toJSON . unpackHeadingId
instance FromJSON HeadingId where
    parseJSON o = packHeadingId <$> parseJSON o

sectionHeadingToId :: SectionHeading -> HeadingId
sectionHeadingToId (SectionHeading h) = HeadingId $ urlEncodeText $ T.unpack h

unpackHeadingId :: HeadingId -> String
unpackHeadingId (HeadingId s) = unpackSBS s

packHeadingId :: String -> HeadingId
packHeadingId = HeadingId . packSBS

-- | The text of a section heading.
newtype SectionHeading = SectionHeading { getSectionHeading :: T.Text }
                       deriving (Show, Eq, Ord, Generic, Hashable, CBOR.Serialise, NFData, FromJSON, ToJSON)

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
packParagraphId = ParagraphId . packSBS

data PageSkeleton = Section !SectionHeading !HeadingId [PageSkeleton]
                  | Para !Paragraph
                  | Image T.Text [PageSkeleton]
                  | List !Int Paragraph
                  | Infobox T.Text [(T.Text, [PageSkeleton])]
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
                 , pageType     :: PageType
                 , pageMetadata :: PageMetadata
                 , pageSkeleton :: [PageSkeleton]
                 }
          deriving (Show, Generic)

instance CBOR.Serialise Page where
    decode = do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeInt
        when (tag /= 0) $ fail "Serialise(Page): Tag indicates this is not a page."
        case len of
          6 -> do
              pageName <- CBOR.decode
              pageId <- CBOR.decode
              pageSkeleton <- CBOR.decode
              pageType <- CBOR.decode
              pageMetadata <- CBOR.decode
              return Page{..}
          4 -> do
              pageName <- CBOR.decode
              pageId <- CBOR.decode
              pageSkeleton <- CBOR.decode
              let pageType = ArticlePage
                  pageMetadata = emptyPageMetadata
              return Page{..}
          _ -> fail "Serialise(Page): Unknown length"
    encode (Page{..}) =
           CBOR.encodeListLen 6
        <> CBOR.encodeInt 0
        <> CBOR.encode pageName
        <> CBOR.encode pageId
        <> CBOR.encode pageSkeleton
        <> CBOR.encode pageType
        <> CBOR.encode pageMetadata

data PageType = ArticlePage
              | CategoryPage
              | DisambiguationPage
              | RedirectPage Link
              deriving (Show, Generic)

instance CBOR.Serialise PageType where
    decode = do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeInt
        let variant t = do
              when (len /= 1) $ fail $ "Unknown "++show t++" encoding"
              pure t
        case tag of
          0 -> variant ArticlePage
          1 -> variant CategoryPage
          2 -> variant DisambiguationPage
          3 -> do
              when (len /= 2) $ fail $ "Unknown RedirectPage encoding"
              RedirectPage <$> CBOR.decode
          _ -> fail "Unknown PageType"

    encode x =
        case x of
          ArticlePage        -> simple 0
          CategoryPage       -> simple 1
          DisambiguationPage -> simple 2
          RedirectPage l     ->
                 CBOR.encodeListLen 2
              <> CBOR.encodeInt 3
              <> CBOR.encode l
      where
        simple n = CBOR.encodeListLen 1 <> CBOR.encodeInt n

newtype PageMetadata = PageMetadata [MetadataItem]
                     deriving (CBOR.Serialise)

instance Show PageMetadata where
  show (PageMetadata list) =
      unlines $ fmap show' list
    where show' x = "- " ++ show x



emptyPageMetadata :: PageMetadata
emptyPageMetadata = PageMetadata []

data MetadataItem = RedirectNames [PageName]
                  | DisambiguationNames [PageName]
                  | DisambiguationIds [PageId]
                  | CategoryNames [PageName]
                  | CategoryIds [PageId]
                  | InlinkIds [PageId]
                  | OldInlinkAnchors [T.Text] -- ^ deprecated
                  | InlinkAnchors (V.Vector (T.Text, Int))
                  | UnknownMetadata !Int !Int [CBOR.Term]
                  deriving (Show, Generic)

instance CBOR.Serialise MetadataItem where
    decode = do
        len <- CBOR.decodeListLen
        tag <- CBOR.decodeInt
        case tag of
          0 -> RedirectNames <$> CBOR.decode
          1 -> DisambiguationNames <$> CBOR.decode
          2 -> DisambiguationIds <$> CBOR.decode
          3 -> CategoryNames <$> CBOR.decode
          4 -> CategoryIds <$> CBOR.decode
          5 -> InlinkIds <$> CBOR.decode
          6 -> OldInlinkAnchors <$> CBOR.decode
          7 -> InlinkAnchors <$> CBOR.decode
          _ -> UnknownMetadata len tag <$> replicateM (len-1) CBOR.decodeTerm

    encode val =
        case val of
          RedirectNames xs -> simple 0 xs
          DisambiguationNames xs -> simple 1 xs
          DisambiguationIds xs -> simple 2 xs
          CategoryNames xs -> simple 3 xs
          CategoryIds xs -> simple 4 xs
          InlinkIds xs -> simple 5 xs
          OldInlinkAnchors xs -> simple 6 xs
          InlinkAnchors xs -> simple 7 xs
          UnknownMetadata len tag y ->
                 CBOR.encodeListLen (fromIntegral len)
              <> CBOR.encodeInt tag
              <> foldMap CBOR.encodeTerm y
      where
        simple :: CBOR.Serialise a => Int -> a -> CBOR.Encoding
        simple tag x =
            CBOR.encodeListLen 1 <> CBOR.encodeInt tag <> CBOR.encode x

$(L.makePrisms ''MetadataItem)

type MetadataField a = L.Prism' MetadataItem a

getMetadata :: MetadataField a -> PageMetadata -> Maybe a
getMetadata p (PageMetadata metas) =
    metas L.^? L.each . p

clearMetadata :: MetadataField a -> PageMetadata -> PageMetadata
clearMetadata p (PageMetadata metas) =
    PageMetadata $ filter (L.isn't p) metas

setMetadata :: MetadataField a -> a -> PageMetadata -> PageMetadata
setMetadata p x (PageMetadata metas) =
    PageMetadata $ meta : filter (L.isn't p) metas
  where meta = x L.^. L.re p

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

parseSectionPath :: T.Text -> Maybe SectionPath
parseSectionPath s
  | pid:hs <- T.split (== '/') s =   -- Todo this is wrong, as entity ids can contain slashes
        Just $ SectionPath (packPageId $ T.unpack pid) (map (packHeadingId . T.unpack) hs)
  | otherwise = Nothing

-- | 'Stub' is like 'Page', but with the guarantee that there are no paragraphs
-- in the page skeleton
data Stub = Stub { stubName     :: PageName
                 , stubPageId   :: PageId
                 , stubType     :: PageType
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
          6 -> do
              stubName <- CBOR.decode
              stubPageId <- CBOR.decode
              stubSkeleton <- CBOR.decode
              stubType <- CBOR.decode
              stubMetadata <- CBOR.decode
              return Stub{..}
          4 -> do
              stubName <- CBOR.decode
              stubPageId <- CBOR.decode
              stubSkeleton <- CBOR.decode
              let stubType = ArticlePage
                  stubMetadata = emptyPageMetadata
              return Stub{..}
          _ -> fail "Serialise(Stub): Unknown length"
    encode (Stub{..}) =
           CBOR.encodeListLen 6
        <> CBOR.encodeInt 1
        <> CBOR.encode stubName
        <> CBOR.encode stubPageId
        <> CBOR.encode stubSkeleton
        <> CBOR.encode stubType
        <> CBOR.encode stubMetadata
