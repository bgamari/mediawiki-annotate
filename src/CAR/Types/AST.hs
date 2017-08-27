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
      -- * Outline documents
    , Stub(..)
      -- * Entity
    , Entity(..), pageIdToName
    ) where

import Data.Foldable
import Data.Char (ord, chr)
import Data.List (intercalate)
import Control.DeepSeq
import GHC.Generics
import qualified Codec.Serialise.Class as CBOR
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
               deriving (Show, Eq, Ord, Hashable, FromJSON, ToJSON, FromJSONKey, CBOR.Serialise)

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

-- | 'Stub' is like 'Page', but with the guarantee that there are no paragraphs
-- in the page skeleton
data Stub = Stub { stubName     :: PageName
                 , stubPageId   :: PageId
                 , stubSkeleton :: [PageSkeleton] }
          deriving (Show, Generic)
instance CBOR.Serialise Stub
