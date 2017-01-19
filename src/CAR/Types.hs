{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CAR.Types
    ( -- * Identifiers
      PageName(..)
    , LinkTarget(..)
    , PageId(..), unpackPageId, pageNameToId
    , SectionHeading(..)
    , HeadingId(..), unpackHeadingId, sectionHeadingToId
    , ParagraphId(..), unpackParagraphId
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
import GHC.Generics
import Data.Monoid
import qualified Data.Binary.Get as Bin
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

unpackSBS :: SBS.ShortByteString -> String
unpackSBS = map (chr . fromIntegral) . SBS.unpack

urlEncodeText :: String -> SBS.ShortByteString
urlEncodeText = SBS.pack . map (fromIntegral . ord) . escapeURIString isAllowedInURI

-- Orphans
instance CBOR.Serialise LinkTarget
deriving instance CBOR.Serialise PageName
instance FromJSON PageName
instance FromJSONKey PageName where
    fromJSONKey = fmap PageName fromJSONKey
instance ToJSON PageName
instance ToJSONKey PageName where
    toJSONKey = contramapToJSONKeyFunction (\(PageName n) -> n) toJSONKey

instance CBOR.Serialise SBS.ShortByteString where
    encode = CBOR.encode . SBS.fromShort
    decode = SBS.toShort <$> CBOR.decode

-- | An ASCII-only form of a page name.
newtype PageId = PageId SBS.ShortByteString
               deriving (Show, Eq, Ord, Generic, CBOR.Serialise, IsString)
instance Hashable PageId

pageNameToId :: PageName -> PageId
pageNameToId (PageName n) = PageId $ urlEncodeText $ T.unpack n

unpackPageId :: PageId -> String
unpackPageId (PageId s) = unpackSBS s

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

unpackParagraphId :: ParagraphId -> String
unpackParagraphId (ParagraphId s) = unpackSBS s

data PageSkeleton = Section !SectionHeading !HeadingId [PageSkeleton]
                  | Para !Paragraph
                  deriving (Show, Generic)
instance CBOR.Serialise PageSkeleton

data ParaBody = ParaText !T.Text
              | ParaLink { paraLinkTarget   :: !PageName
                         , paraLinkSection  :: !(Maybe T.Text)
                         , paraLinkTargetId :: !PageId
                         , paraLinkAnchor   :: !T.Text
                         }
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

    go (Bin.Partial f) [] = go (f Nothing) []
    go (Bin.Partial f) (bs : bss) = go (f (Just bs)) bss
    go (Bin.Done bs _ x) bss = x : start (bs : bss)
    go (Bin.Fail rest _ err) _ = error $ show err

encodeCborList :: CBOR.Serialise a => [a] -> BSB.Builder
encodeCborList = CBOR.toBuilder . foldMap CBOR.encode

prettyPage :: LinkStyle -> Page -> String
prettyPage linkStyle (Page (PageName name) _ skeleton) =
    unlines $ [ T.unpack name, replicate (T.length name) '=', "" ]
            ++ map (prettySkeleton linkStyle) skeleton

type LinkStyle = PageName -> Maybe T.Text -> T.Text -> String

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

prettyParagraph :: LinkStyle -> Paragraph -> String
prettyParagraph renderLink (Paragraph paraId bodies) =
    "{" ++ unpackParagraphId paraId ++ "} " ++ concatMap go bodies ++ "\n"
  where
    go (ParaText t) = T.unpack t
    go (ParaLink name section _ anchor) = renderLink name section anchor

withLink :: LinkStyle
withLink (PageName name) section anchor =
    "["<>T.unpack anchor<>"]("<>T.unpack name<>msection<>")"
  where
    msection
      | Just s <- section = "#"<>T.unpack s
      | otherwise         = mempty

anchorOnly :: LinkStyle
anchorOnly _ _ anchor = T.unpack anchor

paraBodiesToId :: [ParaBody] -> ParagraphId
paraBodiesToId =
    ParagraphId . SBS.toShort . Base16.encode . SHA.finalize . foldl' SHA.update SHA.init . map toBS
  where
    toBS (ParaText t)       = TE.encodeUtf8 t
    toBS (ParaLink _ _ _ t) = TE.encodeUtf8 t
