{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Foldable
import GHC.Generics
import Data.Monoid
import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Crypto.Hash.SHA1 as SHA
import qualified Data.ByteString.Base16 as Base16
import Data.MediaWiki.Markup
import Data.Aeson.Types
import Data.Hashable
import Data.String

-- Orphans
instance CBOR.Serialise PageName
deriving instance IsString PageName
instance FromJSON PageName
instance FromJSONKey PageName where
    fromJSONKey = fmap PageName fromJSONKey
instance ToJSON PageName
instance ToJSONKey PageName where
    toJSONKey = contramapToJSONKeyFunction (\(PageName n) -> n) toJSONKey
instance Hashable PageName

newtype SectionHeading = SectionHeading T.Text
                       deriving (Show, Generic)
instance Hashable SectionHeading
instance CBOR.Serialise SectionHeading

data Paragraph = Paragraph { paraId :: ParagraphId, paraBody :: [ParaBody] }
               deriving (Show, Generic)
instance CBOR.Serialise Paragraph

newtype ParagraphId = ParagraphId BS.ByteString -- Hash
                    deriving (Show, Generic)
instance CBOR.Serialise ParagraphId

data PageSkeleton = Section !SectionHeading [PageSkeleton]
                  | Para !Paragraph
                  deriving (Show, Generic)
instance CBOR.Serialise PageSkeleton

data ParaBody = ParaText !T.Text
              | ParaLink !PageName !T.Text
              deriving (Show, Generic)
instance CBOR.Serialise ParaBody

data Page = Page { pageName :: PageName, pageSkeleton :: [PageSkeleton] }
          deriving (Show, Generic)
instance CBOR.Serialise Page

readValues :: CBOR.Serialise a => BSL.ByteString -> [a]
readValues = start . BSL.toChunks
  where
    start = go CBOR.deserialiseIncremental
    go (CBOR.Partial f) (bs : bss) = go (f (Just bs)) bss
    go (CBOR.Done bs _ x) bss = x : start (bs : bss)
    go (CBOR.Fail rest _ err) _ = error $ show err
    go _ [] = error "ran out of data"

prettyPage :: Page -> String
prettyPage (Page (PageName name) skeleton) =
    unlines $ [ T.unpack name, replicate (T.length name) '=', "" ]
            ++ map prettySkeleton skeleton

type LinkStyle = PageName -> T.Text -> String
prettySkeleton' :: LinkStyle -> PageSkeleton -> String
prettySkeleton' renderLink = go 1
  where
    go :: Int -> PageSkeleton -> String
    go n (Section (SectionHeading name) children) =
        unlines
        $ [ replicate n '#' ++ " " ++ T.unpack name]
          <> map (go (n+1)) children
          <> [""]
    go _ (Para para) = prettyParagraph renderLink para

prettyParagraph :: LinkStyle -> Paragraph -> String
prettyParagraph renderLink (Paragraph (ParagraphId paraId) bodies) =
    "{" ++ BS.unpack paraId ++ "} " ++ concatMap go bodies ++ "\n"
  where
    go (ParaText t) = T.unpack t
    go (ParaLink name anchor) = renderLink name anchor

prettySkeletonWithLinks :: PageSkeleton -> String
prettySkeletonWithLinks = prettySkeleton' withLink

prettySkeleton :: PageSkeleton -> String
prettySkeleton = prettySkeleton' anchorOnly

withLink :: LinkStyle
withLink (PageName name) anchor = "["<>T.unpack anchor<>"]("<>T.unpack name<>")"

anchorOnly :: LinkStyle
anchorOnly _ anchor = T.unpack anchor

toParagraphId :: [ParaBody] -> ParagraphId
toParagraphId =
    ParagraphId . Base16.encode . SHA.finalize . foldl' SHA.update SHA.init . map toBS
  where
    toBS (ParaText t)   = TE.encodeUtf8 t
    toBS (ParaLink _ t) = TE.encodeUtf8 t
