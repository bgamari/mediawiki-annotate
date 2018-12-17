{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module EdgeDocCorpus
  ( EdgeDoc(..)
  , pageToEdgeDocs
  , pagesToEdgeDocs
  , paragraphToEdgeDocs
  , paragraphsToEdgeDocs

  -- ^ usage
  , EdgeDocsLookup
  , wrapEdgeDocsTocs
  , readEdgeDocsToc
  ) where

import Data.Monoid hiding (All, Any)
import Data.Ord
import Control.DeepSeq
import GHC.Generics

import Codec.Serialise
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import Data.Maybe
import CAR.TocFile as Toc


import qualified Data.SmallUtf8 as Utf8

import CAR.Types
import CAR.Utils

data EdgeDoc = EdgeDoc { edgeDocParagraphId     :: !ParagraphId
                       , edgeDocArticleId       :: !PageId
                       , edgeDocNeighbors       :: !(HS.HashSet PageId)
                       , edgeDocContent         :: !T.Text
                       }
           deriving (Show, Generic)

instance Ord EdgeDoc where
    compare = comparing $ \x -> (edgeDocParagraphId x, edgeDocArticleId x)

instance Serialise EdgeDoc
instance NFData EdgeDoc

instance Eq EdgeDoc where
    x == y =
           edgeDocParagraphId x == edgeDocParagraphId y
        && edgeDocArticleId x == edgeDocArticleId y

instance Hashable EdgeDoc where
    hashWithSalt salt x =
        hashWithSalt salt (edgeDocParagraphId x, edgeDocArticleId x)

anonymousPageId :: PageId
anonymousPageId = PageId $ Utf8.unsafeFromShortByteString "enwiki:anonymous"




-- ---- open and use ------

type EdgeDocsLookup =  ([ParagraphId] -> [EdgeDoc])

wrapEdgeDocsTocs :: HM.HashMap ParagraphId EdgeDoc
                 -> EdgeDocsLookup
wrapEdgeDocsTocs paraId2EdgeDoc =
    \paragraphIds -> catMaybes $ fmap (`HM.lookup` paraId2EdgeDoc) paragraphIds


readEdgeDocsToc :: Toc.IndexedCborPath ParagraphId EdgeDoc -> IO EdgeDocsLookup
readEdgeDocsToc edgeDocsFileWithToc = do
    toc <- Toc.open edgeDocsFileWithToc
    return $ \paragraphIds -> mapMaybe ( `Toc.lookup` toc) paragraphIds



-- -------  build ------------------

pageToEdgeDocs :: Page -> [EdgeDoc]
pageToEdgeDocs (Page pageName pageId _ _ pageSkeleta) =
    foldMap (go mempty) pageSkeleta
  where
    go :: [SectionHeading] -> PageSkeleton -> [EdgeDoc]
    go headings (Section heading _ children) =
        concatMap (go (heading : headings)) $ children
    go headings (Para paragraph) =
      [convertPara paragraph headings]
    go headings (Image _ children) =
        concatMap (go (headings)) $ children
    go headings (List _ paragraph) =
      [convertPara paragraph headings]
    go headings (Infobox _ args) =
        concatMap (concatMap (go headings) . snd) args

    convertPara :: Paragraph -> [SectionHeading] -> EdgeDoc
    convertPara para headings=
      let
        edgeDocParagraphId    = paraId para
        edgeDocArticleId      = pageId
        edgeDocNeighbors      = HS.fromList
                              $ [pageId] ++ fmap linkTargetId (paraLinks para)
        edgeDocContent        = paragraphContent para headings
      in EdgeDoc {..}

    paragraphContent :: Paragraph -> [SectionHeading] -> T.Text
    paragraphContent para headings =
         TL.toStrict
       $ TL.fromStrict (getPageName pageName)
      <> "\n"
      <> TL.intercalate " " (fmap (TL.fromStrict . getSectionHeading) headings)
      <> "\n"
      <> paraToText para

paragraphToEdgeDocs :: Paragraph -> [EdgeDoc]
paragraphToEdgeDocs para =
    [convertPara para]
  where
    go ::  PageSkeleton -> [EdgeDoc]
    go  (Para paragraph) =
      [convertPara paragraph ]

    convertPara :: Paragraph -> EdgeDoc
    convertPara para =
      let
        edgeDocParagraphId    = paraId para
        edgeDocArticleId      = anonymousPageId
        edgeDocNeighbors      = HS.fromList
                              $ fmap linkTargetId (paraLinks para)
        edgeDocContent        = paragraphContent para
      in EdgeDoc {..}

    paragraphContent :: Paragraph ->  T.Text
    paragraphContent para  =
         TL.toStrict $ paraToText para

edgeDocHasLinks :: EdgeDoc -> Bool
edgeDocHasLinks = not . HS.null . edgeDocNeighbors

pagesToEdgeDocs :: [Page] -> [EdgeDoc]
pagesToEdgeDocs =
    foldMap (filter edgeDocHasLinks . pageToEdgeDocs)

paragraphsToEdgeDocs :: [Paragraph] -> [EdgeDoc]
paragraphsToEdgeDocs =
    foldMap (filter edgeDocHasLinks . paragraphToEdgeDocs)

