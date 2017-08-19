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
  ) where

import Data.Monoid hiding (All, Any)
import Data.Maybe
import Control.DeepSeq
import GHC.Generics

import Data.Binary
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashSet as HS
import Data.Hashable

import CAR.Types
import CAR.Utils

data EdgeDoc = EdgeDoc { edgeDocParagraphId     :: !ParagraphId
                       , edgeDocArticleId       :: !PageId
                       , edgeDocNeighbors       :: !(HS.HashSet PageId)
                       , edgeDocContent         :: !T.Text
                       }
           deriving (Show, Generic)

instance (Hashable a, Eq a, Binary a) => Binary (HS.HashSet a) where
    put = put . HS.toList
    get = HS.fromList <$> get
instance Binary EdgeDoc
instance NFData EdgeDoc

instance Eq EdgeDoc where
    x == y =
           edgeDocParagraphId x == edgeDocParagraphId y
        && edgeDocArticleId x == edgeDocArticleId y

instance Hashable EdgeDoc where
    hashWithSalt salt x =
        hashWithSalt salt (edgeDocParagraphId x, edgeDocArticleId x)

pageToEdgeDocs :: Page -> [EdgeDoc]
pageToEdgeDocs (Page pageName pageId pageSkeleta) =
    filter (not . isNullPageId . edgeDocArticleId) -- discard edge docs when articleId is an empty entity
    $ foldMap (go mempty) pageSkeleta
  where
    isNullPageId = null . unpackPageId

    go :: [SectionHeading] -> PageSkeleton -> [EdgeDoc]
    go headings (Section heading _ children) =
        concatMap (go (heading : headings)) $ children
    go headings (Para paragraph) =
      [convertPara paragraph headings]
    go _headings (Image{}) = []

    convertPara :: Paragraph -> [SectionHeading] -> EdgeDoc
    convertPara para headings=
      let
        edgeDocParagraphId    = paraId para
        edgeDocArticleId      = pageId
        edgeDocNeighbors      = HS.fromList $ filter (not . isNullPageId) -- kick out links to empty entity ids
                              $ [pageId] ++ fmap linkTargetId (paraLinks para)
        edgeDocContent        = paragraphContent para headings
      in EdgeDoc {..}

    paragraphContent :: Paragraph -> [SectionHeading] -> T.Text
    paragraphContent para headings =
         TL.toStrict
       $ paraToText para
      <> TL.intercalate " " (fmap (TL.fromStrict . getSectionHeading) headings)
      <> TL.fromStrict (getPageName pageName)

edgeDocHasLinks :: EdgeDoc -> Bool
edgeDocHasLinks = not . HS.null . edgeDocNeighbors

pagesToEdgeDocs :: [Page] -> [EdgeDoc]
pagesToEdgeDocs =
    foldMap (filter edgeDocHasLinks . pageToEdgeDocs) . filter (isNothing . pageRedirect)
