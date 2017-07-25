{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeDocCorpus
  ( EdgeDoc(..)
  , pageToEdgeDocs
  , pagesToEdgeDocs
  ) where

import Data.Monoid hiding (All, Any)
import Control.DeepSeq
import GHC.Generics

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Hashable

import CAR.Types
import CAR.Utils

data EdgeDoc = EdgeDoc { edgeDocParagraphId     :: ParagraphId
                       , edgeDocArticleId       :: PageId
                       , edgeDocNeighbors       :: [PageId]
                       , edgeDocContent         :: T.Text
                       }
           deriving (Show, Generic)

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
    foldMap (go mempty) pageSkeleta
  where
    go :: [SectionHeading] -> PageSkeleton -> [EdgeDoc]
    go headings (Section heading _ children) =
        concatMap (go (heading : headings)) $ children
    go headings (Para paragraph) =
      [convertPara paragraph headings]
    go headings (Image{}) = []

    convertPara :: Paragraph -> [SectionHeading] -> EdgeDoc
    convertPara paragraph headings=
      let
        edgeDocParagraphId    = paraId $ paragraph
        edgeDocArticleId      = pageId
        edgeDocNeighbors      = [pageId] ++ (fmap linkTargetId $ paraLinks $ paragraph)
        edgeDocContent        = paragraphContent paragraph headings
      in EdgeDoc {..}
      where paragraphContent :: Paragraph -> [SectionHeading] -> T.Text
            paragraphContent paragraph headings =
              TL.toStrict (paraToText $ paragraph)
              <> (T.intercalate " " $ fmap getSectionHeading $ headings)
              <> (getPageName pageName)

edgeDocHasLinks :: EdgeDoc -> Bool
edgeDocHasLinks edgeDoc
  | _:_:_ <- edgeDocNeighbors edgeDoc = True
  | otherwise                         = False

pagesToEdgeDocs :: [Page] -> [EdgeDoc]
pagesToEdgeDocs =
    foldMap (filter edgeDocHasLinks . pageToEdgeDocs)
