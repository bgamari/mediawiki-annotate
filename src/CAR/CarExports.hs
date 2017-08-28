{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module CAR.CarExports
    ( PassageFile
      -- * Stubs
    , Stub(..)
    , toStubSkeleton
    , prettyStub
      -- * Paragraphs
    , toParagraphs
      -- * Ground truth
    , Relevance(..)
    , Annotation(..)
    , EntityAnnotation(..)
    , toAnnotations
    , toEntityAnnotations
    , prettyAnnotation
    , prettyEntityAnnotation
    ) where

import Data.Maybe
import qualified Data.DList as DList
import qualified Data.Text as T
import qualified Data.Set as S

import Data.MediaWiki.Markup (PageName(..))
import CAR.Types hiding (paraId)
import CAR.Utils

-- Passage file
type PassageFile = [Paragraph]

-- Ground truth
data Relevance = Relevant | NonRelevant
               deriving (Show, Eq, Ord)

-- | A relevance annotation of a paragraph in a section
data Annotation = Annotation SectionPath ParagraphId Relevance
                deriving (Show, Eq, Ord)

data EntityAnnotation = EntityAnnotation SectionPath PageId Relevance
                deriving (Show, Eq, Ord)

-- | In TREC @qrel@ format.
prettyAnnotation :: Annotation -> String
prettyAnnotation (Annotation sectionPath paraId rel) =
    unwords [ escapeSectionPath sectionPath
            , "0"
            , unpackParagraphId paraId
            , case rel of
                Relevant    -> "1"
                NonRelevant -> "0"
            ]
prettyEntityAnnotation :: EntityAnnotation -> String
prettyEntityAnnotation (EntityAnnotation sectionPath entityId rel) =
    unwords [ escapeSectionPath sectionPath
            , "0"
            , unpackPageId entityId
            , case rel of
                Relevant    -> "1"
                NonRelevant -> "0"
            ]

toStubSkeleton :: Page -> Stub
toStubSkeleton (Page name pageId meta skeleton) =
    Stub name pageId meta (mapMaybe go skeleton)
  where
    go :: PageSkeleton -> Maybe PageSkeleton
    go (Section heading headingId children) =
        Just $ Section heading headingId (mapMaybe go children)
    go (Para _) = Nothing
    go (Image{}) = Nothing

prettyStub :: Stub -> String
prettyStub (Stub (PageName name) _ _ skeleton) =
    unlines $ [ T.unpack name, replicate (T.length name) '=', "" ]
           ++ map (prettySkeleton anchorOnly) skeleton

toParagraphs :: Page -> [Paragraph]
toParagraphs =
    concatMap go . pageSkeleton
  where
    go :: PageSkeleton -> [Paragraph]
    go (Section _ _ children) = concatMap go children
    go (Para para) = [para]
    go (Image{}) = [] -- ignore images

toAnnotations :: Page -> S.Set Annotation
toAnnotations (Page _ pageId _ skeleton) =
    -- recurse into sections, recursively collect section path, emit one annotation per paragraph
    foldMap (go mempty) skeleton
  where
    go :: DList.DList HeadingId -> PageSkeleton -> S.Set Annotation
    go parentIds (Section _ sectId children) =
        let parentIds' = parentIds `DList.snoc` sectId
        in foldMap (go parentIds') children
    go parentIds (Para (Paragraph paraId _)) =
        S.singleton $ Annotation sectionPath paraId Relevant
      where
        sectionPath = SectionPath pageId (DList.toList parentIds)
    go _parentIds (Image{}) = mempty

toEntityAnnotations :: (PageId -> PageId) -> Page ->  S.Set EntityAnnotation
toEntityAnnotations resolveRedirect (Page _ pageId _ skeleton) =
    -- recurse into sections, recursively collect section path, emit one entity annotation per link
    foldMap (go mempty) skeleton
  where
    go :: DList.DList HeadingId -> PageSkeleton -> S.Set EntityAnnotation
    go parentIds (Section _ sectionId children) =
        let parentIds' = parentIds `DList.snoc` sectionId
        in foldMap (go parentIds') children
    go parentIds (Para paragraph) =
        let entityIds =  filter (not . badEntityId)
                      $ fmap (resolveRedirect . linkTargetId)
                      $ paraLinks paragraph
        in S.fromList
            $  [EntityAnnotation sectionPath entityId Relevant
               | entityId <- entityIds
               ]
      where
        sectionPath = SectionPath pageId (DList.toList parentIds)
        badEntityId entityId = null $ unpackPageId entityId
    go _parentIds (Image{}) = mempty
