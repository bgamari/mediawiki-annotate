{-# LANGUAGE OverloadedStrings #-}

module CarExports
    ( ParaNumber(..)
    , PassageFile
      -- * Stubs
    , Stub(..)
    , toStubSkeleton
    , prettyStub
      -- * Paragraphs
    , toParagraphs
      -- * Ground truth
    , SectionPath(..)
    , Relevance(..)
    , Annotation(..)
    , toAnnotations
    , prettyAnnotation
    ) where

import Data.List (intercalate)
import Data.Maybe
import Data.Foldable
import qualified Data.ByteString.Char8 as BS
import qualified Data.DList as DList
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.URI

import qualified Data.Binary.Serialise.CBOR as CBOR

import Data.MediaWiki.Markup (PageName(..))
import Types
import AnnotationsFile

-- General

newtype ParaNumber = ParaNumber Int -- Sequential index
                   deriving (Show)

-- Passage file
type PassageFile = [Paragraph]

-- Stub
data Stub = Stub { stubName :: PageName
                 , stubSkeleton :: [PageSkeleton] }
          deriving (Show)

-- Ground truth
data SectionPath = SectionPath PageName [SectionHeading]
               deriving (Show)

data Relevance = Relevant | NonRelevant
               deriving (Show)

data Annotation = Annotation SectionPath ParagraphId Relevance
                deriving (Show)

escapeSectionPath :: SectionPath -> String
escapeSectionPath (SectionPath pageName headings) =
    escapeURIString isAllowedInURI
    $ intercalate "/" $ (T.unpack $ getPageName pageName) : map sectionHeading headings
  where
    sectionHeading (SectionHeading h) = T.unpack h

prettyAnnotation :: Annotation -> String
prettyAnnotation (Annotation sectionPath (ParagraphId paraId) rel) =
    unwords [ escapeSectionPath sectionPath
            , BS.unpack paraId
            , case rel of
                Relevant    -> "1"
                NonRelevant -> "0"
            ]

toStubSkeleton :: Page -> Stub
toStubSkeleton (Page name skeleton) =
    Stub name (mapMaybe go skeleton)
  where
    go :: PageSkeleton -> Maybe PageSkeleton
    go (Section heading children) =
        Just $ Section heading (mapMaybe go children)
    go (Para _) = Nothing

prettyStub :: Stub -> String
prettyStub (Stub (PageName name) skeleton) =
    unlines $ [ T.unpack name, replicate (T.length name) '=', "" ]
           ++ map prettySkeleton skeleton

toParagraphs :: Page -> [Paragraph]
toParagraphs (Page name skeleton) =
    concatMap go skeleton
  where
    go :: PageSkeleton -> [Paragraph]
    go (Section _ children) = concatMap go children
    go (Para para) = [para]

toAnnotations :: Page -> [Annotation]
toAnnotations (Page name skeleton) =
    concatMap (go mempty) skeleton
  where
    go :: DList.DList SectionHeading -> PageSkeleton -> [Annotation]
    go parents (Section section children) =
        let parents' = parents `DList.snoc` section
        in concatMap (go parents') children
    go parents (Para (Paragraph paraId body)) =
        [Annotation path paraId Relevant]
      where
        path = SectionPath name (DList.toList parents)
