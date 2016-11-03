{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

import Data.List (intercalate)
import Data.Maybe
import Data.Foldable
import qualified Data.ByteString.Char8 as BS
import qualified Data.DList as DList
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

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

prettyAnnotation :: Annotation -> String
prettyAnnotation (Annotation (SectionPath pageName headings) (ParagraphId paraId) rel) =
    unwords [ T.unpack $ getPageName pageName
            , intercalate "/" $ map (\(SectionHeading h) -> T.unpack h) headings
            , BS.unpack paraId
            , show rel ]

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
    concatMap (go []) skeleton
  where
    go :: [SectionHeading] -> PageSkeleton -> [Annotation]
    go parents (Section section children) =
        let parents' = (section : parents)
        in concatMap (go parents') children
    go parents (Para (Paragraph paraId body)) =
        [Annotation path paraId Relevant]
      where
        path = SectionPath name (reverse parents)

main :: IO ()
main = do
    let path = "hello.cbor"
    anns <- openAnnotations path
    return ()
