{-# LANGUAGE OverloadedStrings #-}

module CAR.QRelFile where

import Data.Semigroup
import qualified Data.Text as T

import CAR.Types
import SimplIR.Format.QRel hiding (IsRelevant(..))

-- Ground truth
data Relevance = Relevant | NonRelevant
               deriving (Show, Eq, Ord)

instance RelevanceScale Relevance where
    parseRelevance "0" = NonRelevant
    parseRelevance "1" = Relevant
    parseRelevance s   = error $ "binaryRelevance: unknown relevance: "++show s

    formatRelevance NonRelevant = "0"
    formatRelevance Relevant    = "1"

-- | A relevance annotation of a paragraph in a section
data Annotation = Annotation SectionPath ParagraphId Relevance
                deriving (Show, Eq, Ord)

data EntityAnnotation = EntityAnnotation SectionPath PageId Relevance
                deriving (Show, Eq, Ord)

readEntityQRel :: FilePath -> IO [EntityAnnotation]
readEntityQRel path = map to <$> readQRel path
  where
    to (Entry qid doc rel)
      | Just spath <- parseSectionPath qid =
        EntityAnnotation spath (packPageId $ T.unpack doc) rel
      | otherwise = error $ "readEntityQRel: Couldn't parse section path: " <> show qid

writeEntityQRel :: FilePath -> [EntityAnnotation] -> IO ()
writeEntityQRel path = writeQRel path . map to
  where
    to (EntityAnnotation qid doc rel) =
        Entry (T.pack $ escapeSectionPath qid) (T.pack $ unpackPageId doc) rel

readParagraphQRel :: FilePath -> IO [Annotation]
readParagraphQRel path = map to <$> readQRel path
  where
    to (Entry qid doc rel)
      | Just spath <- parseSectionPath qid =
        Annotation spath (packParagraphId $ T.unpack doc) rel
      | otherwise = error $ "readEntityQRel: Couldn't parse section path: " <> show qid

writeParagraphQRel :: FilePath -> [Annotation] -> IO ()
writeParagraphQRel path = writeQRel path . map to
  where
    to (Annotation qid doc rel) =
        Entry (T.pack $ escapeSectionPath qid) (T.pack $ unpackParagraphId doc) rel
