{-# LANGUAGE OverloadedStrings #-}

module CAR.QRelFile
  ( RelevanceScale (..)
  , IsRelevant (..)
  , GradedRelevance(..)
  , Annotation (..)
  , EntityAnnotation (..)
  , EntityPassageAnnotation (..)
  , readEntityQRel, writeEntityQRel
  , readEntityPassageQRel, writeEntityPassageQRel
  , readParagraphQRel, writeParagraphQRel
  )  where

import Data.Semigroup
import qualified Data.Text as T

import CAR.Types
import SimplIR.Format.QRel -- hiding (IsRelevant(..))
import CAR.RunFile (parseEntityPassageString, writeEntityPassageString)
-- Ground truth
-- data Relevance = Relevant | NonRelevant
--                deriving (Show, Eq, Ord)
--
-- instance RelevanceScale Relevance where
--     parseRelevance "0" = NonRelevant
--     parseRelevance "1" = Relevant
--     parseRelevance s   = error $ "binaryRelevance: unknown relevance: "++show s
--
--     formatRelevance NonRelevant = "0"
--     formatRelevance Relevant    = "1"

-- | A relevance annotation of a paragraph in a section
data Annotation rel = Annotation SectionPath ParagraphId rel
                deriving (Show, Eq, Ord)

data EntityAnnotation rel = EntityAnnotation SectionPath PageId rel
                deriving (Show, Eq, Ord)

data EntityPassageAnnotation rel = EntityPassageAnnotation SectionPath PageId ParagraphId rel
                deriving (Show, Eq, Ord)

readEntityQRel :: RelevanceScale rel => FilePath -> IO [EntityAnnotation rel]
readEntityQRel path = map to <$> readQRel path
  where
    to (Entry qid doc rel)
      | Just spath <- parseSectionPath qid =
        EntityAnnotation spath (packPageId $ T.unpack doc) rel
      | otherwise = error $ "readEntityQRel: Couldn't parse section path: " <> show qid

writeEntityQRel :: RelevanceScale rel => FilePath -> [EntityAnnotation rel] -> IO ()
writeEntityQRel path = writeQRel path . map to
  where
    to (EntityAnnotation qid doc rel) =
        Entry (T.pack $ escapeSectionPath qid) (T.pack $ unpackPageId doc) rel

readEntityPassageQRel :: RelevanceScale rel => FilePath -> IO [EntityPassageAnnotation rel]
readEntityPassageQRel path = map to <$> readQRel path
  where
    to (Entry qid docName rel)
      | Just spath <- parseSectionPath qid =
        EntityPassageAnnotation spath (packPageId $ T.unpack ent) (packParagraphId $ T.unpack psg) rel
      | otherwise = error $ "readEntityQRel: Couldn't parse section path: " <> show qid
        where (ent, psg) = parseEntityPassageString docName

writeEntityPassageQRel :: RelevanceScale rel => FilePath -> [EntityPassageAnnotation rel] -> IO ()
writeEntityPassageQRel path = writeQRel path . map to
  where
    to (EntityPassageAnnotation qid ent psg rel) =
        Entry (T.pack $ escapeSectionPath qid) (writeEntityPassageString (T.pack $ unpackParagraphId psg, T.pack $ unpackPageId ent)) rel

readParagraphQRel :: RelevanceScale rel => FilePath -> IO [Annotation rel]
readParagraphQRel path = map to <$> readQRel path
  where
    to (Entry qid doc rel)
      | Just spath <- parseSectionPath qid =
        Annotation spath (packParagraphId $ T.unpack doc) rel
      | otherwise = error $ "readEntityQRel: Couldn't parse section path: " <> show qid

writeParagraphQRel :: RelevanceScale rel => FilePath -> [Annotation rel] -> IO ()
writeParagraphQRel path = writeQRel path . map to
  where
    to (Annotation qid doc rel) =
        Entry (T.pack $ escapeSectionPath qid) (T.pack $ unpackParagraphId doc) rel
