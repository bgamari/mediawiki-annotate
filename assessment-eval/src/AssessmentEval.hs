{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module AssessmentEval where

import Data.Maybe
import Data.Semigroup
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import System.FilePath
import Data.Time.Clock
import Data.Time.Format

import qualified SimplIR.Format.QRel as QRel

newtype QueryId = QueryId T.Text
                deriving (Eq, Ord, Show, Hashable)

newtype DocumentId = DocumentId T.Text
                   deriving (Eq, Ord, Show, Hashable)

type Assessments = HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance

newtype Assessor = Assessor T.Text
                 deriving (Eq, Ord, Show, Hashable)

readAssessments :: FilePath -> IO Assessments
readAssessments = fmap (foldMap toAssessments) . QRel.readQRel
  where
    toAssessments :: QRel.Entry QRel.GradedRelevance -> Assessments
    toAssessments QRel.Entry{..} =
        HM.singleton (QueryId queryId, DocumentId documentName) relevance

assessorFromFilepath :: FilePath -> Assessor
assessorFromFilepath path =
    Assessor annotator
  where
    fpath = T.pack $ takeFileName path
    Just nosuffix = fpath `T.stripPrefix` ".json"
    annotator: _= "-" `T.splitOn` nosuffix

dateFromFilepath :: FilePath -> UTCTime
dateFromFilepath path =
    date
    where
      fpath = T.pack $ takeFileName path
      -- Format:   $login-$session-$data.json
      Just nosuffix =  ( fpath `T.stripPrefix` ".json")
      
      splits = "-" `T.splitOn` nosuffix
      dateStr = "-" `T.intercalate` (drop 2 splits)  -- drop login and session, reunite rest of date string
      Just date = parseTimeM False defaultTimeLocale "%F-%H%M" (T.unpack dateStr)