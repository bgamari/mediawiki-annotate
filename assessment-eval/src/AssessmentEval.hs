{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

newtype DocumentId = DocumentId {unDocumentId:: T.Text}
                   deriving (Eq, Ord, Show, Hashable)

type Assessments = HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance

newtype Assessor = Assessor {unAssessor :: T.Text}
                 deriving (Eq, Ord, Show, Hashable)

readAssessments :: FilePath -> IO Assessments
readAssessments = fmap (foldMap toAssessments) . QRel.readQRel
  where
    toAssessments :: QRel.Entry QRel.QueryId QRel.DocumentName QRel.GradedRelevance -> Assessments
    toAssessments QRel.Entry{..} =
        HM.singleton (QueryId queryId, DocumentId documentName) relevance

assessorFromFilepath :: FilePath -> Assessor
assessorFromFilepath path = fromMaybe (error $ "Failed to get assessor from file path: "++path) $ do
    let fpath = dropExtension' $ T.pack $ takeFileName path
    annotator: _ <- pure $ "-" `T.splitOn` fpath
    return $ Assessor annotator

dateFromFilepath :: FilePath -> UTCTime
dateFromFilepath path = fromMaybe (error $ "Failed to get date from file path: "++path) $ do
    let fpath = dropExtension' $ T.pack $ takeFileName path
    let splits = "-" `T.splitOn` fpath
        dateStr = "-" `T.intercalate` drop 2 splits  -- drop login and session, reunite rest of date string
    parseTimeM False defaultTimeLocale "%F-%H%M" (T.unpack dateStr)
      -- Format:   $login-$session-$data.json


dropExtension' :: T.Text -> T.Text
dropExtension' str =
    T.takeWhile (/='.') str

