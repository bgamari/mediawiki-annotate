-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields#-}

module Utils where

import Data.Aeson as Aeson
import Data.Text.Encoding

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Hashable
import Data.Foldable

import qualified Data.Text as T

import CAR.Types
import Types
import PageStats
import Data.Time


loadPage pageFile = do
    pages <- loadJsonL pageFile
         :: IO [AssessmentPage]

    forM_ pages (\page -> BSLC.putStrLn $ Aeson.encode page)


loadJson :: FilePath -> IO AssessmentPage
loadJson pageFile = do
            page <- either error id . Aeson.eitherDecode <$> BSL.readFile pageFile
                 :: IO AssessmentPage
            return page

loadJsonL :: FilePath -> IO [AssessmentPage]
loadJsonL pageFile = do
            lines <- BSLC.lines <$> BSL.readFile pageFile
            let parseAssessmentPage :: BSL.ByteString -> AssessmentPage
                parseAssessmentPage line =
                    case Aeson.eitherDecode line of
                        Left msg -> error msg
                        Right page -> page

                pages :: [AssessmentPage]
                pages = fmap parseAssessmentPage lines
            return pages


loadRuns :: [FilePath] -> IO (M.Map (QueryId, RunId) AssessmentPage)
loadRuns filePaths = do
    multiPages <- mapM (loadJsonL)  filePaths
    return $ M.fromList
           $ [ ((apSquid page, apRunId page), page)
             | run <- multiPages
             , page <- run
             ]

loadAssessment :: FilePath -> IO SavedAssessments
loadAssessment file =
    either error id . Aeson.eitherDecode <$> BSL.readFile file
--       :: IO SavedAssessments

loadUserAssessments :: [FilePath] -> IO (M.Map (UserId, QueryId) [SavedAssessments]) -- sorted list
loadUserAssessments filePaths = do
    multiAssessments <- mapM (loadAssessment) filePaths
    let userToAssessment =
            M.fromListWith (<>)
            [ ((user, queryId), [sa])
            | sa@SavedAssessments{metaData = AssessmentMetaData{annotatorIds = users}, savedData = state } <- multiAssessments
            , user <- users
            , queryId <- S.toList $ assessmentStateQueryIds state
            ]
    return $  fmap (sortList) userToAssessment

  where sortList :: [SavedAssessments] -> [SavedAssessments]
        sortList lst = sortOn (\x -> (timeStamp :: AssessmentMetaData -> Maybe UTCTime ) $ metaData x) lst

writeAssessmentState :: FilePath -> AssessmentState -> IO()
writeAssessmentState outFile state =
    BSL.writeFile outFile $ Aeson.encode state

writeAssessment :: FilePath -> SavedAssessments -> IO()
writeAssessment outFile state =
    BSL.writeFile outFile $ Aeson.encode state


