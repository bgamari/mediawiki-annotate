-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields#-}

module Main where

import Options.Applicative
import Control.Monad
import GHC.Generics
import Control.Exception
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Hashable
import Data.Foldable
import Data.Time
import System.FilePath.Posix

import Options.Applicative
import qualified Data.Text as T

import CAR.Types
import Types
import PageStats

import Utils

data SaveAs = SaveAsState | SaveAsSavedAssessments

opts :: Parser (IO ())
opts = subparser
    $ cmd "page" loadPage'
    <> cmd "doIt" doIt'
  where cmd name action = command name (info (helper <*> action) fullDesc)
        loadPage' = loadPage
                  <$> option str (short 'p' <> long "page" <> metavar "PageFILE" <> help "Page definition file (JSON file)")
        doIt' = doIt
                  <$> many (argument str (metavar "AssessmentFILE" <> help "assessmentFile"))
                  <*> many (option str (short 'p' <> long "page" <> metavar "PageFILE" <> help "page File in JsonL format"))
                  <*> (option str (short 'o' <> long "outdir" <> metavar "DIR" <> help "directory to write merge results to") )
                  <*> (flag SaveAsState SaveAsSavedAssessments (long "saved-assessments" <> help "Save as SavedAssessments, otherwise save as AssessmentState"))


doIt :: [FilePath] -> [FilePath] -> FilePath -> SaveAs -> IO ()
doIt assessmentFiles pageFiles outDir saveAs = do
    user2Assessment <- loadUserAssessments assessmentFiles
    allRuns <- loadRuns pageFiles

    results <- mapM (mergeAssessmentsAndSave outDir saveAs) $ M.toList user2Assessment

    putStrLn $ unlines
          $ [ (show userId) <> " " <> (show queryId)  <> "  " <> (show $ apRunId page) <> ": "  <> (show $ pageStats queryId paragraphIds mergedState)
            | ((queryId, userId), mergedState) <- results
            , ((queryId2, _runId2), page ) <- M.toList allRuns
            , apSquid page == queryId
            , queryId2 == queryId
            , let paragraphIds = convertToParagraphIds page
            ]



mergeAssessmentsAndSave :: FilePath -> SaveAs -> ((UserId, QueryId), [SavedAssessments]) ->  IO (((QueryId, UserId), AssessmentState))
mergeAssessmentsAndSave outDir saveAs ((userId, queryId), asQList)  = do
    let outFile = outDir </> (T.unpack userId) <> "-" <> (T.unpack $ unQueryId queryId) <.> "json"

    mergedState <- if (and [ isJust nrs2 | SavedAssessments{savedData=AssessmentState{nonrelevantState2=nrs2}} <- asQList]) then
                        foldM accumulateNew (emptyAssessmentState) asQList
                   else
                        foldM accumulateNew (emptyAssessmentState) asQList
--                        foldlM accumulateThisRun (emptyAssessmentState) asQList

    case saveAs of
        SaveAsState ->
            writeAssessmentState outFile mergedState
        SaveAsSavedAssessments -> do
            now <- getCurrentTime
            let meta = AssessmentMetaData {runIds = S.toList $ assessmentStateRunIds mergedState
                                          , annotatorIds = [userId]
                                          , timeStamp = Just now
                                          , sessionId = Nothing  -- todo ignored currently
                                          }
            let savedAssessments = SavedAssessments {savedData = mergedState, metaData = meta}
            writeAssessment outFile savedAssessments



    print $ "Written "<> outFile
    return $ ((queryId, userId), mergedState)


accumulateNew :: (AssessmentState) -> SavedAssessments -> IO (AssessmentState)
accumulateNew (a@AssessmentState{}) (SavedAssessments{savedData = s@AssessmentState{..}, metaData = AssessmentMetaData{runIds = thisRunIds, timeStamp =ts}}) = do
    let a2 = mergeAssessmentState s a
    putStrLn $  "accumNew    timeStamp" <> (show ts) <> " " <> (show $ assessmentStateQueryIds s) <> " thisRunIds:" <> (show thisRunIds)
    return (a2)

accumulateThisRun :: (AssessmentState) -> SavedAssessments -> IO (AssessmentState)
accumulateThisRun (a@AssessmentState{}) (SavedAssessments{savedData = s@AssessmentState{..}, metaData = AssessmentMetaData{runIds = thisRunIds, timeStamp =ts}}) = do
    let filteredS = filterAssessmentStateByRunId (thisRunIds) s
        a2 = mergeAssessmentState filteredS a
        allRuns2 = assessmentStateRunIds a2
    debug s filteredS a2 allRuns2
    return (a2)
  where debug s filteredS a2 allRuns2 = do
            let allRuns = assessmentStateRunIds s
            putStrLn $  "accumOld timeStamp" <> (show ts) <> " " <> (show $ assessmentStateQueryIds s) <> " thisRunIds:" <> (show thisRunIds)
                     <> "   Difference in runIds = "<> show (allRuns `S.difference` allRuns2) <> "   and   " <>  show (allRuns2 `S.difference` allRuns)
                     <> "\n  origSize: "<> (show $ assessmentStateSize s)
                     <> "\n  diffSize: "<> (show $ assessmentStateSize filteredS)
                     <> "\n  accumSize: "<> (show $ assessmentStateSize a2)
                     <> "\n  which are lost? "<> ( unlines $ fmap show $ whichAreLost a2 s)

whichAreLost AssessmentState{nonrelevantState = lost1}  AssessmentState{nonrelevantState = lost2} =
            let lost = (S.fromList $ M.toList lost2) `S.difference` (S.fromList $ M.toList lost1)
            in [ (ts1, para1, runIds1) | (AssessmentKey{paragraphId=para1}, AnnotationValue{timeStamp= ts1, runIds=runIds1}) <- S.toList lost]


main :: IO ()
main = join $ execParser' (helper <*> opts) mempty

execParser' :: Parser a -> InfoMod a -> IO a
execParser' parser pinfo =
    execParser $ info (parser) pinfo
