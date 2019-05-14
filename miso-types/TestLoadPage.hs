-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields#-}

module Main where

import Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.Text.Encoding

import Options.Applicative
import Control.Monad
import GHC.Generics
import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Hashable

import Options.Applicative
import qualified Data.Text as T

import CAR.Types
import Types
import qualified OldTypes as OldTypes

opts :: Parser (IO ())
opts = subparser
    $ cmd "page" loadPage'
    <> cmd "old-assessments" loadOldAssessments'
    <> cmd "assessments" loadAssessments'
    <> cmd "merge-pages" mergePages'
    <> cmd "convert-assessments" convertAssessments'
  where cmd name action = command name (info (helper <*> action) fullDesc)
        loadPage' = loadPage
                  <$> option str (short 'p' <> long "page" <> metavar "PageFILE" <> help "Page definition file (JSON file)")
        loadOldAssessments' = loadOldAssessments
                  <$> option str (short 'a' <> long "assessments" <> metavar "AssessFILE (old)" <> help "Assessment file (JSON file)")
        loadAssessments' = loadAssessments
                  <$> option str (short 'a' <> long "assessments" <> metavar "AssessFILE (new)" <> help "Assessment file (JSON file)")
        mergePages' = mergePages
                  <$> some (argument str (metavar "PageFILE" <> help "Page definition file (JSON file)"))
                  <*> option str (short 'o' <> long "output" <> metavar "JSON" <> help "Json file to write the merged paged to")
        convertAssessments' = convertAssessments
                  <$> option str (short 'a' <> long "assessments" <> metavar "AssessFILE (old)" <> help "Input Assessment file (JSON file)")
                  <*> option str (short 'o' <> long "output" <> metavar "AssessFILE (new)" <> help "Output Assessment file")

loadPage pageFile = do
    page <- either error id . Aeson.eitherDecode <$> BSL.readFile pageFile
         :: IO AssessmentPage

    Data.ByteString.Lazy.Char8.putStrLn $ AesonPretty.encodePretty page


mergePages :: [FilePath] -> FilePath -> IO ()
mergePages pageFiles outFile = do
    pages <- mapM load pageFiles
             :: IO [AssessmentPage]
    let out = SubmissionRun pages
    BSL.writeFile outFile $ Aeson.encode out

  where load :: FilePath -> IO AssessmentPage
        load pageFile = do
            page <- either error id . Aeson.eitherDecode <$> BSL.readFile pageFile
                 :: IO AssessmentPage
            return page



loadOldAssessments assessmentFile = do
    assess <- either error id . Aeson.eitherDecode <$> BSL.readFile assessmentFile
         :: IO OldTypes.SavedAssessments

    Data.ByteString.Lazy.Char8.putStrLn $ AesonPretty.encodePretty assess

loadAssessments assessmentFile = do
    assess <- either error id . Aeson.eitherDecode <$> BSL.readFile assessmentFile
         :: IO Types.SavedAssessments

    Data.ByteString.Lazy.Char8.putStrLn $ AesonPretty.encodePretty assess

convertAssessments oldAssessFile outFile = do
    oldAssessment <- either error id . Aeson.eitherDecode <$> BSL.readFile oldAssessFile
         :: IO OldTypes.SavedAssessments

    let newAssessment :: Types.SavedAssessments
        newAssessment = convert oldAssessment

    BSL.writeFile outFile $ Aeson.encode newAssessment
  where convert (x@OldTypes.SavedAssessments {metaData= metaData'}) =
            convertSavedAssessments x
          where
            convertSavedAssessments (OldTypes.SavedAssessments {..}) =
                Types.SavedAssessments {savedData = convertAssessmentState savedData, metaData = convertAssessmentMetaData metaData}
            convertAssessmentMetaData (OldTypes.AssessmentMetaData {..}) =
                Types.AssessmentMetaData {
                    runIds = fmap OldTypes.runId assessmentRuns
                    , annotatorIds = [userId]
                    , timeStamp = Just timeStamp
                    , sessionId = Nothing
                }
            convertAssessmentState (OldTypes.AssessmentState {..}) =
                Types.AssessmentState {
                    notesState = M.fromList [ (convertAssessmentKey k, [ wrapValue v])
                                            | (k, v) <- M.toList notesState
                                            ]
                    , facetState =  M.fromList [ (convertAssessmentKey k, [ wrapValue $ convertFacetValue fv lv] )
                                               | (k, fv) <- M.toList facetState
                                               , let lv = fromMaybe UnsetLabel $ k `M.lookup` labelState
                                               ]
                    , transitionLabelState = M.fromList [ (convertAssessmentTransitionKey k, wrapValue $ convertTransitionLabel v)
                                                        | (k, v) <- M.toList transitionLabelState
                                                        ]
                    , nonrelevantState = M.fromList [ (convertAssessmentKey k, wrapValue () )
                                                  | (k,v) <- M.toList hiddenState
                                                  , v  --  drop entries with value "False"
                                                  ]
                }
            convertAssessmentKey (OldTypes.AssessmentKey{..}) =
                Types.AssessmentKey {queryId = queryId
                                    , paragraphId = paragraphId
                                    }
            convertAssessmentTransitionKey (OldTypes.AssessmentTransitionKey {..}) =
                Types.AssessmentTransitionKey { queryId = queryId
                                              , paragraphId1 = paragraphId1
                                              , paragraphId2 = paragraphId2
                                              }
            convertFacetValue facet label =
                Types.FacetValue{ facet = convertAssessmentFacet facet
                                , relevance = label
                                }
            convertTransitionLabel  :: AssessmentTransitionLabel -> AssessmentTransitionLabel
            convertTransitionLabel  AppropriateTransition = CoherentTransition
            convertTransitionLabel  label = label

            convertAssessmentFacet (OldTypes.AssessmentFacet {..}) =
                Types.AssessmentFacet { apHeading = apHeading
                                      , apHeadingId = apHeadingId
                                      }

            wrapValue v =
                let OldTypes.AssessmentMetaData{userId=userId', timeStamp=timeStamp', assessmentRuns=assessmentRuns'} = metaData'
                in Types.AnnotationValue { annotatorId = userId'
                                        , timeStamp = timeStamp'
                                        , sessionId = ""
                                        , runIds = fmap OldTypes.runId assessmentRuns'
                                        , value = v
                                        }

main :: IO ()
main = join $ execParser' (helper <*> opts) mempty

execParser' :: Parser a -> InfoMod a -> IO a
execParser' parser pinfo =
    execParser $ info (parser) pinfo
