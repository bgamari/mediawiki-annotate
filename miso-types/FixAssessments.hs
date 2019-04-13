-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}

module Main where

import Data.Aeson as Aeson

import Control.Monad
import GHC.Generics
import Control.Exception
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe
import Data.Hashable

import Options.Applicative
import qualified Data.Text as T

import CAR.Types
import Types


options :: Parser (FilePath, FilePath, FilePath)
options =
    (,,)
      <$> argument str (metavar "AssessFILE" <> help "Assessments (JSON file)")
      <*> option str (short 'p' <> long "page" <> metavar "PageFILE" <> help "Page definition file (JSON file)")
      <*> option str (short 'o' <> long "output" <> metavar "PageFILE" <> help "Page definition file (JSON file)")

main :: IO ()
main = do
    (inputAssessmentFile, pageFile, outputAssessmentFile) <- execParser $ info options mempty

    page <- either error id . Aeson.eitherDecode <$> BSL.readFile pageFile
         :: IO AssessmentPage
    assessments <- either error id . Aeson.eitherDecode <$> BSL.readFile inputAssessmentFile
         :: IO SavedAssessments

    when (isAssessedPage page assessments) $ do
        putStrLn $ "Assessment page: "<> show pageFile
        let assessments' = fixAssessments page assessments
        BSL.writeFile outputAssessmentFile $ Aeson.encode assessments'

isAssessedPage  :: AssessmentPage -> SavedAssessments -> Bool
isAssessedPage  AssessmentPage{apParagraphs=paragraphs, apSquid=pageQid}
                s@SavedAssessments{savedData=state@AssessmentState{..}} =
    let pageParas = S.fromList
                  $ [ pid
                    | Paragraph{paraId=pid} <- paragraphs
                    ]
        transitionLabelParas = S.fromList
                           $ concat
                           $  [ [paragraphId1, paragraphId2]
                             | (AssessmentTransitionKey{..}, _) <- M.toList $ transitionLabelState
                             , queryId == pageQid
                             ]
        labelParas = S.fromList
                   $  [ paragraphId
                     | (AssessmentKey{..}, _) <- M.toList $ labelState
                     , queryId == pageQid
                     ]

        numTransitionOverlap = length $ pageParas `S.intersection` transitionLabelParas
        rateTransitionOverlap = (realToFrac numTransitionOverlap) / (realToFrac $ length transitionLabelParas)
        numLabelOverlap = length $ pageParas `S.intersection` labelParas
        rateLabelOverlap = (realToFrac numLabelOverlap) / (realToFrac $ length labelParas)

    in rateLabelOverlap>0.9 && rateTransitionOverlap > 0.9


fixAssessments :: AssessmentPage -> SavedAssessments -> SavedAssessments
fixAssessments AssessmentPage{apParagraphs=paragraphs, apSquid=pageQid} s@SavedAssessments{savedData=state@AssessmentState{..}} =
    let paraHiddenMap = M.fromList
                      $ [ (paraId, hiddenValue )
                        | (AssessmentKey{paragraphId=paraId, queryId=assessQid} , hiddenValue) <- M.toList hiddenState
                        , assessQid == pageQid
                        ]
        unhiddenParas = [ paraId
                     | Paragraph{paraId=paraId} <- paragraphs
                     , let isHidden = fromMaybe False $ paraId `M.lookup` paraHiddenMap
                     , not isHidden
                     ]
        fetchPreviousPara = M.fromList [ (p2, p1) | (p1, p2) <- zip unhiddenParas (drop 1 $ unhiddenParas)]
        transitionLabelState' = M.fromList
                              $ catMaybes  -- drop transitions that don't have a visible predecessor
                              $  [ if maybePara1' == Nothing then Nothing else Just (transKey',transValue)
                                 | (transKey@AssessmentTransitionKey{paragraphId1=para1, paragraphId2=para2}, transValue) <- M.toList transitionLabelState
                                 , let maybePara1' = para2 `M.lookup` fetchPreviousPara
                                 , let transKey' = transKey {paragraphId1=(fromJust maybePara1')}
                                 ]
    in s{savedData=state{transitionLabelState=transitionLabelState'} }
