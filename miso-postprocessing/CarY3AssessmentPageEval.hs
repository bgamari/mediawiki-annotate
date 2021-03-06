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
import Data.List as L
import Data.Maybe
import Data.Hashable
import Data.Foldable
import Data.Time
import System.FilePath.Posix

import Options.Applicative
import qualified Data.Text as T

--import QRel as Q
import CAR.QRelFile as Q

import CAR.Types
import Types
import PageStats
import CAR.QRelFile

import MisoPostprocessing.Utils
import Utils -- miso-types.Utils

import SimplIR.TrecEval

--data TrecEvalResult = TrecEvalResult { trecEvalMetric :: Metric
--                                     , trecEvalQuery  :: Maybe String
--                                     , trecEvalScore  :: Double
--                                     }

opts :: Parser (IO ())
opts = subparser
    $ cmd "doIt" doIt'
  where cmd name action = command name (info (helper <*> action) fullDesc)
        doIt' = doIt
                  <$> many (argument str (metavar "AssessmentFILE" <> help "assessmentFile"))
                  <*> many (option str (short 'p' <> long "page" <> metavar "PageFILE" <> help "page File in JsonL format"))


doIt :: [FilePath] -> [FilePath] -> IO ()
doIt assessmentFiles pageFiles = do
    query2Assessment <- loadQueryAssessments assessmentFiles

    allRuns <- loadRuns pageFiles
    let evalResults = M.mapWithKey (evalSingleRun query2Assessment) allRuns
--    putStrLn $ unlines [ show key <> "  " <> show res
--                      | (key,res) <- M.toList evalResults
--                      , not $ null res
--                      ]

    let runQueryScores :: M.Map (RunId, Metric) [Double]
        runQueryScores =  M.fromListWith (<>)
                      [ ((runId, metric), [score])
                      | ((_queryId, runId),res) <- M.toList evalResults
                      , TrecEvalResult{trecEvalMetric=metric, trecEvalScore=score} <- res
                      ]
        runScores :: M.Map (RunId, Metric) (Double, Double)
        runScores = fmap (\scores -> (mean scores, stderr scores)) runQueryScores
    putStrLn $ unlines [ show key <> "  " <> show res
                      | (key,res) <- M.toList runScores
                      ]


  where evalSingleRun :: (M.Map QueryId SavedAssessments) -> (QueryId, RunId) -> AssessmentPage -> [TrecEvalResult]
        evalSingleRun query2Assessment (queryId, runId) page@AssessmentPage{} =
            case (queryId `M.lookup` query2Assessment) of
                Nothing -> []
                Just SavedAssessments{savedData = assessmentState} ->
                    let pA = pageAssessments queryId (convertToParagraphIds page) assessmentState
                    in evalPageAssessments queryId pA

        evalPageAssessments :: QueryId ->  PageAssessmentData -> [TrecEvalResult]
        evalPageAssessments queryId pA =
            [facetOverlapMetric queryId pA
            , transitionMetric queryId pA
            , relevanceMetric queryId pA
            ]


-- | facetOverlap: Do neighboring pasages share the same facet?
-- | Ignore any paragraph that is marked as "remove". Of the remaining ones, count how often two neighboring passages
-- | are annotated with the same facet. (If passages are annotated with multiple facets, any shared facet will count)
-- | The relevance label of the facet is ignored, as long as it is at least "CAN"
-- | Caveat: This metric may indicate a high score, even if the page only covers a single facet, and even if most
-- | passages are non-relevant (i.e, removed)
facetOverlapMetric :: QueryId -> PageAssessmentData -> TrecEvalResult
facetOverlapMetric queryId PageAssessmentData{..} =
    TrecEvalResult { trecEvalMetric = Metric $ "facetOverlap"
                   , trecEvalQuery = Just $ T.unpack $ unQueryId queryId
                   , trecEvalScore = facetScore }
  where facetScore =
            meanOrDefault 0.0
            $ [ score
            | [(_, facetValues1), (_, facetValues2)] <- slidingWindow 2 facetAssessments
            , let fV1 = S.fromList [ apHeadingId fId | FacetValue{facet = fId} <- facetValues1]
            , let fV2 = S.fromList [ apHeadingId fId | FacetValue{facet = fId} <- facetValues2]
            , let score = if S.disjoint fV1 fV2 then 0.0 else 1.0
            ]


-- | Transition quality: How many transitions are marked as high quality.
-- | Ignore any paragraph that is marked as "remove". Of the remaining ones, count how often two neighboring passages
-- | are annotated with the quality Appropriate or Same transition (score 1), otherwise score 0 (for Topic Switch).
-- | Caveat: This metric may indicate a high score, even if the page only covers a single facet, and even if most
-- | passages are non-relevant (i.e, removed)
transitionMetric :: QueryId -> PageAssessmentData -> TrecEvalResult
transitionMetric queryId PageAssessmentData{..} =
    TrecEvalResult { trecEvalMetric = Metric $ "transition"
                   , trecEvalQuery = Just $ T.unpack $ unQueryId queryId
                   , trecEvalScore = transitionScore }
  where transitionScore =
            meanOrDefault 0.0
            $ [ score
            | (_, _, maybeTransitionLabel)  <- transitionAssessments
            , Just transitionLabel <- pure maybeTransitionLabel
            , let score = if transitionLabel `L.elem` [AppropriateTransition, SameTransition, CoherentTransition] then 1.0 else 0.0
            ]

-- | Relevance quality: among all paragraphs on the page, the average relevance judgment (Must: 1.0, SHould 0.66, Can 0.33, Not/Remove 0.0)
-- | Paragraph that is marked as "remove" will be counted as 0
-- | Caveat: This metric may indicate a high score, even if the page only covers a single facet.
relevanceMetric :: QueryId -> PageAssessmentData -> TrecEvalResult
relevanceMetric queryId PageAssessmentData{..} =
    TrecEvalResult { trecEvalMetric = Metric $ "relevance"
                   , trecEvalQuery = Just $ T.unpack $ unQueryId queryId
                   , trecEvalScore = relevanceScore }
  where relevanceScore =
            meanOrDefault 0.0
            $ [ score
            | (_, facetValues)  <- facetAssessments
            , let maxL = maximum [ label | FacetValue{relevance=label} <- facetValues]
            , Just rel <- pure $ assessmentLabelToGradedRelevance maxL
            , let score = realToFrac (max 0 ( unGradedRelevance  rel)) / 3.0
            ]
            <> [0.0 | paraId <- removedParagraphs] -- removed paragraphs

-- | Facet Coverage: among all paragraphs on the page, (a) count the number of given outline facets that have at least
-- | one relevant passage;  (b)  is each facet covered with the same number of passages (balance)?
-- | Ignore any paragraph that is marked as "remove". Of the remaining ones, count how often two neighboring passages
-- | are annotated with the quality Appropriate or Same transition (score 1), otherwise score 0 (for Topic Switch).
-- | Caveat: This metric may indicate a high score, even if the page only covers a single facet.

--coverageMetric :: QueryId -> PageAssessmentData -> TrecEvalResult
--coverageMetric queryId PageAssessmentData{..} =
--    TrecEvalResult { trecEvalMetric = Metric $ "coverage"
--                   , trecEvalQuery = Just $ T.unpack $ unQueryId queryId
--                   , trecEvalScore = coverageScore }
--  where coverageScore =
--        assFacets = ...
--        coveredFacets = S.fromList
--            $ [ facetId
--            | (_, facetValues)  <- facetAssessments
--            , facetId <-  [  apHeadingId fId
--               | FacetValue{facet = facetValues} <- facetValues
--               , FacetValue{facet = fId} <- facetValues]
--            ]

main :: IO ()
main = join $ execParser' (helper <*> opts) mempty

execParser' :: Parser a -> InfoMod a -> IO a
execParser' parser pinfo =
    execParser $ info (parser) pinfo


stderr :: [Double] -> Double
stderr scores =
    (sqrt $ variance scores) / (sqrt $ realToFrac $ length scores)

variance :: [Double] -> Double
variance scores =
    let mu = mean scores
    in mean [ (mu - s)^2 | s <- scores]
mean :: [Double] -> Double
mean scores = sum scores / realToFrac (length scores)

meanOrDefault :: Double -> [Double] -> Double
meanOrDefault def scores =
    case scores of
        [] -> def
        x -> mean x
