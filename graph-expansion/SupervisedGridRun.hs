{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Control.Parallel.Strategies

import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import System.Random
import GHC.Generics

import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.List
import Data.Maybe

import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.Utils
import GridFeatures
import TrainAndStore

import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace.Normalise

import qualified CAR.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import MultiTrecRunFile


import Debug.Trace

type NumResults = Int

type EntityIndex = Index.OnDiskIndex Term PageId Int

data SeedDerivation = SeedsFromLeadSection
                    | SeedsFromEntityIndex EntityIndex
                    -- | SeedsFromHeadingEntityLinks -- TODO

data QuerySource = QueriesFromCbor FilePath QueryDerivation SeedDerivation
                 | QueriesFromJson FilePath

data RankingType = EntityRanking | EntityPassageRanking
  deriving (Show)

data ModelSource = ModelFromFile FilePath -- filename to read model from
                 | TrainModel FilePath -- filename to write resulting file to
  deriving (Show)

data ExperimentSettings = AllExp | NoEdgeFeats | NoEntityFeats | AllEdgeWeightsOne | JustAggr | JustScore | JustRecip | JustSimpleRm-- | LessFeatures | ExpPage | JustNone
  deriving (Show, Read, Ord, Eq, Enum, Bounded)





opts :: Parser ( FilePath
               , FilePath
               , QuerySource
               , [CarRun.QueryId]
               , NumResults
               , [(GridRun, EntityOrEdge, FilePath)]
               , FilePath
               , ModelSource
               , [ExperimentSettings]
               , Maybe MiniBatchParams
               )
opts =
    (,,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
    <*> some gridRunParser
    <*> (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> modelSource
    <*> many (option auto (long "exp" <> metavar "EXP" <> help ("one or more switches for experimentation. Choices: " ++(show [minBound @ExperimentSettings .. maxBound]))))
    <*> optional minibatchParser
    where

      querySource :: Parser QuerySource
      querySource =
              fromCborTitle
          <|> option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")
          <|> fromEntityIndex
        where
          queryDeriv =
              flag QueryFromPageTitle QueryFromSectionPaths
                   (long "query-from-sections" <> help "Use sections as query documents")
          fromCborTitle =
              QueriesFromCbor
                <$> option str (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
                <*> queryDeriv
                <*> pure SeedsFromLeadSection

          fromEntityIndex =
              QueriesFromCbor
                <$> option str (short 'Q' <> long "queries-nolead" <> metavar "CBOR" <> help "Queries from CBOR pages taking seed entities from entity retrieval")
                <*> queryDeriv
                <*> option (SeedsFromEntityIndex . Index.OnDiskIndex <$> str) (long "entity-index" <> metavar "INDEX" <> help "Entity index path")

      modelSource :: Parser ModelSource
      modelSource =
            option (TrainModel <$> str) (long "train-model" <> metavar "Model-FILE" <> help "train learning-to-rank model and write to Model-FILE")
--         <|> option (ModelFromFile <$> str) (long "read-model" <> metavar "Model-FILE" <> help "read learning-to-rank model from Model-FILE")



-- --------------------------------- Query Doc ------------------------------------------------------


data QueryDoc = QueryDoc { queryDocQueryId      :: !CarRun.QueryId
                         , queryDocQueryText    :: !T.Text
                         }
           deriving (Show, Generic)
instance FromJSON QueryDoc
instance ToJSON QueryDoc

data QueryDocList = QueryDocList { queryDocListContent :: [QueryDoc]}
           deriving Generic
instance FromJSON QueryDocList
instance ToJSON QueryDocList

data QueryDerivation = QueryFromPageTitle | QueryFromSectionPaths

pagesToQueryDocs :: QueryDerivation
                 -> [Page]
                 -> [QueryDoc]
pagesToQueryDocs deriv pages =
    queryDocs
  where
    queryDocs = case deriv of
      QueryFromPageTitle ->
          [ QueryDoc { queryDocQueryId      = CarRun.pageIdToQueryId $  pageId page
                     , queryDocQueryText    = getPageName $ pageName page
                     }
          | page <- pages
          ]
      QueryFromSectionPaths ->
          [ QueryDoc { queryDocQueryId      = CarRun.sectionPathToQueryId sectionPath
                     , queryDocQueryText    = T.unwords
                                            $ getPageName (pageName page) : getPageName (pageName page) -- twice factor
                                              : map getSectionHeading headings
                     }
          | page <- pages
          , (sectionPath, headings, _) <- pageSections page
          ]


-- ---------------------------------------------------------------------------------------



main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc,
      queryRestriction, numResults, gridRunFiles
      , qrelFile, modelSource
      , experimentSettings
      , miniBatchParamsMaybe) <- execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction
    putStrLn $ " MinbatchParams (only for training) : "++ (show miniBatchParamsMaybe)

    let miniBatchParams = fromMaybe defaultMiniBatchParams miniBatchParamsMaybe
    let entityRunFiles  = [ (g, r) | (g, Entity, r) <- gridRunFiles]

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ))

    F.SomeFeatureSpace (entFSpace :: F.FeatureSpace EntityFeature allEntFeats) <- pure entSomeFSpace
    F.SomeFeatureSpace (edgeFSpace :: F.FeatureSpace EdgeFeature allEdgeFeats) <- pure edgeSomeFSpace

    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries


    let fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile

    queries <-
        if null queryRestriction
          then return queries'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queries'
    putStrLn $ "# query count: " ++ show (length queries)



    entityRuns <-  mapM (mapM CAR.RunFile.readEntityRun) entityRunFiles  -- mapM mapM -- first map over list, then map of the snd of a tuple

    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
        collapsedEntityRun = collapseRuns entityRuns

        tr x = traceShow x x

    case modelSource of
      ModelFromFile _file -> error $ "SupervisedGridRun Does not support  loading models from file"
      TrainModel modelFile -> do
          let docFeatures = makeFeatures entFSpace collapsedEntityRun

              allData :: TrainData EntityFeature allEntFeats
              allData = augmentWithQrels qrel docFeatures

              metric :: ScoringMetric IsRelevant QueryId
              metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant

              totalElems = getSum . foldMap ( Sum . length ) $ allData
              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

          putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
                    " queries and "++ show totalElems ++" items total of which "++
                    show totalPos ++" are positive."

          let displayTrainData :: Show f => TrainData f s -> [String]
              displayTrainData trainData =
                [ show k ++ " -> "++ show elem
                | (k,list) <- M.toList trainData
                , elem <- list]

          putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData allData)
          gen0 <- newStdGen  -- needed by learning to rank
          trainMe miniBatchParams gen0 allData entFSpace metric outputFilePrefix modelFile

--
-- expSettingToCrit :: [ExperimentSettings] ->  (CombinedFeature -> Bool)
-- expSettingToCrit exps fname =
--     all (`convert` fname) exps
--   where
--     convert :: ExperimentSettings -> (CombinedFeature -> Bool)
--     convert exp = case exp of
--                     AllExp -> const True
--                     NoEdgeFeats -> noEdge
--                     NoEntityFeats -> noEntity
--                     AllEdgeWeightsOne -> const True -- needs to be handled elsewhere
--                     JustAggr -> onlyAggr
--                     JustScore -> onlyScore
--                     JustRecip -> onlyRR


makeFeatures :: forall allEntFeats. ()
             => F.FeatureSpace EntityFeature allEntFeats
             -> M.Map QueryId [MultiRankingEntry PageId GridRun]
             -> M.Map (QueryId, QRel.DocumentName) (EntityFeatureVec allEntFeats)
makeFeatures entFSpace collapsedEntityRun =
    let
        docFeatures''' :: M.Map (QueryId, QRel.DocumentName) (EntityFeatureVec allEntFeats)
        docFeatures''' = M.fromList
                    $ withStrategy (parBuffer 200 $ evalTuple2 r0 rwhnf)
                    [ ((query, T.pack $ unpackPageId pid), features)
                    | (query, entityRun) <- M.toList collapsedEntityRun
                    , entityRankEntry <- entityRun
                    , let pid = multiRankingEntryGetDocumentName entityRankEntry
                    , let features =  makeEntFeatVector entFSpace (entityScoreVecFromMultiRankings entityRankEntry [])
                    ]


     -- todo filter with expCrits
--         docFeatures'' = fmap crit docFeatures'''
--                         where crit = filterExpSettings combinedFSpace'

        normalizer = zNormalizer $ M.elems docFeatures'''
        docFeatures = withStrategy (parTraversable rwhnf)
                    $fmap (normFeatures normalizer) docFeatures'''

    in docFeatures

