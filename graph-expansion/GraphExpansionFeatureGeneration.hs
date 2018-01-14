{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Control.Exception
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import Numeric.Log
import System.Random

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.List
import Data.Maybe

import CAR.Types
import CAR.ToolVersion
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.Utils (nubWithKey)
import CAR.TocFile as Toc


import EdgeDocCorpus
import WriteRanking
import GraphExpansionExperiments
import GraphExpansion
import qualified SimplIR.SimpleIndex as Index
import SimplIR.TopK (collectTopK)
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper

import qualified CAR.RunFile as CAR.RunFile
import qualified CAR.QRelFile as CAR.QRelFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Format.TrecRunFile as Run
import MultiTrecRunFile

import qualified Data.Vector.Unboxed as VU

import Debug.Trace

type NumResults = Int

type EntityIndex = Index.OnDiskIndex Term PageId Int

data SeedDerivation = SeedsFromLeadSection
                    | SeedsFromEntityIndex EntityIndex
                    -- | SeedsFromHeadingEntityLinks -- TODO

data QuerySource = QueriesFromCbor FilePath QueryDerivation SeedDerivation
                 | QueriesFromJson FilePath

data Graphset = Fullgraph | Subgraph
data RankingType = EntityRanking | EntityPassageRanking
  deriving (Show)

-- type IsRelevant = LearningToRank.IsRelevant


opts :: Parser ( FilePath
               , FilePath
               , QuerySource
               , [CarRun.QueryId]
               , NumResults
               , [FilePath]
               , [FilePath]
               , Toc.IndexedCborPath ParagraphId EdgeDoc
               , FilePath
               , FilePath
               )
opts =
    (,,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
    <*> many (option str (long "entityrun" <> metavar "ERUN" <> help "run files for entities/page ids"))
    <*> many (option str (long "edgedocrun" <> metavar "RUN" <> help "run files with edgedocs/paragraph ids"))
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "edge-doc-cbor" <> metavar "EdgeDoc-CBOR" <> help "EdgeDoc cbor file"))
    <*> (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> (option str (short 'm' <> long "model" <> metavar "Model-FILE"))
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


computeRankingsForQuery :: (Index.RetrievalModel Term EdgeDoc Int -> RetrievalFunction EdgeDoc)
                        -> AnnotationsFile
                        -> CarRun.QueryId  ->  [Term]
                        -> [(RetrievalFun, [(ParagraphId, Double)])]

computeRankingsForQuery
      retrieveDocs
      annsFile queryId query =
      let

        retrievalResults :: [(RetrievalFun, [(ParagraphId, Double)])]
        retrievalResults = [ (irname, retrievalResult)
                           | (irname, retrievalModel) <- retrievalModels
                           , let retrievalResult =
                                   fmap (\(ed, ld) -> (edgeDocParagraphId ed, ln ld)) $
                                   retrieveDocs retrievalModel query
                           ]

    in retrievalResults


logMsg :: CarRun.QueryId -> RetrievalFun -> String -> IO ()
logMsg queryId method t = T.putStr $ (CarRun.unQueryId queryId)<>"\t"<>T.pack (show method)<>"\t"<>T.pack t<>"\n"




main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc,
      queryRestriction, numResults, entityRunFiles, edgedocRunFiles, edgeDocsCborFile
      , qrelFile, modelFile) <- execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    siteId <- wikiSite . fst <$> readPagesFileWithProvenance articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) entityRunFiles)
    putStrLn $ "# EdgeDoc runs: "++ ( show $ fmap (show) edgedocRunFiles)

    gen0 <- newStdGen

    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs siteId queryDeriv <$> readPagesOrOutlinesAsPages queryFile

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

    edgeDocsLookup <- readEdgeDocsToc edgeDocsCborFile

    entityRuns <-  mapM CAR.RunFile.readEntityRun entityRunFiles
    edgedocRuns <-  mapM CAR.RunFile.readParagraphRun edgedocRunFiles

    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId]
        collapsedEntityRun = collapseRuns entityRuns
        collapsedEdgedocRun = collapseRuns edgedocRuns

        docFeatures :: M.Map (QueryId, QRel.DocumentName) Features
        docFeatures = M.fromList
                     [ ((qid, T.pack $ unpackPageId pid), features)
                     | (query, edgeRun) <- M.toList collapsedEdgedocRun
                     , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
                     , ((qid, pid), features) <- generateEntityFeatures edgeDocsLookup featuresOf query edgeRun entityRun
                     ]
        featureNames = fmap (FeatureName . T.pack . show) (entityRunFiles ++ edgedocRunFiles)        -- Todo Fix featureNames

        franking :: M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
        franking = augmentWithQrels qrel docFeatures Relevant

    -- Option a) drop in an svmligh style features annsFile
    -- Option b) stick into learning to rank
--         trainData = changeKey RunFile.unQueryId franking
        discardUntrainable :: M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)] -> M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
        discardUntrainable franking =
            M.filter hasPosAndNeg  franking
          where hasPosAndNeg list =
                  let hasPos = any (\(_,_,r) -> r == Relevant) list
                      hasNeg = any (\(_,_,r) -> r /= Relevant) list
                  in hasPos && hasNeg

        trainData :: M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
        trainData = discardUntrainable franking
        metric = avgMetricData trainData
--         metric = avgMetricQrel qrel
        totalElems = getSum . foldMap ( Sum . length ) $ trainData
        totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ trainData

    putStrLn $ "Training model with (trainData) "++ show (M.size trainData) ++
               " queries and "++ show totalElems ++" items total of which "++
               show totalPos ++" are positive."

    let displayTrainData :: M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
                         -> [String]
        displayTrainData trainData =
          [ show k ++ " -> "++ show elem
          | (k,list) <- M.toList trainData
          , elem <- list]

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (displayTrainData trainData)


    let (model, trainScore) = learnToRank trainData featureNames metric gen0
    putStrLn $ "Model train evaluation "++ show trainScore ++ " MAP."

    BSL.writeFile modelFile $ Data.Aeson.encode model

    putStrLn $ "Written model to file "++ (show modelFile) ++ " ."



changeKey :: Ord k' => (k-> k') -> M.Map k v -> M.Map k' v
changeKey f map_ =
    M.fromList $ fmap (\(key,val) -> (f key, val)) $ M.toList map_


generateEntityFeatures
    :: EdgeDocsLookup
    -> (PageId -> [EdgeDoc] -> MultiRankingEntry PageId -> [MultiRankingEntry ParagraphId] -> Features)
    -> QueryId
    -> [MultiRankingEntry ParagraphId]
    -> [MultiRankingEntry PageId]
    -> [((QueryId, PageId), Features)]
generateEntityFeatures edgeDocsLookup featuresOf' query edgeRun entityRun =
    let paraIdToEdgedocRun = HM.fromList [ (multiRankingEntryGetDocumentName run, run) | run <- edgeRun]
        edgeDocs = edgeDocsLookup $ HM.keys paraIdToEdgedocRun  -- ToDo create consistent edgedocs  NOW

        universalGraph = edgeDocsToUniverseGraph edgeDocs
                                                               -- Todo or features below will be empty!

    in  [ ((query, entity), featuresOf' entity edgeDocs entityRankEntry edgeDocsRankEntries)
        | entityRankEntry <- entityRun
        , let entity = multiRankingEntryGetDocumentName entityRankEntry  -- for each entity in ranking...
        , Just edgeDocs <-  pure $entity `HM.lookup` universalGraph             -- fetch adjacent edgedocs
        , let edgeDocsRankEntries :: [MultiRankingEntry ParagraphId]
              edgeDocsRankEntries =
                [ entry
                | edgeDoc <- edgeDocs
                , let paraId = edgeDocParagraphId edgeDoc
                , Just entry <- pure $ paraId `HM.lookup` paraIdToEdgedocRun   -- get edgedoc rank entries
                ]
        ]

fconcat :: Features -> Features -> Features
fconcat (Features xs) (Features ys) = Features  (xs VU.++ ys)

fsum :: Features -> Features -> Features
fsum (Features xs) (Features ys) = Features $ VU.zipWith (+) xs ys

featuresOf :: PageId
           -> [EdgeDoc]
           -> MultiRankingEntry PageId
           -> [MultiRankingEntry ParagraphId]
           -> Features
featuresOf entity edgeDocs entityRankEntry edgedocsRankEntries =
--     let vec = Features VU.fromList [1,3,4,5]

    let entityScoreVec = Features . VU.fromList
                       $ fmap CAR.RunFile.carScore -- todo named features; other features
                       $ multiRankingEntryAll entityRankEntry


        singleEdgeDocFeatureVec :: MultiRankingEntry ParagraphId -> Features
        singleEdgeDocFeatureVec edgedocRankEntry =
           Features . VU.fromList
           $ fmap CAR.RunFile.carScore -- todo named features; other features
           $ multiRankingEntryAll edgedocRankEntry

        (eHead: eRest) = fmap singleEdgeDocFeatureVec edgedocsRankEntries
        edgeDocScoreVec = foldr fsum eHead eRest

    in fconcat entityScoreVec edgeDocScoreVec


type EdgeDocsLookup =  ([ParagraphId] -> [EdgeDoc])

readEdgeDocsToc :: Toc.IndexedCborPath ParagraphId EdgeDoc -> IO EdgeDocsLookup
readEdgeDocsToc edgeDocsFileWithToc = do
    toc <- Toc.open edgeDocsFileWithToc
    return $ \paragraphIds -> mapMaybe ( `Toc.lookup` toc) paragraphIds


dropUnjudged :: Ord q
             => M.Map q [(QRel.DocumentName, Features, Maybe IsRelevant)]
             -> M.Map q [(QRel.DocumentName, Features, IsRelevant)]
dropUnjudged featureMap =
    M.filter (not . null)   -- drop entries with empty lists
    $ M.map (mapMaybe dropUnjudged') featureMap
   where dropUnjudged' (doc, feat, Just rel) = Just $ (doc, feat, rel)
         dropUnjudged' (_ , _, Nothing) = Nothing


