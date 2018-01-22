{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import GHC.Generics
import Codec.Serialise

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Foldable as Foldable


import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.Utils (nubWithKey)
import CAR.TocFile as Toc


import EdgeDocCorpus
import WriteRanking
import GraphExpansionExperiments hiding (Bm25, Ql)
import GraphExpansion hiding (RetrievalFun, Bm25, Ql)
import qualified SimplIR.SimpleIndex as Index
import SimplIR.TopK (collectTopK)
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)


import qualified CAR.RunFile as CAR.RunFile
import qualified CAR.QRelFile as CAR.QRelFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Format.TrecRunFile as Run
import MultiTrecRunFile


import qualified CAR.Retrieve as Retrieve

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
               , FilePath, FilePath, FilePath, FilePath
               , Toc.IndexedCborPath ParagraphId EdgeDoc
               , FilePath
               , FilePath
               )
opts =
    (,,,,,,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
    <*> many (option str (long "entityrun" <> metavar "ERUN" <> help "run files for entities/page ids"))
    <*> many (option str (long "edgedocrun" <> metavar "RUN" <> help "run files with edgedocs/paragraph ids"))
    <*> option str (long "entity-bm25" <> metavar "RUN" <> help "entity BM25 run")
    <*> option str (long "entity-ql" <> metavar "RUN" <> help "entity Query Likelihood run")
    <*> option str (long "edgedoc-bm25" <> metavar "RUN" <> help "edgedoc BM25 run")
    <*> option str (long "edgedoc-ql" <> metavar "RUN" <> help "edge Query Likelihood run")
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


bm25MethodName :: CarRun.MethodName
bm25MethodName = CarRun.MethodName "BM25"
qlMethodName :: CarRun.MethodName
qlMethodName = CarRun.MethodName "QL"


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc,
      queryRestriction, numResults, entityRunFiles, edgedocRunFiles
      , entityBm25RunFile, entityQlRunFile, edgedocBm25RunFile, edgedocQlRunFile
      , edgeDocsCborFile
      , qrelFile, modelFile) <- execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    siteId <- wikiSite . fst <$> readPagesFileWithProvenance articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ++ [entityBm25RunFile, entityQlRunFile]))
    putStrLn $ "# EdgeDoc runs: "++ ( show $ fmap (show) (edgedocRunFiles ++ [edgedocBm25RunFile, edgedocQlRunFile]))

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


    let fixRun methodName entries = fmap (\entry -> entry {CarRun.carMethodName = methodName} ) entries

    entityBm25Run <- fixRun bm25MethodName <$> CAR.RunFile.readEntityRun entityBm25RunFile
    entityQlRun <- fixRun qlMethodName <$> CAR.RunFile.readEntityRun entityQlRunFile
    edgedocBm25Run <- fixRun bm25MethodName <$> CAR.RunFile.readParagraphRun edgedocBm25RunFile
    edgedocQlRun <- fixRun qlMethodName <$> CAR.RunFile.readParagraphRun edgedocQlRunFile

    entityRuns' <-  mapM CAR.RunFile.readEntityRun entityRunFiles
    edgedocRuns' <-  mapM CAR.RunFile.readParagraphRun edgedocRunFiles
    let entityRuns = entityRuns' ++ [entityBm25Run, entityQlRun]
    let edgedocRuns = edgedocRuns' ++ [edgedocBm25Run, edgedocQlRun]

    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId]
        collapsedEntityRun = collapseRuns entityRuns
        collapsedEdgedocRun = collapseRuns edgedocRuns

        docFeatures :: M.Map (QueryId, QRel.DocumentName) (FeatureVec CombinedFeatures Double)
        docFeatures = M.fromList
                     [ ((qid, T.pack $ unpackPageId pid), features)
                     | (query, edgeRun) <- M.toList collapsedEdgedocRun
                     , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
                     , ((qid, pid), features) <- generateEntityFeatures edgeDocsLookup featuresOf query edgeRun entityRun
                     ]
--         featureNames = fmap (FeatureName . T.pack . show) (entityRunFiles ++ edgedocRunFiles)        -- Todo Fix featureNames

        docFeatures' = fmap (Features . F.toVector) docFeatures
        featureNames = fmap (FeatureName . T.pack . show) $ F.featureNames combinedFSpace

        franking :: M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
        franking = augmentWithQrels qrel docFeatures' Relevant

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
--         metric = avgMetricData trainData
        metric = avgMetricQrel qrel
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

    putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData trainData)


    -- Train model on all data
    let (model, trainScore) = learnToRank trainData featureNames metric gen0
    putStrLn $ "Model train evaluation "++ show trainScore ++ " MAP."

    BSL.writeFile modelFile $ Data.Aeson.encode model

    putStrLn $ "Written model to file "++ (show modelFile) ++ " ."


    -- write train ranking
    CAR.RunFile.writeEntityRun (outputFilePrefix++"-train.run")
         $ l2rRankingToRankEntries (CAR.RunFile.MethodName "l2r train")
         $ rerankRankings' model franking


    -- todo load external folds
    let folds = chunksOf 5 $ M.keys trainData
        trainProcedure trainData = learnToRank trainData featureNames metric gen0
        predictRanking = kFoldCross (trainProcedure) folds trainData franking
        testScore = metric predictRanking

    putStrLn $ "K-fold cross validation score " ++ (show testScore)++"."

    -- write test ranking
    CAR.RunFile.writeEntityRun (outputFilePrefix++"-test.run")
        $ l2rRankingToRankEntries (CAR.RunFile.MethodName "l2r test")
        $ predictRanking
  where

      l2rRankingToRankEntries :: CAR.RunFile.MethodName -> Rankings rel CAR.RunFile.QueryId QRel.DocumentName -> [CAR.RunFile.EntityRankingEntry]
      l2rRankingToRankEntries methodName rankings =
        [ CAR.RunFile.RankingEntry { carQueryId = query
                      , carDocument = packPageId $ T.unpack doc
                      , carRank = rank
                      , carScore = score
                     , carMethodName = methodName
                     }
        | (query, Ranking ranking) <- M.toList rankings
        , ((score, (doc, rel)), rank) <- ranking `zip` [1..]
        ]


kFoldCross :: forall q docId rel. (Ord q)
           => (M.Map q [(docId, Features, rel)] -> (Model, Double))
           -> [[q]]
           -> M.Map q [(docId, Features, rel)]
           -> M.Map q [(docId, Features, rel)]
           -> M.Map q (Ranking (docId, rel))
kFoldCross trainProcedure folds allTrainData allTestData =
    M.unions $ fmap trainSingleFold folds
  where
    trainSingleFold :: [q]  -> M.Map q (Ranking (docId, rel))
    trainSingleFold testQueries =
      let testData :: M.Map q [(docId, Features, rel)]
          testData =  M.filterWithKey (\query _ -> query `elem` testQueries) allTestData
          trainData :: M.Map q [(docId, Features, rel)]
          trainData =  M.filterWithKey (\query _ -> query `notElem` testQueries) allTrainData
          (model, trainScore) = trainProcedure trainData
          testRanking :: M.Map q (Ranking (docId, rel))
          testRanking = rerankRankings' model testData
      in testRanking

changeKey :: Ord k' => (k-> k') -> M.Map k v -> M.Map k' v
changeKey f map_ =
    M.fromList $ fmap (\(key,val) -> (f key, val)) $ M.toList map_



-- Set up the feature space



data QueryModel = All | Title
         deriving (Show, Ord, Eq, Enum, Bounded, Generic, Serialise)
data RetrievalModel = Bm25 | Ql
         deriving (Show, Ord, Eq, Enum, Bounded, Generic, Serialise)
data ExpansionModel = NoneX | Rm | EcmX | EcmRm
         deriving (Show, Ord, Eq, Enum, Bounded, Generic, Serialise)
data IndexType = EcmIdx | EntityIdx | PageIdx | ParagraphIdx
         deriving (Show, Ord, Eq, Enum, Bounded, Generic, Serialise)

entityRuns :: [GridRun]
entityRuns = [ GridRun qm rm em it
             | qm <- enumAll
             , rm <- enumAll
             , (it, em) <- [ (EcmIdx, EcmX), (EcmIdx, EcmRm)
                           , (PageIdx, NoneX), (PageIdx, Rm)]
                           ++ [(EntityIdx, em) | em <- enumAll]
             ]


edgeDocRuns :: [GridRun]
edgeDocRuns = [ GridRun qm rm em it
             | qm <- enumAll
             , rm <- enumAll
             , (it, em) <- [ (EcmIdx, NoneX), (EcmIdx, Rm)
                           , (ParagraphIdx, NoneX), (ParagraphIdx, Rm)
                           ]
             ]



data GridRun = GridRun QueryModel RetrievalModel ExpansionModel IndexType
         deriving (Show, Ord, Eq, Generic, Serialise)
allGridRuns = GridRun <$> enumAll <*> enumAll <*> enumAll <*> enumAll

data Run = GridRun' GridRun | Aggr
         deriving (Show, Ord, Eq, Generic, Serialise)
allRuns = (GridRun' <$> allGridRuns) <> [Aggr]

data RunFeature = ScoreF | RecipRankF | LinearRankF | BucketRankF | CountF
         deriving (Show, Ord, Eq, Enum, Bounded, Generic, Serialise)
data EntityOrEdge = Entity | Edge
         deriving (Show, Ord, Eq, Enum, Bounded, Generic, Serialise)
data Feature (ty :: EntityOrEdge) where
    RetrievalFeature :: Run -> RunFeature -> Feature ty
    EntIncidentEdgeDocsRecip :: Feature 'Entity
    EntDegreeRecip :: Feature 'Entity
    EntDegree  :: Feature 'Entity
    EdgeDocKL  :: Feature 'Edge
    EdgeCount  :: Feature 'Edge
    --deriving (Show, Ord, Eq, Generic, Serialise)

deriving instance Show (Feature a)
deriving instance Ord (Feature a)
deriving instance Eq (Feature a)

allEntityFeatures :: [Feature 'Entity]
allEntityFeatures =
    (RetrievalFeature <$> allRuns <*> enumAll)
    <> [EntIncidentEdgeDocsRecip, EntDegreeRecip, EntDegree]

enumAll :: (Enum a, Bounded a) => [a]
enumAll = [minBound..maxBound]

allEdgeFeatures :: [Feature 'Edge]
allEdgeFeatures =
    (RetrievalFeature <$> allRuns <*> enumAll) <> [EdgeDocKL, EdgeCount]


type EntityFeatures = Feature 'Entity

type EdgeFeatures = Feature 'Edge

type CombinedFeatures = Either EntityFeatures EdgeFeatures

entFSpace :: FeatureSpace EntityFeatures
entFSpace = mkFeatureSpace allEntityFeatures

edgeFSpace :: FeatureSpace EdgeFeatures
edgeFSpace = mkFeatureSpace allEdgeFeatures

combinedFSpace :: FeatureSpace CombinedFeatures
combinedFSpace = concatSpace entFSpace edgeFSpace


makeEntFeatVector :: [(EntityFeatures,Double)] -> F.FeatureVec EntityFeatures Double
makeEntFeatVector xs =
    F.modify entFSpace defaults xs
 where defaults = F.fromList entFSpace ([ (EntIncidentEdgeDocsRecip, 0.0)
                                       , (EntDegreeRecip, 0.0)
                                       , (EntDegree, 0.0)
                                       ]
                                       ++ defaultRankFeatures' Aggr
                                       ++ defaultRankFeatures' (GridRun' $ GridRun All Bm25 NoneX EntityIdx)
                                       ++ defaultRankFeatures' (GridRun' $ GridRun All Ql NoneX EntityIdx)
                                        )

makeEdgeFeatVector :: [(EdgeFeatures,Double)] -> F.FeatureVec EdgeFeatures Double
makeEdgeFeatVector xs =
    F.modify edgeFSpace defaults xs
 where defaults = F.fromList edgeFSpace ([ (EdgeCount, 0.0)
                                        , (EdgeDocKL, 0.0)
                                        ]
                                       ++ defaultRankFeatures' Aggr
                                       ++ defaultRankFeatures' (GridRun' $ GridRun All Bm25 NoneX ParagraphIdx)
                                       ++ defaultRankFeatures' (GridRun' $ GridRun All Ql NoneX ParagraphIdx)                                        )

-- concatVectors :: FeatureVec EntityFeatures Double -> FeatureVec EdgeFeatures Double -> FeatureVec CombinedFeatures Double
-- concatVectors entFeats edgeFeats =  concatFeatureVec entFeats edgeFeats



defaultRankFeatures :: RunFeature -> Double
defaultRankFeatures runF =
    case runF of
      ScoreF -> -1000.0
      RecipRankF -> 0.0
      LinearRankF -> 0.0
      BucketRankF -> 0.0
      CountF -> 0.0

defaultRankFeatures' :: Run -> [(Feature ty, Double)]
defaultRankFeatures' run =
    [ (RetrievalFeature run runF, defaultRankFeatures runF)
    | runF <- enumAll
    ]


score :: RankingEntry d -> Double
score entry  = CAR.RunFile.carScore entry

recipRank :: RankingEntry d  -> Double
recipRank entry = 1.0/ (1.0 + realToFrac rank)
  where rank = CAR.RunFile.carRank entry

linearRank :: Int -> RankingEntry d  -> Double
linearRank maxLen entry
    | rank > maxLen = 0.0
    | otherwise = realToFrac $ maxLen - rank
  where rank = CAR.RunFile.carRank entry

bucketRank :: RankingEntry d  -> Double
bucketRank entry
    | rank >= 5 = 3.0
    | rank >= 20 = 2.0
    | otherwise = 1.0
  where rank = CAR.RunFile.carRank entry


count :: RankingEntry d -> Double
count _ = 1.0



rankFeatures :: RunFeature -> RankingEntry d -> Double
rankFeatures runF entry =
    case runF of
      ScoreF -> score entry
      RecipRankF -> recipRank entry
      LinearRankF -> linearRank 100  entry
      BucketRankF -> bucketRank entry
      CountF -> count entry

rankFeatures' :: Run -> RankingEntry d -> [(Feature ty, Double)]
rankFeatures' run entry =
    [ (RetrievalFeature run runF, rankFeatures runF entry)
    | runF <- enumAll
    ]




generateEntityFeatures
    :: EdgeDocsLookup
    -> (PageId -> [EdgeDoc] -> MultiRankingEntry PageId -> [MultiRankingEntry ParagraphId] -> FeatureVec CombinedFeatures Double)
    -> QueryId
    -> [MultiRankingEntry ParagraphId]
    -> [MultiRankingEntry PageId]
    -> [((QueryId, PageId), (FeatureVec CombinedFeatures Double))]
generateEntityFeatures edgeDocsLookup featuresOf' query edgeRun entityRun =
    let paraIdToEdgedocRun = HM.fromList [ (multiRankingEntryGetDocumentName run, run) | run <- edgeRun]
        edgeDocs = edgeDocsLookup $ HM.keys paraIdToEdgedocRun

        universalGraph = edgeDocsToUniverseGraph edgeDocs


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
           -> FeatureVec CombinedFeatures Double
featuresOf entity edgeDocs entityRankEntry edgedocsRankEntries =

    let
        indicentEdgeDocs = realToFrac $ length edgeDocs
        degree =  realToFrac $ HS.size $ foldl1' HS.union $ fmap edgeDocNeighbors edgeDocs

        recipRank rank =  1.0/ (1.0 + realToFrac rank)

        edgeDocsByPara = HM.fromList [ (edgeDocParagraphId edgeDoc, edgeDoc )
                                    | edgeDoc <- edgeDocs
                                    ]

        entityScoreVec entityEntry = makeEntFeatVector  (
                                            [ (EntIncidentEdgeDocsRecip, recip indicentEdgeDocs)
                                            , (EntDegreeRecip, recip degree)
                                            , (EntDegree, degree)
                                            ]
                                            ++ rankFeatures' Aggr (multiRankingEntryCollapsed entityEntry)
                                            ++ concat (catMaybes
                                            [ rankFeatures' (GridRun' $ GridRun All Bm25 NoneX EntityIdx) <$> (findEntry' bm25MethodName entityEntry)
                                            , rankFeatures' (GridRun' $ GridRun All Ql NoneX EntityIdx) <$> (findEntry' qlMethodName entityEntry)
                                            ] )
                                           )

        edgeScoreVec edgeEntry = makeEdgeFeatVector $
                                            [ (EdgeCount, 1.0)
                                            , (EdgeDocKL, (edgeDocKullbackLeibler edgeDocs ) ( fromJust $ (multiRankingEntryGetDocumentName edgeEntry) `HM.lookup` edgeDocsByPara) )
                                            ]
                                            ++ rankFeatures' Aggr (multiRankingEntryCollapsed edgeEntry)
                                            ++ concat (catMaybes
                                            [ rankFeatures' (GridRun' $ GridRun All Bm25 NoneX EntityIdx) <$> (findEntry' bm25MethodName edgeEntry)
                                            , rankFeatures' (GridRun' $ GridRun All Ql NoneX EntityIdx) <$> (findEntry' qlMethodName edgeEntry)
                                            ] )


    in concatFeatureVec (entityScoreVec entityRankEntry) (F.aggregateWith (+) (fmap edgeScoreVec edgedocsRankEntries))
  where
        findEntry' :: CarRun.MethodName -> MultiRankingEntry d  -> Maybe (RankingEntry d)
        findEntry' methodName entry =
            findEntry methodName $ multiRankingEntryAll entry


        findEntry :: CarRun.MethodName -> [RankingEntry doc] -> Maybe (RankingEntry doc)
        findEntry method entries = find  ((== method). CarRun.carMethodName)  entries

        edgeDocKullbackLeibler :: [EdgeDoc] -> EdgeDoc -> Double
        edgeDocKullbackLeibler otherEdgeDocsOfConnectedEntities edgeDoc =

          let (backTermCounts, backTotal) =
                termCountsAndTotal
                $ fmap edgeDocContent
                $ HS.toList $ HS.fromList
                $ otherEdgeDocsOfConnectedEntities

              (termCounts, total) =
                termCountsAndTotal [(edgeDocContent edgeDoc)]
          in kullbackLeibler (termCounts, total) (backTermCounts, backTotal)


        kullbackLeibler :: (TermCounts, Int) -> (TermCounts, Int) -> Double
        kullbackLeibler (termCounts, total) (backTermCounts, backTotal) =
                            Foldable.sum $ [ pi * (log pi - log qi)
                                            | (term,count) <- HM.toList (getTermCounts termCounts)
                                            , let pi = (realToFrac count) / (realToFrac total)
                                            , let bcount = fromJust $ term `HM.lookup` (getTermCounts backTermCounts)
                                            , let qi = (realToFrac bcount) / (realToFrac backTotal)
                                            ]



        termCountsAndTotal :: [T.Text] -> (Retrieve.TermCounts, Int)
        termCountsAndTotal texts =
                              let termCounts =
                                    foldMap (textToTokens) texts
                                  total = Foldable.sum $ HM.elems $ getTermCounts $ termCounts
                              in (termCounts, total)





textToTokens :: T.Text -> Retrieve.TermCounts
textToTokens = foldMap Retrieve.oneTerm . Retrieve.textToTokens'




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


