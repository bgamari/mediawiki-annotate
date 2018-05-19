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

import Control.Concurrent.Async
import Data.Ord
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
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Foldable as Foldable
import Data.Function
import Data.Bifunctor
import Data.Hashable


import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.TocFile as Toc


import EdgeDocCorpus
import GraphExpansionExperiments hiding (Bm25, Ql)
import GraphExpansion hiding (RetrievalFun, Bm25, Ql)
import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)


import qualified CAR.RunFile as CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import MultiTrecRunFile
import PageRank
import DenseMapping
import Graph


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

data ExperimentSettings = AllExp | NoEdgeFeats | NoEntityFeats | AllEdgeWeightsOne | JustAggr | JustScore | JustRecip
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PageRankExperimentSettings = PageRankNormal | PageRankJustStructure | PageRankWeightOffset1 | PageRankWeightOffset01
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PageRankConvergence = L2Convergence | Iteration10 | Iteration2
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PosifyEdgeWeights = Exponentiate | ExpDenormWeight | Linear | Logistic | CutNegative
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data GraphWalkModel = PageRankWalk | BiasedPersPageRankWalk
  deriving (Show, Read, Ord, Eq, Enum, Bounded)



-- GridRun  QueryModel RetrievalModel ExpansionModel IndexType
gridRunParser :: Parser (GridRun, EntityOrEdge, FilePath)
gridRunParser = option (str >>= parseGridRunFile) (long "grid-run")
  where
    parseGridRunFile :: String -> ReadM (GridRun, EntityOrEdge, FilePath)
    parseGridRunFile s
      | a:b:c:d:e:rest <- words s
      = do !a' <- safeRead "QueryModel" a
           !b' <- safeRead "RetrievalModel" b
           !c' <- safeRead "ExpansionModel" c
           !d' <- safeRead "IndexType" d
           !e' <- safeRead "EntityOrEdge" e
           return (GridRun a' b' c' d', e', unwords rest)
      | otherwise
      = fail $ "Failed to tokenise: " ++ s
      where
        safeRead :: Read a => String -> String -> ReadM a
        safeRead thing s'
          | (x,""):_ <- reads s' = return x
          | otherwise = fail $ "failed to parse "++thing++": "++s'

-- | PageRank teleportation \(\alpha\)
type TeleportationProb = Double

opts :: Parser ( FilePath
               , FilePath
               , QuerySource
               , [CarRun.QueryId]
               , NumResults
               , [(GridRun, EntityOrEdge, FilePath)]
               , Toc.IndexedCborPath ParagraphId EdgeDoc
               , FilePath
               , ModelSource
               , PosifyEdgeWeights
               , TeleportationProb
               , [ExperimentSettings]
               , PageRankExperimentSettings
               , PageRankConvergence
               , GraphWalkModel
               )
opts =
    (,,,,,,,,,,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "number of results per query")
    <*> some gridRunParser
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "edge-doc-cbor" <> metavar "EdgeDoc-CBOR" <> help "EdgeDoc cbor file"))
    <*> (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> modelSource
    <*> option auto (long "posify" <> metavar "OPT" <> help ("Option for how to ensure positive edge weights. Choices: " ++(show [minBound @PosifyEdgeWeights .. maxBound])) <> value Exponentiate)
    <*> option auto (long "teleport" <> help "teleport probability (for page rank)" <> value 0.1)
    <*> many (option auto (long "exp" <> metavar "EXP" <> help ("one or more switches for experimentation. Choices: " ++(show [minBound @ExperimentSettings .. maxBound]))))
    <*> option auto (long "pagerank-settings" <> metavar "PREXP" <> help ("Option for how to ensure positive edge weights. Choices: " ++(show [PageRankNormal,PageRankJustStructure,  PageRankWeightOffset1, PageRankWeightOffset01])) <> value PageRankNormal)
    <*> option auto (long "pagerank-convergence" <> metavar "CONV" <> help ("How pagerank determines convergence. Choices: " ++(show [minBound @PageRankConvergence .. maxBound])) <> value Iteration10)
    <*> option auto (long "graph-walk-model" <> metavar "PAGERANK" <> help ("Graph walk model. Choices: " ++(show [minBound @GraphWalkModel .. maxBound])) <> value PageRankWalk)
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
        <|> option (ModelFromFile <$> str) (long "read-model" <> metavar "Model-FILE" <> help "read learning-to-rank model from Model-FILE")


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

type TrainData =  M.Map CAR.RunFile.QueryId [(QRel.DocumentName, Features, IsRelevant)]
type ReturnWithModelDiagnostics a = (a, [(String, Model, Double)])


main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    (articlesFile, outputFilePrefix, querySrc,
      queryRestriction, numResults, gridRunFiles
      , edgeDocsCborFile
      , qrelFile, modelSource
      , posifyEdgeWeightsOpt,  teleportation, experimentSettings
      , pageRankExperimentSettings, pageRankConvergence, graphWalkModel  ) <- execParser' 1 (helper <*> opts) mempty
    putStrLn $ "# Pages: " ++ show articlesFile
    siteId <- wikiSite . fst <$> readPagesFileWithProvenance articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    let entityRunFiles  = [ (g, r) | (g, Entity, r) <- gridRunFiles]
        edgedocRunFiles = [ (g, r) | (g, Edge, r) <- gridRunFiles]

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ))
    putStrLn $ "# EdgeDoc runs: "++ ( show $ fmap (show) (edgedocRunFiles))

    putStrLn $ " Experimentation settins: "++ (show experimentSettings)
    putStrLn $ " model comes from : "++ (show modelSource)
    putStrLn $ " teleport (only for page rank) : "++ (show teleportation)
    putStrLn $ " posify with (only for page rank) : "++ (show posifyEdgeWeightsOpt)
    putStrLn $ " pageRankExperimentSettings (only for page rank) : "++ (show pageRankExperimentSettings)
    putStrLn $ " graphWalkModel (only for page rank) : "++ (show graphWalkModel)

    gen0 <- newStdGen  -- needed by learning to rank

    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs siteId queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries

    let combinedFSpace' = mkFeatureSpace
                          $ filter (expSettingToCrit experimentSettings)
                          $ F.featureNames combinedFSpace  -- Todo this is completely unsafe

    let fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile

    queries <-
        if null queryRestriction
          then return queries'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queries'
    putStrLn $ "# query count: " ++ show (length queries)

    edgeDocsLookup <- readEdgeDocsToc edgeDocsCborFile


    entityRuns <-  mapM (mapM CAR.RunFile.readEntityRun) entityRunFiles  -- mapM mapM -- first map over list, then map of the snd of a tuple
    edgeRuns <-  mapM (mapM CAR.RunFile.readParagraphRun) edgedocRunFiles

    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
        collapsedEntityRun = collapseRuns entityRuns
        collapsedEdgedocRun = collapseRuns edgeRuns

        tr x = traceShow x x

    -- predict mode
    -- alternative: load model from disk, then use graph feature vectors to produce a graph with edge weights (Graph PageId Double)
    -- use pagerank on this graph to predict an alternative node ranking
    -- save predicted node ranking as run-file
    case modelSource of
      ModelFromFile modelFile -> do
          Just model <-  trace "loading model" $ Data.Aeson.decode @Model <$> BSL.readFile modelFile

          let docFeatures :: M.Map (QueryId, QRel.DocumentName) CombinedFeatureVec
              docFeatures = makeStackedFeatures edgeDocsLookup collapsedEntityRun collapsedEdgedocRun combinedFSpace' experimentSettings
              degreeCentrality = fmap (modelWeights `F.dotFeatureVecs`) docFeatures
                where modelWeights = modelToFeatureVec combinedFSpace' model
              queryToScoredList = M.fromListWith (<>) [(q, [(d, score)]) | ((q,d), score) <- M.toList degreeCentrality ]
              ranking :: M.Map QueryId (Ranking.Ranking Double QRel.DocumentName)
              ranking = fmap (Ranking.fromList . map swap) queryToScoredList

              rankingPageId :: M.Map QueryId (Ranking.Ranking Double PageId)
              rankingPageId = fmap (fmap qrelDocNameToPageId) ranking

              nodeDistr :: M.Map QueryId (HM.HashMap PageId Double) -- only positive entries, expected to sum to 1.0
              nodeDistr = fmap nodeRankingToDistribution rankingPageId

          let edgeFSpace' = mkFeatureSpace
                              $ tr
                              $  filter (expSettingToCritEdge experimentSettings)
                              $ F.featureNames edgeFSpace  -- Todo this is completely unsafe

          let params :: EdgeFeatureVec
              params = tr $ F.fromList edgeFSpace' -- TODO: Use modelToFeatureVec
                  [ (k'', v)
                  | (k, v) <- M.toList $ modelWeights model
                  , let k' = read $ T.unpack $ getFeatureName k :: Either EntityFeature EdgeFeature
                  , Right k'' <- pure k'
                  ]

          let graphWalkRanking :: QueryId -> Ranking.Ranking Double PageId
              graphWalkRanking query
                 | any (< 0) graph' = error ("negative entries in graph' for query "++ show query ++ ": "++ show (count (< 0) graph'))
                 | otherwise = traceShow (toEntries eigv) $ Ranking.fromList $ map swap $ toEntries eigv
                where
                  count pred = getSum . foldMap f
                    where f x = if pred x then Sum 1 else Sum 0

                  candidates = selectCandidateGraph edgeDocsLookup query edgeRun entityRun
                    where
                      edgeRun = collapsedEdgedocRun M.! query
                      entityRun = collapsedEntityRun M.! query

                  -- TODO: very unsafe
--                   params' = F.unsafeFeatureVecFromVector $ getFeatures
--                             $ Features $ F.getFeatureVec params
                  params' = params

                  graph :: Graph PageId EdgeFeatureVec
                  graph =  fmap (filterExpSettingsEdge edgeFSpace edgeFSpace' (expSettingToCritEdge experimentSettings))
                         $ generateEdgeFeatureGraph query candidates -- edgeDocsLookup query edgeRun entityRun


                  normalizer :: Normalization
                  normalizer = zNormalizer $ map (Features . F.getFeatureVec) $ Foldable.toList graph


                  graph' :: Graph PageId Double
                  graph' = fmap (posifyDot pageRankExperimentSettings posifyEdgeWeightsOpt normalizer params' (Foldable.toList graph)) graph
                  -- for debugging...
--                   graph' = fmap (\feats -> trace (show feats) ( tr  ( posifyDot params' feats))) graph
                    where
                      normFeats :: EdgeFeatureVec -> EdgeFeatureVec
                      normFeats fv =
                          let v :: VU.Vector Double
                              v = F.getFeatureVec fv
                              normedF = normFeatures normalizer (Features v)
                              normedV :: VU.Vector Double
                              normedV =  getFeatures normedF
                          in F.unsafeFeatureVecFromVector normedV

                  eigv :: Eigenvector PageId Double
                  eigv =
                       let pageRankIters = zip walkIters (tail walkIters)

                       in case pageRankConvergence of
                            L2Convergence -> snd
                                           $ head
                                           $ dropWhile (\(x,y) -> relChange x y > 1e-4)
                                           $ pageRankIters
                            Iteration10   -> snd $ (!! 10)  pageRankIters
                            Iteration2    -> snd $ (!! 2)  pageRankIters
                  walkIters :: [Eigenvector PageId Double]
                  walkIters = case graphWalkModel of
                                PageRankWalk -> pageRank teleportation graph'
                                BiasedPersPageRankWalk -> persPageRankWithNonUniformSeeds 0 seedNodeDistr graph'
                                  where  betaTotal = teleportation
                                         seedNodeDistr = fmap (* betaTotal) (nodeDistr M.! query )



              runRanking query = do
                  let ranking = graphWalkRanking query
                      rankEntries =  [ CAR.RunFile.RankingEntry query pageId rank score (CAR.RunFile.MethodName "PageRank")
                                    | (rank, (score, pageId)) <- zip [1..] (Ranking.toSortedList ranking)
                                    ]

                  CAR.RunFile.writeEntityRun  (outputFilePrefix ++ "-"++ T.unpack (CAR.RunFile.unQueryId query) ++"-pagerank-test.run")
                                    $ rankEntries
          mapConcurrently_(runRanking . queryDocQueryId) queries

      TrainModel modelFile -> do
          let docFeatures = fmap featureVecToFeatures
                          $ makeStackedFeatures edgeDocsLookup collapsedEntityRun collapsedEdgedocRun combinedFSpace' experimentSettings

--               docFeatures' = fmap (Features . F.toVector) docFeatures''
--               normalizer = zNormalizer $ M.elems docFeatures'
--               docFeatures = fmap (normFeatures normalizer) docFeatures'

              featureNames :: _
              featureNames = fmap (FeatureName . T.pack . show) $ F.featureNames combinedFSpace'

              franking :: TrainData
              franking = augmentWithQrels qrel docFeatures Relevant

              -- Option a) drop in an svmligh style features annsFile
              -- Option b) stick into learning to rank
              discardUntrainable :: TrainData -> TrainData
              discardUntrainable franking =
                  M.filter hasPosAndNeg  franking
                where hasPosAndNeg list =
                        let hasPos = any (\(_,_,r) -> r == Relevant) list
                            hasNeg = any (\(_,_,r) -> r /= Relevant) list
                        in hasPos && hasNeg

              trainData :: TrainData
              trainData = discardUntrainable franking
              metric = avgMetricQrel qrel
              totalElems = getSum . foldMap ( Sum . length ) $ trainData
              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ trainData

          putStrLn $ "Training model with (trainData) "++ show (M.size trainData) ++
                    " queries and "++ show totalElems ++" items total of which "++
                    show totalPos ++" are positive."

          let displayTrainData :: TrainData
                               -> [String]
              displayTrainData trainData =
                [ show k ++ " -> "++ show elem
                | (k,list) <- M.toList trainData
                , elem <- list]

          putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData trainData)


          let trainProcedure:: String -> TrainData -> ReturnWithModelDiagnostics (Model, Double)
              trainProcedure modelDesc trainData =
                  let trainWithDifferentGens :: StdGen -> Int -> (StdGen, ReturnWithModelDiagnostics (Model, Double))
                      trainWithDifferentGens gen i =
                                let (genA, genB) = System.Random.split gen
                                in (genA, restartModel i genB)
                        where restartModel i gen =
                                let (model, trainScore) = learnToRank trainData featureNames metric gen
                                in ((model, trainScore), [((modelDesc ++ "-restart-"++show i), model, trainScore)])


                      other:: [ReturnWithModelDiagnostics (Model, Double)]
                      (_, other) = mapAccumL trainWithDifferentGens gen0 [0..4]
                      modelsWithTrainScore:: [(Model, Double)]
                      (modelsWithTrainScore, modelDiag) = unzip other
                      (model, trainScore) = maximumBy (compare `on` snd) modelsWithTrainScore
                  in ((model, trainScore), [(modelDesc ++"-best", model, trainScore)] ++ concat modelDiag)


          let outputDiagnostics :: (String, Model, Double) -> IO ()
              outputDiagnostics (modelDesc,model, trainScore) = do
                      storeModelData outputFilePrefix modelFile model trainScore modelDesc
                      storeRankingData outputFilePrefix franking metric model modelDesc

              modelDiag :: [(String, Model, Double)]
              ((model, trainScore), modelDiag) = trainProcedure "train" trainData

            -- todo  exportGraphs model

                                -- todo load external folds
          let folds = chunksOf foldLen $ M.keys trainData
                      where foldLen = ((M.size trainData) `div` 5 ) +1

--         trainProcedure trainData = learnToRank trainData featureNames metric gen0
              (predictRanking, modelDiag') = kFoldCross trainProcedure folds trainData franking
              testScore = metric predictRanking

          -- evaluate all work here!
          mapConcurrently_ outputDiagnostics $ modelDiag ++ modelDiag'

                -- eval and write train ranking (on all data)
          storeModelData outputFilePrefix modelFile model trainScore "train"
          storeRankingData outputFilePrefix franking metric model "train"

          putStrLn $ "K-fold cross validation score " ++ (show testScore)++"."
          -- write test ranking that results from k-fold cv
          CAR.RunFile.writeEntityRun (outputFilePrefix++"-test.run")
              $ l2rRankingToRankEntries (CAR.RunFile.MethodName "l2r test")
              $ predictRanking


qrelDocNameToPageId :: QRel.DocumentName -> PageId
qrelDocNameToPageId docname = packPageId $ T.unpack docname

makeStackedFeatures :: EdgeDocsLookup
                    ->  M.Map QueryId [MultiRankingEntry PageId GridRun]
                    ->  M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
                    ->  FeatureSpace CombinedFeature
                    -> [ExperimentSettings]
                    ->  M.Map (QueryId, QRel.DocumentName) CombinedFeatureVec
makeStackedFeatures edgeDocsLookup collapsedEntityRun collapsedEdgedocRun combinedFSpace' experimentSettings =
    let
        docFeatures''' :: M.Map (QueryId, QRel.DocumentName) CombinedFeatureVec
        docFeatures''' = M.fromList
                    [ ((qid, T.pack $ unpackPageId pid), features)
                    | (query, edgeRun) <- M.toList collapsedEdgedocRun
                    , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
                    , let candidates = selectCandidateGraph edgeDocsLookup query edgeRun entityRun
                    , ((qid, pid), features) <- HM.toList $ combineEntityEdgeFeatures query candidates
                    ]

        docFeatures'' = fmap crit docFeatures'''
                        where crit = filterExpSettings combinedFSpace combinedFSpace' (expSettingToCrit experimentSettings)

        docFeatures' = fmap (Features . F.toVector) docFeatures''
        normalizer = zNormalizer $ M.elems docFeatures'
        docFeatures = fmap (normFeatures normalizer) docFeatures'

    in fmap featuresToFeatureVec docFeatures


logistic :: Double -> Double
logistic t =
    1.0 / (1.0 + exp (-1 * t))

-- -------------------------------------------
--   Learning to Rank, k-fold Cross, restarts
-- -------------------------------------------

l2rRankingToRankEntries :: CAR.RunFile.MethodName
                        -> M.Map CAR.RunFile.QueryId (Ranking SimplIR.LearningToRank.Score (QRel.DocumentName, rel))
                        -> [CAR.RunFile.EntityRankingEntry]
l2rRankingToRankEntries methodName rankings =
  [ CAR.RunFile.RankingEntry { carQueryId = query
                             , carDocument = packPageId $ T.unpack doc
                             , carRank = rank
                             , carScore = score
                             , carMethodName = methodName
                             }
  | (query, ranking) <- M.toList rankings
  , ((score, (doc, rel)), rank) <- Ranking.toSortedList ranking `zip` [1..]
  ]


-- Train model on all data
storeModelData :: FilePath
               -> FilePath
               -> Model
               -> Double
               -> [Char]
               -> IO ()
storeModelData outputFilePrefix modelFile model trainScore modelDesc = do
  putStrLn $ "Model "++modelDesc++ " train metric "++ (show trainScore) ++ " MAP."
  let modelFile' = outputFilePrefix++modelFile++"-model-"++modelDesc++".json"
  BSL.writeFile modelFile' $ Data.Aeson.encode model
  putStrLn $ "Written model "++modelDesc++ " to file "++ (show modelFile') ++ " ."

storeRankingData ::  FilePath
               -> TrainData
               -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
               -> Model
               -> [Char]
               -> IO ()
storeRankingData outputFilePrefix franking metric model modelDesc = do

  let rerankedFranking = rerankRankings' model franking
  putStrLn $ "Model "++modelDesc++" test metric "++ show (metric rerankedFranking) ++ "MAP."
  CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
       $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
       $ rerankedFranking

kFoldCross :: forall q docId rel. (Ord q, Show q)
           => (String -> M.Map q [(docId, Features, rel)] -> ReturnWithModelDiagnostics (Model, Double))
           -> [[q]]
           -> M.Map q [(docId, Features, rel)]
           -> M.Map q [(docId, Features, rel)]
            -- -> ML.Map q (Ranking (docId, rel))
           -> ReturnWithModelDiagnostics (ML.Map q (Ranking SimplIR.LearningToRank.Score (docId, rel)))
kFoldCross trainProcedure folds allTrainData allTestData =
    let (result, modelDiag) = unzip $ fmap (\(fidx, queries) -> trainSingleFold fidx queries) $ zip [0 .. ] folds
    in (M.unions result, concat modelDiag)
  where
    trainSingleFold :: Int -> [q]  -> ReturnWithModelDiagnostics (M.Map q (Ranking SimplIR.LearningToRank.Score (docId, rel)))
    trainSingleFold foldIdx testQueries =
      let testData :: M.Map q [(docId, Features, rel)]
          testData =  M.filterWithKey (\query _ -> query `elem` testQueries) allTestData
          trainData :: M.Map q [(docId, Features, rel)]
          trainData =  M.filterWithKey (\query _ -> query `notElem` testQueries) allTrainData

          foldId = show foldIdx
          ((model, trainScore), modelDiag) = trainProcedure ("fold-"++foldId) trainData
          testRanking :: M.Map q (Ranking SimplIR.LearningToRank.Score (docId, rel))
          testRanking = rerankRankings' model testData
      in (testRanking, modelDiag)

changeKey :: Ord k' => (k-> k') -> M.Map k v -> M.Map k' v
changeKey f map_ =
    M.fromList $ fmap (\(key,val) -> (f key, val)) $ M.toList map_



-- -------------------------------------------
-- the feature space
-- -------------------------------------------

data QueryModel = All | Title
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data RetrievalModel = Bm25 | Ql
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data ExpansionModel = NoneX | Rm | EcmX | EcmRm
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)
data IndexType = EcmIdx | EntityIdx | PageIdx | ParagraphIdx
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise, Hashable)

entityRunsF :: [GridRun]
entityRunsF = [ GridRun qm rm em it
             | qm <- [minBound..maxBound]
             , rm <- [minBound..maxBound]
             , (it, em) <- [ (EcmIdx, EcmX), (EcmIdx, EcmRm)
                           , (ParagraphIdx, EcmX), (ParagraphIdx, EcmRm)
                           , (PageIdx, NoneX), (PageIdx, Rm)]
                           ++ [(EntityIdx, em) | em <- [minBound..maxBound]]
             ]


edgeRunsF :: [GridRun]
edgeRunsF = [ GridRun qm rm em it
             | qm <- [minBound..maxBound]
             , rm <- [minBound..maxBound]
             , (it, em) <- [ (EcmIdx, NoneX), (EcmIdx, Rm)
                           , (ParagraphIdx, NoneX), (ParagraphIdx, Rm)
                           ]
             ]



data GridRun = GridRun QueryModel RetrievalModel ExpansionModel IndexType
         deriving (Show, Read, Ord, Eq, Generic, Serialise, Hashable)

data Run = GridRun' GridRun | Aggr
         deriving (Show, Read, Ord, Eq, Generic, Serialise)
allEntityRunsF = (GridRun' <$> entityRunsF) <> [Aggr]
allEdgeRunsF = (GridRun' <$> edgeRunsF) <> [Aggr]

data RunFeature = ScoreF | RecipRankF | LinearRankF | BucketRankF | CountF
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

allRunFeatures :: [RunFeature]
allRunFeatures = [ScoreF] --[minBound..maxBound]

data EntityFeature where
    EntRetrievalFeature :: Run -> RunFeature -> EntityFeature
    EntIncidentEdgeDocsRecip :: EntityFeature
    EntDegreeRecip :: EntityFeature
    EntDegree  :: EntityFeature
    deriving (Show, Read, Ord, Eq)

data EdgeFeature where
    EdgeRetrievalFeature :: Run -> RunFeature -> EdgeFeature
    EdgeDocKL  :: EdgeFeature
    EdgeCount  :: EdgeFeature
    deriving (Show, Read, Ord, Eq)

data EntityOrEdge = Entity | Edge
         deriving (Show, Read, Ord, Eq, Enum, Bounded, Generic, Serialise)

type CombinedFeature = Either EntityFeature EdgeFeature

allEntityFeatures :: [EntityFeature]
allEntityFeatures =
    (EntRetrievalFeature <$> allEntityRunsF <*> allRunFeatures)
    <> [EntIncidentEdgeDocsRecip, EntDegreeRecip, EntDegree]


allEdgeFeatures :: [EdgeFeature]
allEdgeFeatures =
    (EdgeRetrievalFeature <$> allEdgeRunsF <*> allRunFeatures)
    <> [EdgeDocKL, EdgeCount]

entFSpace :: FeatureSpace EntityFeature
entFSpace = mkFeatureSpace allEntityFeatures

edgeFSpace :: FeatureSpace EdgeFeature
edgeFSpace = mkFeatureSpace allEdgeFeatures

combinedFSpace :: FeatureSpace CombinedFeature
combinedFSpace = concatSpace entFSpace edgeFSpace




-- -------------------------------------------
-- filtering of feature spaces
-- -------------------------------------------


filterExpSettingsEdge ::  FeatureSpace EdgeFeature
                  ->  FeatureSpace EdgeFeature
                  -> (EdgeFeature -> Bool)
                  ->  (FeatureVec EdgeFeature Double)
                  ->  (FeatureVec EdgeFeature Double)
filterExpSettingsEdge fromFeatSpace toFeatSpace pred features =
    F.fromList toFeatSpace
    $ [ pair
      | pair@(fname, _) <- F.toList fromFeatSpace features
      , pred fname
      ]

onlyAggrEdge :: EdgeFeature -> Bool
onlyAggrEdge (EdgeRetrievalFeature Aggr runf) = True
onlyAggrEdge _  = False

onlyScoreEdge :: EdgeFeature -> Bool
onlyScoreEdge (EdgeRetrievalFeature _ ScoreF) = True
onlyScoreEdge _  = False

onlyRREdge :: EdgeFeature -> Bool
onlyRREdge (EdgeRetrievalFeature _ RecipRankF) = True
onlyRREdge _  = False

expSettingToCritEdge :: [ExperimentSettings] ->  (EdgeFeature -> Bool)
expSettingToCritEdge exps fname =
    all (`convertEdge` fname) exps

convertEdge :: ExperimentSettings -> (EdgeFeature -> Bool)
convertEdge exp = case exp of
                AllExp -> const True
                NoEdgeFeats -> const False
                NoEntityFeats -> const True
                AllEdgeWeightsOne -> const True -- needs to be handled elsewhere
                JustAggr -> onlyAggrEdge
                JustScore -> onlyScoreEdge
                JustRecip -> onlyRREdge

filterExpSettings ::  FeatureSpace CombinedFeature
                  ->  FeatureSpace CombinedFeature
                  -> (CombinedFeature -> Bool)
                  ->  (FeatureVec CombinedFeature Double)
                  ->  (FeatureVec CombinedFeature Double)
filterExpSettings fromFeatSpace toFeatSpace pred features =
    F.fromList toFeatSpace
    $ [ pair
      | pair@(fname, _) <- F.toList fromFeatSpace features
      , pred fname
      ]
noEntity :: CombinedFeature -> Bool
noEntity (Left _) = False
noEntity _  = True

noEdge :: CombinedFeature -> Bool
noEdge (Right _) = False
noEdge _  = True

onlyAggr :: CombinedFeature -> Bool
onlyAggr (Left (EntRetrievalFeature Aggr runf)) = True
onlyAggr (Right (EdgeRetrievalFeature Aggr runf)) = True
onlyAggr _  = False

onlyScore :: CombinedFeature -> Bool
onlyScore (Left (EntRetrievalFeature _ ScoreF)) = True
onlyScore (Right (EdgeRetrievalFeature _ ScoreF)) = True
onlyScore _  = False

onlyRR :: CombinedFeature -> Bool
onlyRR (Left (EntRetrievalFeature _ RecipRankF)) = True
onlyRR (Right (EdgeRetrievalFeature _ RecipRankF)) = True
onlyRR _  = False

expSettingToCrit :: [ExperimentSettings] ->  (CombinedFeature -> Bool)
expSettingToCrit exps fname =
    all (`convert` fname) exps
  where
    convert :: ExperimentSettings -> (CombinedFeature -> Bool)
    convert exp = case exp of
                    AllExp -> const True
                    NoEdgeFeats -> noEdge
                    NoEntityFeats -> noEntity
                    AllEdgeWeightsOne -> const True -- needs to be handled elsewhere
                    JustAggr -> onlyAggr
                    JustScore -> onlyScore
                    JustRecip -> onlyRR



-- -------------------------------------------
-- make feature vectors with defaults and stuff
-- -------------------------------------------

type EdgeFeatureVec = FeatureVec EdgeFeature Double
type EntityFeatureVec = FeatureVec EntityFeature Double
type CombinedFeatureVec = FeatureVec CombinedFeature Double

makeEntFeatVector :: [(EntityFeature, Double)] -> F.FeatureVec EntityFeature Double
makeEntFeatVector xs =
    F.modify entFSpace defaults xs
 where defaults = F.fromList entFSpace ([ (EntIncidentEdgeDocsRecip, 0.0)
                                       , (EntDegreeRecip, 0.0)
                                       , (EntDegree, 0.0)
                                       ]
                                       ++ [ feat
                                          | entityRun <- allEntityRunsF
                                          , feat <- defaultEntRankFeatures entityRun
                                          ]
                                       )

makeEdgeFeatVector :: [(EdgeFeature, Double)] -> F.FeatureVec EdgeFeature Double
makeEdgeFeatVector xs =
    F.modify edgeFSpace defaults xs
 where defaults = F.fromList edgeFSpace ([ (EdgeCount, 0.0)
                                         , (EdgeDocKL, 0.0)
                                         ]
                                        ++ [ feat
                                           | edgeRun <- allEdgeRunsF
                                           , feat <- defaultEdgeRankFeatures edgeRun
                                           ]
                                        )

defaultRankFeatures :: RunFeature -> Double
defaultRankFeatures runF =
    case runF of
      ScoreF -> -1000.0
      RecipRankF -> 0.0
      LinearRankF -> 0.0
      BucketRankF -> 0.0
      CountF -> 0.0

defaultEntRankFeatures :: Run -> [(EntityFeature, Double)]
defaultEntRankFeatures run =
    [ (EntRetrievalFeature run runF, defaultRankFeatures runF)
    | runF <- allRunFeatures
    ]

defaultEdgeRankFeatures :: Run -> [(EdgeFeature, Double)]
defaultEdgeRankFeatures run =
    [ (EdgeRetrievalFeature run runF, defaultRankFeatures runF)
    | runF <- allRunFeatures
    ]

rankFeatures :: RunFeature -> RankingEntry d -> Double
rankFeatures runF entry =
    case runF of
      ScoreF -> score entry
      RecipRankF -> recipRank entry
      LinearRankF -> linearRank 100  entry
      BucketRankF -> bucketRank entry
      CountF -> count entry
  where
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

rankEntFeatures :: Run -> RankingEntry d -> [(EntityFeature, Double)]
rankEntFeatures run entry =
    [ (EntRetrievalFeature run runF, rankFeatures runF entry)
    | runF <- allRunFeatures
    ]

rankEdgeFeatures :: Run -> RankingEntry d -> [(EdgeFeature, Double)]
rankEdgeFeatures run entry =
    [ (EdgeRetrievalFeature run runF, rankFeatures runF entry)
    | runF <- allRunFeatures
    ]

generateEdgeFeatureGraph:: QueryId
                        -> Candidates
--                         -> [MultiRankingEntry ParagraphId GridRun]
--                         -> [MultiRankingEntry PageId GridRun]
                        -> Graph PageId EdgeFeatureVec
generateEdgeFeatureGraph query cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                               , candidateEdgeRuns = edgeRun
                                               , candidateEntityRuns = _entityRun} = -- edgeDocsLookup query edgeRun entityRun =
    let
        edgeDocsLookup = wrapEdgeDocsTocs $ HM.fromList $ [ (edgeDocParagraphId edgeDoc, edgeDoc) | edgeDoc <- allEdgeDocs]
        edgeDoc paraId = case edgeDocsLookup [paraId] of
                           [] -> error $ "No edgedoc for paraId "++show paraId
                           (a:_) -> a

        edgeFeat :: ParagraphId -> _
        edgeFeat paraId edgeEntry = edgeScoreVec edgeEntry

        divideEdgeFeats feats cardinality = F.scaleFeatureVec (1 / (realToFrac cardinality)) feats
        edgeCardinality ed = HS.size $ edgeDocNeighbors ed

        aggrFeatVecs :: EdgeFeatureVec -> EdgeFeatureVec -> EdgeFeatureVec
        aggrFeatVecs features1 features2 =
            F.aggregateWith (+) [features1, features2]

        oneHyperEdge :: (ParagraphId, MultiRankingEntry ParagraphId GridRun)
                     -> [((PageId, PageId), EdgeFeatureVec)]
        oneHyperEdge (paraId, edgeEntry) =
              [ ((u, v) , dividedFeatVec)
              | u <- HS.toList $ edgeDocNeighbors (edgeDoc paraId)
              , v <- HS.toList $ edgeDocNeighbors (edgeDoc paraId) -- include self links (v==u)!
              , let featVec = edgeFeat paraId edgeEntry
              , let dividedFeatVec = divideEdgeFeats featVec (edgeCardinality (edgeDoc paraId))
              ]

        allHyperEdges :: HM.HashMap (PageId, PageId) EdgeFeatureVec
        allHyperEdges = HM.fromListWith aggrFeatVecs
                      $ foldMap oneHyperEdge
                      $ [ (multiRankingEntryGetDocumentName edgeEntry, edgeEntry)
                        | edgeEntry <- edgeRun
                        ]


        edgeFeatureGraph :: HM.HashMap PageId (HM.HashMap PageId EdgeFeatureVec)
        edgeFeatureGraph = HM.fromListWith (<>)
                         $ fmap (\((u,v),f) -> (u, HM.singleton v f))
                         $ HM.toList allHyperEdges

    in Graph edgeFeatureGraph


generateNodeFeatures :: QueryId -> [MultiRankingEntry PageId GridRun] -> [EdgeDoc] -> HM.HashMap PageId EntityFeatureVec
generateNodeFeatures query entityRun allEdgeDocs =
   let
        universalGraph :: HM.HashMap PageId [EdgeDoc]
        universalGraph = edgeDocsToUniverseGraph allEdgeDocs

   in HM.fromList [ (entity, (entityScoreVec entityRankEntry edgeDocs))
                  | entityRankEntry <- entityRun
                  , let entity = multiRankingEntryGetDocumentName entityRankEntry  -- for each entity in ranking...
                  , Just edgeDocs <- pure $ entity `HM.lookup` universalGraph
                  ]


{-
data Node payload = Node
                 { nodePageId :: PageId
                 , nodeData :: payload
                 }


mergeGraphAndNodeFeatures :: Graph (Node ()) (EdgeFeatureVec)
                          -> HM.HashMap PageId (Node EntityFeatureVec)
                          -> Graph (Node EntityFeatureVec) (EdgeFeatureVec)
mergeGraphAndNodeFeatures edgeFeatureGraph' nodeFeatures  =
    let
        nodeFeatures' pageId = fromJust $ pageId `HM.lookup` nodeFeatures

        edgeFeatureGraph ::  HM.HashMap (Node ()) ( HM.HashMap (Node ()) (EdgeFeatureVec)  )
        Graph edgeFeatureGraph = edgeFeatureGraph'
        annotatedGraph = fmap nodeMergeOuter $ HM.toList edgeFeatureGraph
          where nodeMergeOuter :: (Node (), HM.HashMap (Node ()) (EdgeFeatureVec))
                               -> (Node EntityFeatureVec, HM.HashMap (Node EntityFeatureVec) (EdgeFeatureVec))
                nodeMergeOuter (Node u _, list) =
                        (nodeFeatures' u, HM.fromList $ fmap nodeMergeInner $ HM.toList list)
                nodeMergeInner :: (Node (), EdgeFeatureVec)
                              -> (Node EntityFeatureVec, EdgeFeatureVec)
                nodeMergeInner (Node v _, edgeFeats) =
                        (nodeFeatures' v, edgeFeats)

    in Graph $ HM.fromList annotatedGraph
--
--         [ (featured u, [(featured v, edge)]
--         | (u, list) <- edgeFeatureGraph
--         , (v, edge) <- list
--         ]
--
-}



data Candidates = Candidates { candidateEdgeDocs :: [EdgeDoc]
                             , candidateEdgeRuns :: [MultiRankingEntry ParagraphId GridRun]
                             , candidateEntityRuns :: [MultiRankingEntry PageId GridRun]
                             }

selectCandidateGraph
    :: EdgeDocsLookup
    -> QueryId
    -> [MultiRankingEntry ParagraphId GridRun]
    -> [MultiRankingEntry PageId GridRun]
    -> Candidates
selectCandidateGraph edgeDocsLookup _queryId edgeRun entityRun =
    Candidates { candidateEdgeDocs = edgeDocs''
               , candidateEdgeRuns = edgeRun''
               , candidateEntityRuns = entityRun''
               }
  where
    restrict :: (Eq a, Hashable a) => [a] -> HM.HashMap a b -> HM.HashMap a b
    restrict keys m =
        let m2 = HM.fromList [(k, ()) | k <- keys]
        in m `HM.intersection` m2

    uniqBy :: (Eq b, Hashable b) => (a->b) -> [a] -> [a]
    uniqBy keyF elems =
        HM.elems $ HM.fromList [ (keyF e, e) | e <- elems]

    -- goal: select the subset of entityRunEntries, edgesRunEntries, and edgeDocs that
    -- fulfill these criteria:
    --
    -- edgeDocs has entry in edgeRuns
    -- entities have entityRun entries
    -- entities have indicent edgeDocs
    --
    -- but otherwise edgeFeatures are only considered,
    -- if a) they belong to one indicent endgeDoc
    -- and b) they have an edgeRun entry

    paraIdToEdgeRun = HM.fromList [ (multiRankingEntryGetDocumentName run, run) | run <- edgeRun]
    pageIdToEntityRun = [(multiRankingEntryGetDocumentName run, run)  | run <- entityRun]

    edgeDocs = edgeDocsLookup $ HM.keys paraIdToEdgeRun


    (entityRun', edgeRun', edgeDocs')  = unzip3
                                      $ [ (entityEntry, edgeEntry, edgeDoc)
                                        | (pageId, entityEntry) <- pageIdToEntityRun
                                        , edgeDoc <- edgeDocs
                                        , pageId `HS.member` (edgeDocNeighbors edgeDoc)
                                        , let paraId = edgeDocParagraphId edgeDoc
                                        , Just edgeEntry <- pure $ paraId `HM.lookup` paraIdToEdgeRun
                                        ]

    entityRun'' = uniqBy multiRankingEntryGetDocumentName entityRun'
    edgeRun'' = uniqBy multiRankingEntryGetDocumentName edgeRun'
    edgeDocs'' = uniqBy edgeDocParagraphId edgeDocs'

combineEntityEdgeFeatures
    :: QueryId
    -> Candidates
    -> HM.HashMap (QueryId, PageId) CombinedFeatureVec
combineEntityEdgeFeatures query cands@Candidates{candidateEdgeDocs = allEdgeDocs, candidateEdgeRuns = edgeRun, candidateEntityRuns = entityRun} =
    let
--         edgeDocsLookup = wrapEdgeDocsTocs $ HM.fromList $ [ (edgeDocParagraphId edgeDoc, edgeDoc) | edgeDoc <- allEdgeDocs]
        edgeFeatureGraph :: Graph PageId (EdgeFeatureVec)
        edgeFeatureGraph = generateEdgeFeatureGraph query cands -- edgeDocsLookup query edgeRun entityRun
        Graph edgeFeatureGraph' = edgeFeatureGraph

        nodeFeatures :: HM.HashMap PageId EntityFeatureVec
        nodeFeatures = generateNodeFeatures query entityRun allEdgeDocs

        -- stack node vector on top of projected edge feature vector
        -- no need to use nodeEdgeFeatureGraph
    in HM.fromList
       [ ((query, u), concatFeatureVec uFeats (F.aggregateWith (+) edgeFeats))
       | entityRankEntry <- entityRun
       , let u = multiRankingEntryGetDocumentName entityRankEntry

       , let Just uFeats =  u `HM.lookup` nodeFeatures
       , let Just edgeFeats = do
                 xs <- u `HM.lookup` edgeFeatureGraph'
                 return [ edgeFeat | edgeFeat <- HM.elems xs ]
       ]

fconcat :: Features -> Features -> Features
fconcat (Features xs) (Features ys) = Features  (xs VU.++ ys)

fsum :: Features -> Features -> Features
fsum (Features xs) (Features ys) = Features $ VU.zipWith (+) xs ys

entityScoreVec :: MultiRankingEntry PageId GridRun -> [EdgeDoc] -> EntityFeatureVec
entityScoreVec entityRankEntry incidentEdgeDocs = makeEntFeatVector  (
      [ (EntIncidentEdgeDocsRecip, recip numIncidentEdgeDocs)
  --  , (EntDegreeRecip, recip degree)
      , (EntDegree, degree)
      ]
      ++ rankEntFeatures Aggr (multiRankingEntryCollapsed entityRankEntry)
      ++ concat [ rankEntFeatures (GridRun' g) entry
         | (g, entry) <- multiRankingEntryAll entityRankEntry
         ]
     )
  where
   numIncidentEdgeDocs = realToFrac $ length incidentEdgeDocs
   degree =  realToFrac $ HS.size $ foldl1' HS.union $ fmap edgeDocNeighbors incidentEdgeDocs

edgeScoreVec :: MultiRankingEntry ParagraphId GridRun
             -> FeatureVec EdgeFeature Double
edgeScoreVec edgedocsRankEntry = makeEdgeFeatVector $
                                    [ (EdgeCount, 1.0)
                                    -- TODO
                                    --, ( EdgeDocKL
                                    --  , let Just edgeDocs = multiRankingEntryGetDocumentName edgedocsRankEntry `HM.lookup` edgeDocsByPara
                                    --    in edgeDocKullbackLeibler connectedEdgeDocs edgeDocs
                                    --  )
                                    ]
                                    ++ rankEdgeFeatures Aggr (multiRankingEntryCollapsed edgedocsRankEntry)
                                    ++ concat [ rankEdgeFeatures (GridRun' g) entry
                                       | (g, entry) <- multiRankingEntryAll edgedocsRankEntry
                                       ]
{-
  where

        connectedEdgeDocs :: ParagraphId -> [EdgeDoc]
        connectedEdgeDocs = undefined

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
-}

textToTokens :: T.Text -> Retrieve.TermCounts
textToTokens = foldMap Retrieve.oneTerm . Retrieve.textToTokens'




type EdgeDocsLookup =  ([ParagraphId] -> [EdgeDoc])

wrapEdgeDocsTocs :: HM.HashMap ParagraphId EdgeDoc
                 -> EdgeDocsLookup
wrapEdgeDocsTocs paraId2EdgeDoc =
    \paragraphIds -> catMaybes $ fmap (`HM.lookup` paraId2EdgeDoc) paragraphIds


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


-- -----------------------------------
-- iterative optimization
-- -----------------------------------

-- | Compute a dot product between a feature and weight vector, ensuring
-- positivity.
posifyDot :: PageRankExperimentSettings -> PosifyEdgeWeights
          -> Normalization
          -> EdgeFeatureVec   -- ^ parameter vector
          -> [EdgeFeatureVec] -- ^ all features
          -> EdgeFeatureVec
          -> Double
posifyDot expSettings posifyOpt normalizer params' allFeatures =
    \feats ->
    let computedWeight =
          case posifyOpt of
              Exponentiate ->  exp (F.dotFeatureVecs params' feats)
              Logistic ->  logistic (F.dotFeatureVecs params' feats)
              CutNegative ->
                  case F.dotFeatureVecs params' feats of
                    x | x > 0.0 -> x
                    _ -> 0.0

              ExpDenormWeight ->  exp (F.dotFeatureVecs denormWeights' feats)
              Linear  -> (F.dotFeatureVecs params' feats) - minimumVal
    in case expSettings of
          PageRankNormal -> computedWeight
          PageRankJustStructure -> 1.0
          PageRankWeightOffset1 -> computedWeight + 1.0
          PageRankWeightOffset01 -> computedWeight + 0.1

  where
    !minimumVal = minimum $ fmap (\feats -> F.dotFeatureVecs params' feats) allFeatures

    denormWeights' :: EdgeFeatureVec
    denormWeights' =
        let v :: VU.Vector Double
            v = F.getFeatureVec params'
            normedF = denormWeights normalizer (Features v)
            normedV :: VU.Vector Double
            normedV =  getFeatures normedF
        in F.unsafeFeatureVecFromVector normedV


-- | Quite unsafe
featureVecToFeatures :: FeatureVec a Double -> Features
featureVecToFeatures = Features . F.getFeatureVec

-- | Quite unsafe
featuresToFeatureVec :: Features -> FeatureVec a Double
featuresToFeatureVec = F.unsafeFeatureVecFromVector . getFeatures

modelToFeatureVec :: (Show a, Read a, Ord a)
                  => FeatureSpace a -> Model -> FeatureVec a Double
modelToFeatureVec fspace model =
    F.fromList fspace
    [ (k', v)
    | (k, v) <- M.toList $ modelWeights model
    , let k' = read $ T.unpack $ getFeatureName k
    ]

interleavedPageRankTraining
    :: ()
    => (EdgeFeatureVec -> EdgeFeatureVec -> Double) -- ^ edge feature dot product
    -> Graph PageId (FeatureVec EdgeFeature Double) -- ^ graph for single query FIXME
    -> FeatureSpace EdgeFeatureVec
    -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
    -> TrainData
    -> StdGen
    -> [(Eigenvector PageId Double, Weight)]
interleavedPageRankTraining dotProduct graph fspace metric trainData =
    go initialPR initialL2R
  where
    go :: VI.Vector VU.Vector (DenseId PageId) Double
       -> Weight -> StdGen
       -> [(Eigenvector PageId Double, Weight)]
    go x0 y0 gen0 =
        let graph' = fmap (dotProduct (featuresToFeatureVec y0)) graph
            x = head $ drop 3 $ persPageRankWithSeedsAndInitial mapping x0 alpha mempty graph'
            (_score, y) = head $ drop 3 $ coordAscent gen metric y0 trainData
            (gen, gen1) = System.Random.split gen0
        in (x,y) : go (eigenvectorValues x) y gen1

    alpha = 0.1
    mapping  = mkDenseMapping (nodeSet graph)
    initialPR = VI.replicate (denseRange mapping) (1 / realToFrac (DenseMapping.size mapping))
    initialL2R = Features $ VU.replicate (featureDimension fspace) 1

    featureNames :: [FeatureName]
    featureNames = fmap (FeatureName . T.pack . show) $ F.featureNames fspace


-- ---------------------------------------------
-- Node rankings to teleportation distribution
-- ---------------------------------------------

nodeRankingToDistribution :: Ranking.Ranking Double PageId
                          -> HM.HashMap PageId Double -- only positive entries, expected to sum to 1.0
nodeRankingToDistribution ranking =
    let proportions = HM.fromList
                    $ [ (node, 1.0 / (realToFrac (rank+1)))
                      | (rank, (_score, node)) <- zip [1 :: Int ..] $ Ranking.toSortedList ranking
                      , rank <= 20
                      ]
        totals = Foldable.sum proportions
    in fmap (/ totals) proportions
