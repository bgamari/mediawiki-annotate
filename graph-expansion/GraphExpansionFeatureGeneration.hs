{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Concurrent.Async
import Control.DeepSeq
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import System.Random
import GHC.Generics
import GHC.Stack

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Hashable

import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.TocFile as Toc
import CAR.Utils
import GridFeatures

import EdgeDocCorpus
import GraphExpansion
import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (featureDimension, FeatureSpace, FeatureVec, featureNames, mkFeatureSpace, concatSpace, concatFeatureVec)
import SimplIR.FeatureSpace.Normalise


import qualified CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import MultiTrecRunFile
import PageRank
import DenseMapping
import Graph

import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import qualified Data.GraphViz.Commands.IO as Dot
import Control.Monad


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



bm25MethodName :: CarRun.MethodName
bm25MethodName = CarRun.MethodName "BM25"
qlMethodName :: CarRun.MethodName
qlMethodName = CarRun.MethodName "QL"



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

(>!<) :: (Show k, Ord k, HasCallStack) => M.Map k v -> k -> v
m >!< key =
    case key `M.lookup` m  of
        Just v -> v
        Nothing -> error $ ">!<: Can't lookup key "<> show key <> "in map. Example keys " <> (show $ take 5 $ M.keys m)<> "..."

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
              pagesToQueryDocs queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries

    let dotFileName :: QueryId -> FilePath
        dotFileName queryId = (outputFilePrefix ++ "-"++ T.unpack (CAR.RunFile.unQueryId queryId) ++"-graphviz.dot")

        filterGraphTopEdges :: Graph PageId Double -> Graph PageId Double
        filterGraphTopEdges graph =  graph -- filterEdges (\_ _ weight -> weight > 5.0) graph

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
    putStrLn $ "Loaded edgDocsLookup."


    entityRuns <-  mapM (mapM CAR.RunFile.readEntityRun) entityRunFiles  -- mapM mapM -- first map over list, then map of the snd of a tuple
    putStrLn $ "Loaded EntityRuns: "<> show (length entityRuns)

    edgeRuns <-  mapM (mapM CAR.RunFile.readParagraphRun) edgedocRunFiles

    putStrLn $ "Loaded EdgeRuns: "<> show (length edgeRuns)

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
          Just model <-  trace "loading model" $ Data.Aeson.decode @(Model CombinedFeature) <$> BSL.readFile modelFile

          let docFeatures :: M.Map (QueryId, QRel.DocumentName) CombinedFeatureVec
              docFeatures = makeStackedFeatures edgeDocsLookup collapsedEntityRun collapsedEdgedocRun combinedFSpace' experimentSettings
              degreeCentrality = fmap (modelWeights' model `score`) docFeatures
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

          let params :: WeightVec CombinedFeature
              params = modelWeights' model

          let graphWalkRanking :: QueryId -> IO (Ranking.Ranking Double PageId)
              graphWalkRanking query
                 | any (< 0) graph' = error ("negative entries in graph' for query "++ show query ++ ": "++ show (count (< 0) graph'))
                 | otherwise = do
                   print $ (take 3 $ toEntries eigv)
                   exportGraphViz (filterGraphTopEdges $ dropDisconnected graph') (dotFileName query)
                   return $ Ranking.fromList $ map swap $ toEntries eigv
--                 | otherwise = traceShow (take 3 $ toEntries eigv) $ Ranking.fromList $ map swap $ toEntries eigv
                where
                  count pred = getSum . foldMap f
                    where f x = if pred x then Sum 1 else Sum 0

                  candidates = selectCandidateGraph edgeDocsLookup query edgeRun entityRun
                    where
                      edgeRun = collapsedEdgedocRun >!< query
                      entityRun = collapsedEntityRun >!< query

                  graph :: Graph PageId EdgeFeatureVec
                  graph =  fmap (filterExpSettingsEdge edgeFSpace edgeFSpace' (expSettingToCritEdge experimentSettings))
                         $ generateEdgeFeatureGraph query candidates -- edgeDocsLookup query edgeRun entityRun


                  normalizer :: Normalisation _ Double
                  normalizer = zNormalizer $ Foldable.toList graph

                  params' :: WeightVec EdgeFeature
                  params' = WeightVec $ F.projectFeatureVec combinedFSpace edgeFSpace
                            (either (const Nothing) Just)
                            (getWeightVec params)

                  graph' :: Graph PageId Double
                  graph' = fmap (posifyDot pageRankExperimentSettings posifyEdgeWeightsOpt normalizer params' (Foldable.toList graph)) graph
                  -- for debugging...
--                   graph' = fmap (\feats -> trace (show feats) ( tr  ( posifyDot params feats))) graph
                    where
                      normFeats :: EdgeFeatureVec -> EdgeFeatureVec
                      normFeats fv = normFeatures normalizer fv

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
                                BiasedPersPageRankWalk -> persPageRankWithNonUniformSeeds (teleportation/2) seedNodeDistr graph'
                                  where  betaTotal = teleportation/2
                                         seedNodeDistr = fmap (* betaTotal) (nodeDistr M.! query )



              runRanking query = do
                  ranking <- graphWalkRanking query
                  let rankEntries =  [ CAR.RunFile.RankingEntry query pageId rank score (CAR.RunFile.MethodName "PageRank")
                                    | (rank, (score, pageId)) <- zip [1..] (Ranking.toSortedList ranking)
                                    ]

                  CAR.RunFile.writeEntityRun  (outputFilePrefix ++ "-"++ T.unpack (CAR.RunFile.unQueryId query) ++"-pagerank-test.run")
                                    $ rankEntries
          mapConcurrently_(runRanking . queryDocQueryId) queries

      TrainModel modelFile -> do
          let docFeatures = makeStackedFeatures edgeDocsLookup collapsedEntityRun collapsedEdgedocRun combinedFSpace' experimentSettings

          putStrLn $ "Made docFeatures: "<>  show (length docFeatures)
          let allData :: TrainData CombinedFeature
              allData = augmentWithQrels qrel docFeatures Relevant

              metric = avgMetricQrel qrel
              totalElems = getSum . foldMap ( Sum . length ) $ allData
              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

          putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
                    " queries and "++ show totalElems ++" items total of which "++
                    show totalPos ++" are positive."

          let displayTrainData :: TrainData f
                               -> [String]
              displayTrainData trainData =
                [ show k ++ " -> "++ show elem
                | (k,list) <- M.toList trainData
                , elem <- list]

          putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
          gen0 <- newStdGen  -- needed by learning to rank
          trainMe gen0 allData combinedFSpace' metric outputFilePrefix modelFile



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

        normalizer = zNormalizer $ M.elems docFeatures''
        docFeatures = fmap (normFeatures normalizer) docFeatures''

    in docFeatures


logistic :: Double -> Double
logistic t =
    1.0 / (1.0 + exp (-1 * t))


changeKey :: Ord k' => (k-> k') -> M.Map k v -> M.Map k' v
changeKey f map_ =
    M.fromList $ fmap (\(key,val) -> (f key, val)) $ M.toList map_



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



entityScoreVec :: MultiRankingEntry PageId GridRun -> [EdgeDoc] -> EntityFeatureVec
entityScoreVec entityRankEntry incidentEdgeDocs = makeEntFeatVector  (
      [ (EntIncidentEdgeDocsRecip, recip numIncidentEdgeDocs)
  --  , (EntDegreeRecip, recip degree)
      , (EntDegree, degree)
      ]
      ++ entityScoreVecFromMultiRankings entityRankEntry
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
             => M.Map q [(QRel.DocumentName, FeatureVec f Double, Maybe IsRelevant)]
             -> M.Map q [(QRel.DocumentName, FeatureVec f Double, IsRelevant)]
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
          -> Normalisation EdgeFeature Double
          -> WeightVec EdgeFeature  -- ^ parameter vector
          -> [EdgeFeatureVec] -- ^ all features
          -> EdgeFeatureVec
          -> Double
posifyDot expSettings posifyOpt normalizer params' allFeatures =
    \feats ->
    let computedWeight =
          case posifyOpt of
              Exponentiate ->  exp (params' `score` feats)
              Logistic ->  logistic (params' `score` feats)
              CutNegative ->
                  case params' `score` feats of
                    x | x > 0.0 -> x
                    _ -> 0.0

              ExpDenormWeight ->  exp (denormWeights' `score` feats)
              Linear  -> (params' `score` feats) - minimumVal
    in case expSettings of
          PageRankNormal -> computedWeight
          PageRankJustStructure -> 1.0
          PageRankWeightOffset1 -> computedWeight + 1.0
          PageRankWeightOffset01 -> computedWeight + 0.1

  where
    !minimumVal = minimum $ fmap (\feats -> params' `score` feats) allFeatures

    denormWeights' :: WeightVec EdgeFeature
    denormWeights' =
        WeightVec $ denormWeights normalizer (getWeightVec params')

interleavedPageRankTraining
    :: ()
    => (EdgeFeatureVec -> EdgeFeatureVec -> Double) -- ^ edge feature dot product
    -> Graph PageId (FeatureVec EdgeFeature Double) -- ^ graph for single query FIXME
    -> FeatureSpace EdgeFeature
    -> ScoringMetric IsRelevant CAR.RunFile.QueryId QRel.DocumentName
    -> TrainData EdgeFeature
    -> StdGen
    -> [(Eigenvector PageId Double, WeightVec EdgeFeature)]
interleavedPageRankTraining dotProduct graph fspace metric trainData =
    go initialPR initialL2R
  where
    go :: VI.Vector VU.Vector (DenseId PageId) Double
       -> WeightVec EdgeFeature -> StdGen
       -> [(Eigenvector PageId Double, WeightVec EdgeFeature)]
    go x0 y0 gen0 =
        let graph' = fmap (y0 `score`) graph
            x = head $ drop 3 $ persPageRankWithSeedsAndInitial mapping x0 alpha mempty graph'
            (_score, y) = head $ drop 3 $ coordAscent gen metric fspace y0 trainData
            (gen, gen1) = System.Random.split gen0
        in (x,y) : go (eigenvectorValues x) y gen1

    alpha = 0.1
    mapping  = mkDenseMapping (nodeSet graph)
    initialPR = VI.replicate (denseRange mapping) (1 / realToFrac (DenseMapping.size mapping))
    initialL2R :: WeightVec EdgeFeature
    initialL2R = WeightVec $ F.repeat fspace 1

    featureNames :: [FeatureName]
    featureNames = fmap (FeatureName . T.pack . show) $ F.featureNames fspace


-- ---------------------------------------------
-- Graphviz export
-- ---------------------------------------------

exportGraphViz :: Graph PageId Double -> FilePath -> IO ()
exportGraphViz fancyWeightedGraph dotFilename = do
    let graph = dotGraph fancyWeightedGraph  --todo highlight seeds
    Dot.writeDotFile (dotFilename ++ ".dot") graph
    void $ Dot.runGraphvizCommand Dot.Neato graph Dot.Svg dotFilename

instance Dot.PrintDot PageId where
    unqtDot pageId = Dot.unqtDot $ unpackPageId pageId
    toDot = pure . PP.dquotes . foldMap PP.char . unpackPageId

dotGraph :: Graph PageId Double -> Dot.DotGraph PageId
dotGraph graph = Dot.graphElemsToDot params nodes edges
  where
    params = Dot.nonClusteredParams { Dot.fmtEdge = \(_,_,w) -> [ Dot.penWidth (w/10.0), Dot.Weight $ Dot.Int (ceiling w) ]
                                    , Dot.fmtNode = \(_,a) -> [Dot.toLabel a]
                                    , Dot.globalAttributes = [ Dot.GraphAttrs [ Dot.OutputOrder Dot.EdgesFirst
                                                                              , Dot.Overlap $ Dot.PrismOverlap Nothing] ]
                                    }
    nodes = [ (a, unpackPageName $ pageIdToName a) | a <- HS.toList $ nodeSet graph ]
    edges = [ (a,b,w)
            | (a, ns) <- HM.toList $ getGraph graph
            , (b, w) <- HM.toList ns
            ]

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
