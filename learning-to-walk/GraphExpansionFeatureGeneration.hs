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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}

import Control.Concurrent.Async
import Control.DeepSeq hiding (rwhnf)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Parallel.Strategies
import Control.Lens (each)
import Data.Coerce
import Data.Tuple
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import System.IO
import Data.Aeson
import System.Random
import GHC.Generics
import GHC.Stack
import Control.Exception
import System.FilePath

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Map.Lazy as ML
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Generic as VG
import qualified Text.PrettyPrint.Leijen.Text as PP
import Data.List
import Data.Maybe
import Data.Foldable as Foldable
import Data.Hashable
import Control.Concurrent
import Control.Concurrent.Map
import Data.List.Split
import qualified Codec.Serialise as CBOR


import CAR.Types hiding (Entity)
import CAR.ToolVersion
import CAR.Retrieve as Retrieve
import qualified CAR.RunFile as CarRun
import CAR.TocFile as Toc
import CAR.Utils
import AspectUtils
import GridFeatures

import EdgeDocCorpus
import DenseMapping
import PageRank
import qualified SimplIR.SimpleIndex as Index
import SimplIR.LearningToRank
import SimplIR.LearningToRankWrapper
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace (FeatureSpace, FeatureVec)
import SimplIR.FeatureSpace.Normalise
import SimplIR.Intern

import qualified CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import qualified SimplIR.Ranking as Ranking
import MultiTrecRunFile
import PageRank
import Graph

import qualified Data.GraphViz as Dot
import qualified Data.GraphViz.Attributes.Complete as Dot
import qualified Data.GraphViz.Commands.IO as Dot
import Control.Monad

import GridFeatures
import EdgeDocCorpus
import LookupWrapper
import CandidateGraph
import NodeAndEdgeFeatures
import TrainAndStore

import Debug.Trace  as Debug

type NumResults = Int

type EntityIndex = Index.OnDiskIndex Term PageId Int

data SeedDerivation = SeedsFromLeadSection
                    | SeedsFromEntityIndex EntityIndex

data QuerySource = QueriesFromCbor FilePath QueryDerivation SeedDerivation
                 | QueriesFromJson FilePath

data RankingType = EntityRanking | EntityPassageRanking
  deriving (Show)

data ModelSource = ModelFromFile FilePath -- filename to read model from
                 | GraphWalkModelFromFile FilePath -- filename to read model from for graph walks
                 | TrainModel FilePath -- filename to write resulting file to
                 | GraphWalkTrainModel FilePath -- filename to read model from
  deriving (Show)

data ExperimentSettings = AllExp | NoEdgeFeats | NoEntityFeats | AllEdgeWeightsOne | JustAggr | NoAggr | JustScore | JustRecip | JustCount | LessFeatures
                        | JustNone | JustSimpleRm | JustSimpleRmCount | JustTitleAndSectionPath
                        | NoNeighborFeats | NoRawEdgeFeats
                        | JustSourceNeighbors | JustUnsourcedNeighbors | JustScaledSourceNeighbors
                        | NoEdgesFromParas | NoEdgesFromAspects | NoEdgesFromPages | NoEdgesFromLinkLink
                        | ExpPage | ExpSection | ExpEcmTestFeature | OnlyNoneXFeature
                        | CandidateNoEdgeDocs | CandidateNoPageDocs | CandidateNoAspectDocs
                        | CandidatesMadeNotFromEntityRuns | CandidatesMadeNotFromEdgeRuns | CandidatesMadeNotFromAspectRuns
                        | CandidateStrict | CandidateGenerous | CandidateDisableDivideEdgeFeats | CandidateRemoveLowNodes
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PageRankExperimentSettings = PageRankNormal | PageRankJustStructure | PageRankWeightOffset1 | PageRankWeightOffset01
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PageRankConvergence = L2Convergence | Iteration10 | Iteration2 | Iteration1
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data PosifyEdgeWeights = Exponentiate | ExpDenormWeight | Linear | Logistic | CutNegative
  deriving (Show, Read, Ord, Eq, Enum, Bounded)

data GraphWalkModel = PageRankWalk | BiasedPersPageRankWalk
  deriving (Show, Read, Ord, Eq, Enum, Bounded)


-- | PageRank teleportation \(\alpha\)
type TeleportationProb = Double


data FlowParser = NormalFlowArguments' NormalFlowArguments | FlowTrainOnly' FlowTrainOnly

opts :: Parser (FlowParser)
opts = commands <|> fmap NormalFlowArguments' normalArgs
  where
    commands = subparser
      $ cmd "train-only" (fmap FlowTrainOnly' trainArgs)
      <> cmd "normal" (fmap NormalFlowArguments' normalArgs)
    cmd name action = command name (info (helper <*> action) fullDesc)


trainArgs :: Parser FlowTrainOnly
trainArgs = FlowTrainOnly
    <$>  (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> optional minibatchParser
    <*> (option str (long "train-data" <> metavar "File with serialized training data"))
    <*> (option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file"))
    <*> option str (short 'm' <> long "train-model" <> metavar "Model-FILE" <> help "train learning-to-rank model and write to Model-FILE")

normalArgs  :: Parser NormalFlowArguments
normalArgs = NormalFlowArguments
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS-FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> querySource
    <*> many (option (CarRun.QueryId . T.pack <$> str) (long "query" <> metavar "QUERY" <> help "execute only this query"))
    <*> option auto (short 'k' <> long "num-results" <> help "use number of results of input rankings (per query)")
    <*> some gridRunParser
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "edge-doc-cbor" <> metavar "EdgeDoc-CBOR" <> help "EdgeDoc cbor file"))
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "page-doc-cbor" <> metavar "PageDoc-CBOR" <> help "PageDoc cbor file"))
    <*> (option (Toc.IndexedCborPath <$> str)  ( long "aspect-doc-cbor" <> metavar "AspectDoc-CBOR" <> help "AspectDoc cbor file"))
    <*> (option str (long "qrel" <> metavar "QRel-FILE"))
    <*> modelSource
    <*> many (option auto (long "posify" <> metavar "OPT" <> help ("Option for how to ensure positive edge weights. For walking without training multiple posify options can be given Choices: " ++(show [minBound @PosifyEdgeWeights .. maxBound]))  ))
    <*> many (option auto (long "teleport" <> help "teleport probability (for page rank), for walking without training multiple teleports can be given" ))
    <*> many (option auto (long "exp" <> metavar "EXP" <> help ("one or more switches for experimentation. Choices: " ++(show [minBound @ExperimentSettings .. maxBound]))))
    <*> option auto (long "pagerank-settings" <> metavar "PREXP" <> help ("Option for how to ensure positive edge weights. Choices: " ++(show [PageRankNormal,PageRankJustStructure,  PageRankWeightOffset1, PageRankWeightOffset01])) <> value PageRankNormal)
    <*> option auto (long "pagerank-convergence" <> metavar "CONV" <> help ("How pagerank determines convergence. Choices: " ++(show [minBound @PageRankConvergence .. maxBound])) <> value Iteration10)
    <*> option auto (long "graph-walk-model" <> metavar "PAGERANK" <> help ("Graph walk model. Choices: " ++(show [minBound @GraphWalkModel .. maxBound])) <> value PageRankWalk)
    <*> optional minibatchParser
    <*> optional (option str (short 'd' <> long "train-data" <> metavar "TRAIN-DATA-FILE" <> help "load training data from serialized file (instead of creating it from scratch)"))
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
        <|> option (ModelFromFile <$> str) (long "test-model" <> metavar "Model-FILE" <> help "read learning-to-rank model from Model-FILE")
        <|> option (GraphWalkModelFromFile <$> str) (long "read-model" <> metavar "Model-FILE" <> help "read learning-to-rank model for graph walking from Model-FILE")
        <|> option (GraphWalkTrainModel <$> str) (long "train-walk-model" <> metavar "Model-FILE" <> help "train learning-to-rank model for graph walking from Model-FILE")




-- --------------------------------- Training Data Serialisation ------------------------------------------------------


data SerialisedTrainingData
    = forall entityPh edgePh . SerialisedTrainingData { serialisedFSpaces :: FeatureSpaces entityPh edgePh
                                                      , serialisedAllData :: TrainData CombinedFeature (F.Stack '[entityPh, edgePh])
                                                      }

instance CBOR.Serialise IsRelevant

instance CBOR.Serialise SerialisedTrainingData where
    encode (SerialisedTrainingData {..}) =
      CBOR.encode (F.featureNames $ entityFSpace serialisedFSpaces)
      <> CBOR.encode (F.featureNames $ edgeFSpace serialisedFSpaces)
      <> CBOR.encode (fmap (fmap (\(a,b,c) -> (a, F.toVector b, c))) serialisedAllData)

    decode = do
      entityFSpace <- F.unsafeFromFeatureList <$> CBOR.decode
      edgeFSpace <- F.unsafeFromFeatureList <$> CBOR.decode
      let fspaces = FeatureSpaces { combinedFSpace = F.eitherSpaces entityFSpace edgeFSpace, .. }
      allData <- CBOR.decode
      let unpackRawFVec (x,y,z)
            | Just v <- F.unsafeFromVector (combinedFSpace fspaces) y = (x,v,z)
            | otherwise = error $ "Serialise(SerialisedTrainingData): Deserialise failure in unpackRawFVec for docid "<> show x
      return $ SerialisedTrainingData fspaces $ fmap (fmap unpackRawFVec) allData

-- --------------------------------- Query Doc ------------------------------------------------------


data QueryDoc = QueryDoc { queryDocQueryId      :: !CarRun.QueryId
                         , queryDocQueryText    :: !T.Text
                         }
           deriving (Show, Generic, Eq)
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

data PageRankHyperParams = PageRankHyperParams { pageRankExperimentSettings :: PageRankExperimentSettings
                                               , posifyEdgeWeights :: PosifyEdgeWeights
                                               , graphWalkModel :: GraphWalkModel
                                               , teleportation :: TeleportationProb
                                               }
-- ---------------------------------------------------------------------------------------

(>!<) :: (Show k, Ord k, HasCallStack) => M.Map k v -> k -> v
m >!< key =
    case key `M.lookup` m  of
        Just v -> v
        Nothing -> error $ ">!<: Can't lookup key "<> show key <> " in map. Map size: "<> show (length m) <>" Example keys " <> (show $ take 10 $ M.keys m)<> "..."

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    oneortheother <- execParser' 1 (helper <*> opts) mempty
    case oneortheother of
        NormalFlowArguments' args -> normalFlow args
        FlowTrainOnly' args -> trainOnlyFlow args


data  FlowTrainOnly
    = FlowTrainOnly { qrelFile :: FilePath
                    , miniBatchParamsMaybe :: Maybe MiniBatchParams
                    , trainDataFileOpt :: FilePath
                    , outputFilePrefix :: FilePath
                    , modelFile :: FilePath
                    }

trainOnlyFlow :: FlowTrainOnly -> IO ()
trainOnlyFlow FlowTrainOnly {..} = do
    putStrLn $ " TrainDataFile : "++ (show trainDataFileOpt)
    putStrLn $ " MinbatchParams (only for training) : "++ (show miniBatchParamsMaybe)

    let miniBatchParams = fromMaybe defaultMiniBatchParams miniBatchParamsMaybe
        fixQRel :: QRel.Entry QRel.QueryId QRel.DocumentName QRel.IsRelevant
                -> QRel.Entry CAR.RunFile.QueryId QRel.DocumentName QRel.IsRelevant
        fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile

    --  allData :: TrainData CombinedFeature _
    SerialisedTrainingData fspaces allData <- CBOR.deserialise <$> BSL.readFile (trainDataFileOpt)
    train fspaces allData qrel miniBatchParams outputFilePrefix modelFile



data NormalFlowArguments
    = NormalFlowArguments
                       { articlesFile ::  FilePath
                       , outputFilePrefix :: FilePath
                       , querySrc :: QuerySource
                       , queryRestriction:: [CarRun.QueryId]
                       , numResults :: NumResults
                       , gridRunFiles:: [(GridRun, EntityOrEdge, FilePath)]
                       , edgeDocsCborFile :: Toc.IndexedCborPath ParagraphId EdgeDoc
                       , pagesDocCborFile :: Toc.IndexedCborPath PageId PageDoc
                       , aspectDocCborFile :: Toc.IndexedCborPath AspectId AspectDoc
                       , qrelFile :: FilePath
                       , modelSource :: ModelSource
                       , posifyEdgeWeightsOpts :: [PosifyEdgeWeights]
                       , teleportations :: [TeleportationProb]
                       , experimentSettings :: [ExperimentSettings]
                       , pageRankExperimentSettings :: PageRankExperimentSettings
                       , pageRankConvergence :: PageRankConvergence
                       , graphWalkModel :: GraphWalkModel
                       , miniBatchParamsMaybe :: Maybe MiniBatchParams
                       , trainDataFileOpt :: Maybe FilePath
                       }
normalFlow :: NormalFlowArguments -> IO ()
normalFlow NormalFlowArguments {..}  = do
    putStrLn $ "# Pages: " ++ show articlesFile
    putStrLn $ "# Query restriction: " ++ show queryRestriction

    F.SomeFeatureSpace (allEntFSpace :: F.FeatureSpace EntityFeature allEntFeats) <- pure entSomeFSpace
    F.SomeFeatureSpace (allEdgeFSpace :: F.FeatureSpace EdgeFeature allEdgeFeats) <- pure edgeSomeFSpace
    let allCombinedFSpace :: F.FeatureSpace CombinedFeature (F.Stack '[allEntFeats, allEdgeFeats])
        allCombinedFSpace = F.eitherSpaces allEntFSpace allEdgeFSpace

    let entityRunFiles  = [ (g, r) | (g, Entity, r) <- gridRunFiles]
        aspectRunFiles  = [ (g, r) | (g, Aspect, r) <- gridRunFiles]
        edgedocRunFiles = [ (g, r) | (g, Edge, r) <- gridRunFiles]

    putStrLn $ "# Entity runs:  "++ (show $ fmap (show) (entityRunFiles ))
    putStrLn $ "# Aspect runs:  "++ (show $ fmap (show) (aspectRunFiles ))
    putStrLn $ "# EdgeDoc runs: "++ ( show $ fmap (show) (edgedocRunFiles))
    putStrLn $ "# numResults: "++ ( show (numResults))

    putStrLn $ " Experimentation settings: "++ (show experimentSettings)
    putStrLn $ " edgeDocs lookup : "++ (show edgeDocsCborFile)
    putStrLn $ " pageDocs lookup : "++ (show pagesDocCborFile)
    putStrLn $ " aspectDocs lookup : "++ (show aspectDocCborFile)
    putStrLn $ " model comes from : "++ (show modelSource)
    putStrLn $ " teleport (only for page rank) : "++ (show teleportations)
    putStrLn $ " posify with (only for page rank) : "++ (show posifyEdgeWeightsOpts)
    putStrLn $ " pageRankExperimentSettings (only for page rank) : "++ (show pageRankExperimentSettings)
    putStrLn $ " graphWalkModel (only for page rank) : "++ (show graphWalkModel)
    putStrLn $ " MinbatchParams (only for training) : "++ (show miniBatchParamsMaybe)
    putStrLn $ " TrainDataFile : "++ (show trainDataFileOpt)

    let serialisedDataFile = outputFilePrefix <.> "alldata.cbor"

        miniBatchParams = fromMaybe defaultMiniBatchParams miniBatchParamsMaybe

        featureGraphSettings :: FeatureGraphSettings
        featureGraphSettings = FeatureGraphSettings
                                 { fgsNoEdgeDocs = not $ (CandidateNoEdgeDocs `elem` experimentSettings)
                                 , fgsNoPageDocs = not $ (CandidateNoPageDocs `elem` experimentSettings)
                                 , fgsDisableDivideEdgeFeats =  not $ (CandidateDisableDivideEdgeFeats `elem` experimentSettings)
                                 , fgsRemoveLowFeatures = CandidateRemoveLowNodes `elem` experimentSettings
                                 , fgsNoAspectDocs = not $ (CandidateNoAspectDocs `elem` experimentSettings)
                                 }

        teleportation = case teleportations of
                          (x:_) -> x
                          []    -> 0.1

        posifyEdgeWeightsOpt = case posifyEdgeWeightsOpts of
                          (x:_) -> x
                          []    -> ExpDenormWeight

    queries' <-
        case querySrc of
          QueriesFromCbor queryFile queryDeriv _seedDeriv -> do
              pagesToQueryDocs queryDeriv <$> readPagesOrOutlinesAsPages queryFile

          QueriesFromJson queryFile -> do
              QueryDocList queries <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queries

    let dotFileName :: QueryId -> FilePath
        dotFileName queryId = outputFilePrefix ++ "-"++ T.unpack (CAR.RunFile.unQueryId queryId) ++"-graphviz.dot"

        filterGraphTopEdges :: Graph PageId Double -> Graph PageId Double
        filterGraphTopEdges graph =  graph -- filterEdges (\_ _ weight -> weight > 5.0) graph

    let fixQRel :: QRel.Entry QRel.QueryId QRel.DocumentName QRel.IsRelevant
                -> QRel.Entry CAR.RunFile.QueryId QRel.DocumentName QRel.IsRelevant
        fixQRel (QRel.Entry qid docId rel) = QRel.Entry (CAR.RunFile.QueryId qid) docId rel
    qrel <- map fixQRel <$> QRel.readQRel @IsRelevant qrelFile


    let qrelMap :: M.Map QueryId (M.Map PageId IsRelevant)
        qrelMap =
          M.unionsWith (M.unionWith checkSame)
          [ M.singleton qid (M.singleton docId' rel)
          | QRel.Entry qid docId rel <- qrel
          , let docId' = packPageId $ T.unpack docId
          ]
          where checkSame :: IsRelevant -> IsRelevant -> IsRelevant
                checkSame rel1 rel2 =
                        if rel1 == rel2 then rel1
                        else error $ "Qrel file contains conflicting relevance data for (query, item): "<> qrelFile

        lookupQrel :: IsRelevant -> QueryId -> PageId -> IsRelevant
        lookupQrel def query document =
            let query' = CAR.RunFile.unQueryId query
            in  if (T.pack $ unpackPageId document) == query'  || (aspectHasPageId document $ parseAspectId query')
                    then Relevant
                else fromMaybe def $ do
                        m <- query `M.lookup` qrelMap
                        r <- document `M.lookup` m
                        return r


    queries <-
        if null queryRestriction
          then return queries'
          else do putStrLn $ "# using only queries "<>show queryRestriction
                  return
                    $ nub
                    $ filter (\q-> queryDocQueryId q `elem` queryRestriction) queries'
    putStrLn $ "# query count: " ++ show (length queries)

    putStrLn "Loading edgeDocsLookup, pageDocLookup, aspectDocLookup."
    edgeDocsLookup <- readEdgeDocsToc edgeDocsCborFile
    pagesLookup <- readAbstractDocToc pagesDocCborFile
                   :: IO (AbstractLookup PageId)
    aspectLookup <- readAbstractDocToc aspectDocCborFile
                   :: IO (AbstractLookup AspectId)

    ncaps <- getNumCapabilities

    putStrLn "Loading EntityRuns..."
    entityRuns <- fmap concat $ mapConcurrentlyL ncaps
        (runInternM . runInternM . mapM (mapM (\path ->
                     lift . internAll (each . CAR.RunFile.document)
                 =<< internAll (each . CAR.RunFile.traverseText (const pure))
                 =<< liftIO (CAR.RunFile.readEntityRun path))))
        (chunksOf 2 entityRunFiles)
        :: IO [(GridRun, [RankingEntry PageId])]

    putStrLn $ "Loaded EntityRuns: "<> show (length entityRuns)

    putStrLn "Loading AspectRuns..."
    aspectRuns <- fmap concat $ mapConcurrentlyL ncaps
        (runInternM . runInternM . mapM (mapM (\path ->
                     lift . internAll (each . CAR.RunFile.document)
                 =<< internAll (each . CAR.RunFile.traverseText (const pure))
                 =<< liftIO (readAspectRun path))))
        (chunksOf 2 aspectRunFiles)
        :: IO [(GridRun, [RankingEntry AspectId])]

    putStrLn $ "Loaded AspectRuns: "<> show (length aspectRuns)

    putStrLn "Loading EdgeRuns..."
    edgeRuns <- fmap concat $ mapConcurrentlyL ncaps
        (runInternM . runInternM . mapM (mapM (\path ->
                     lift . internAll (each . CAR.RunFile.document)
                 =<< internAll (each . CAR.RunFile.traverseText (const pure))
                 =<< liftIO (CAR.RunFile.readParagraphRun path))))
        (chunksOf 2 edgedocRunFiles)
        :: IO [(GridRun, [RankingEntry ParagraphId])]

    putStrLn $ "Loaded EdgeRuns: "<> show (length edgeRuns)

    putStrLn "Computing collapsed runs..."
    let collapsedEntityRun :: M.Map QueryId [MultiRankingEntry PageId GridRun]
        !collapsedEntityRun =
            collapseRuns
            $ map (fmap $ filter (\entry -> CAR.RunFile.carRank entry <= numResults)) entityRuns
        collapsedAspectRun :: M.Map QueryId [MultiRankingEntry AspectId GridRun]
        !collapsedAspectRun =
            collapseRuns
            $ map (fmap $ filter (\entry -> CAR.RunFile.carRank entry <= numResults)) aspectRuns
        collapsedEdgedocRun :: M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
        !collapsedEdgedocRun =
            collapseRuns
            $ map (fmap $ filter (\entry -> CAR.RunFile.carRank entry <= numResults)) edgeRuns
         -- Todo: collapsed Aspect Runs
        tr x = traceShow x x
    putStrLn "Computed collapsed runs."
    putStrLn $ "queries from collapsed entity runs: "++show (M.size collapsedEntityRun)
    putStrLn $ "queries from collapsed aspect runs: "++show (M.size collapsedAspectRun)
    putStrLn $ "queries from collapsed edge doc runs: "++show (M.size collapsedEdgedocRun)

    let candidateGraphGenerator :: CandidateGraphGenerator
        candidateGraphGenerator =
            let candidateGraphSettings = CandidateGraphSettings {  cfsMadeFromEntityRuns = not $ CandidatesMadeNotFromEntityRuns `elem` experimentSettings
                                                                 , cfsMadeFromEdgeRuns =  not $ CandidatesMadeNotFromEdgeRuns `elem` experimentSettings
                                                                 , cfsMadeFromAspectRuns = not$ CandidatesMadeNotFromAspectRuns `elem` experimentSettings
                                                                 }
            in  if CandidateStrict `elem` experimentSettings  then
                   selectStrictCandidateGraph edgeDocsLookup pagesLookup aspectLookup
                else -- (CandidateStrict or none)
                    selectGenerousCandidateGraph candidateGraphSettings edgeDocsLookup pagesLookup aspectLookup


        pageRankHyperParams = PageRankHyperParams pageRankExperimentSettings posifyEdgeWeightsOpt graphWalkModel teleportation

    let   makeFeatureGraphs :: forall edgeFSpace. F.FeatureSpace EdgeFeature edgeFSpace ->  ML.Map QueryId (Graph PageId (EdgeFeatureVec edgeFSpace))
          makeFeatureGraphs edgeFSpace' =
              M.fromList -- $ withStrategy (parTraversable $ evalTuple2 r0 rseq)
                         $[ (qid, f qid)
                         | q <- queries
                         , let qid = queryDocQueryId q
                         ]
            where
              f :: QueryId -> Graph PageId (EdgeFeatureVec edgeFSpace)
              f query =
                  generateEdgeFeatureGraph edgeFSpace' featureGraphSettings query pagesLookup aspectLookup candidates mempty -- todo add node features instead of mempty!!
                where
                  !candidates = Debug.trace "created candidate graph." $ candidateGraphGenerator query edgeRun entityRun aspectRun
                    where
                      edgeRun = collapsedEdgedocRun >!< query
                      entityRun = collapsedEntityRun >!< query
                      aspectRun = collapsedAspectRun >!< query




    case modelSource of
      GraphWalkTrainModel modelFile -> do
          let updatedModelFile = modelFile <> "walk.json"
              (MiniBatchParams batchSteps batchSize evalSteps) = miniBatchParams
              evalSteps' = evalSteps +2
--               pageRankSteps = evalSteps'
          putStrLn "loading model"
          Just (SomeModel model) <-  Data.Aeson.decode @(SomeModel CombinedFeature) <$> BSL.readFile modelFile
          putStrLn "loaded model."

          mkFeatureSpaces (modelFeatures model) $ \(F.FeatureMappingInto modelToCombinedFeatureVec) (fspaces :: FeatureSpaces entityPh edgePh) -> do

              let featureGraphs :: ML.Map QueryId (Graph PageId (EdgeFeatureVec edgePh))
                  !featureGraphs = makeFeatureGraphs (edgeFSpace fspaces)

                  nodeDistr :: M.Map QueryId (HM.HashMap PageId Double) -- xyes, only positive entries, expected to sum to 1.0
                  !nodeDistr =
                      nodeDistrPriorForGraphwalk fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup (coerce modelToCombinedFeatureVec $ modelWeights' model) collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

                  augmentWithQrels :: QueryId -> Ranking Double PageId -> Ranking Double (PageId, IsRelevant)
                  augmentWithQrels query =
                      fmap (\page -> (page, lookupQrel NotRelevant query page))

                  totalRels :: M.Map QueryId Int
                  !totalRels = fmap countRel qrelMap
                                where countRel :: M.Map x IsRelevant -> Int
                                      countRel m = length [ r
                                                          | (_, r) <- M.toList m
                                                          , QRel.isPositive r
                                                          ]

                  metric :: ScoringMetric IsRelevant QueryId
                  metric = meanAvgPrec (\q -> fromMaybe 0 $ q `M.lookup` totalRels)  Relevant

                  !someKindOfTrainingData = M.fromList $ [(q,q) | q <- intersect (M.keys totalRels) (M.keys featureGraphs) ] -- totalRels does not include queries for which there is no training data

              gen0 <- newStdGen


              -- only get the edge features out
              Just (F.FeatureMappingInto toEdgeVec) <- pure $ F.mapFeaturesInto (modelFeatures model) (edgeFSpace fspaces) (either (const Nothing) Just)
              putStrLn $ "preparing graph walk ..."
              let initParams' :: WeightVec EdgeFeature edgePh
                  initParams' = coerce toEdgeVec $ modelWeights' model

                  initialEigenv :: Graph PageId a -> Eigenvector PageId Double
                  initialEigenv graph' = PageRank.uniformInitial mapping
                    where !mapping = DenseMapping.mkDenseMapping (nodeSet graph')

                  -- todo should we should parallelize nextPageRankIter computation: parallelize produceWalkingGraph over queries?
                  -- todo: no, because the evaluation is driven by coordinatAscent (via: rerank query w)
                  -- todo: but can we move to the global name space?

                  -- todo: we call it repeatedly with the same eigvs, (oncefor the iteration and once for the computing the ranking. Is this intentional?


                  -- nextPageRankIter holds a map from query  to a function that computes one step of pagerank given a parameter :: WeightVec
                  -- ! because we don't want to delay the computation. Danger of out-of-memory issues, but if we want multiple iterations we would need this anyway.
                  nextPageRankIter :: M.Map QueryId (WeightVec EdgeFeature edgePh -> M.Map QueryId (Eigenvector PageId Double) -> (Ranking Double PageId, Eigenvector PageId Double))
                  !nextPageRankIter = withStrategy (parTraversable rseq )
                                     $ makeNextPageRankIter (edgeFSpace fspaces)
                                                          (PageRankHyperParams pageRankExperimentSettings posifyEdgeWeightsOpt graphWalkModel teleportation )
                                                          pageRankConvergence queries featureGraphs nodeDistr
              putStrLn $ "...done preparing graph walk, starting interleaved optimization."
              let
                  iterate :: StdGen
                          -> WeightVec EdgeFeature edgePh
                          -> M.Map QueryId (Eigenvector PageId Double)
                          -> [(WeightVec EdgeFeature edgePh, M.Map QueryId (Eigenvector PageId Double), M.Map QueryId (Ranking Double (PageId, IsRelevant)))]
                  iterate gen0 params eigvs =

                      let
                          rerank :: QueryId -> WeightVec EdgeFeature edgePh -> Ranking Double (PageId, IsRelevant)
                          rerank query w =
                              let (ranking, _pageRank) = (nextPageRankIter >!< query) w eigvs     -- one step of pagerank for this query, then take ranking
                              in augmentWithQrels query ranking                             -- we do not save the pagerank result at this point.


                          (gen1, gen2) = System.Random.split gen0
                          optimise :: StdGen -> WeightVec EdgeFeature edgePh -> M.Map QueryId QueryId -> [WeightVec EdgeFeature edgePh]
                          optimise gen model someKindOfTrainingData' =
                                let scoreParams = naiveCoordAscent metric rerank gen model someKindOfTrainingData'
                                in fmap snd scoreParams
                                -- Note: naiveCoordAscent will parallize reranking across queries

                          params' :: WeightVec EdgeFeature edgePh
                          params' = (!!evalSteps') $ miniBatched batchSteps batchSize optimise gen1 params someKindOfTrainingData
    --                       (_score, params') : _ = naiveCoordAscent metric rerank gen1 params someKindOfTrainingData  -- without minibatching

                          iterResult :: M.Map QueryId (Ranking Double PageId, Eigenvector PageId Double)
                          iterResult = fmap (\f -> f params' eigvs) nextPageRankIter

                          eigvs' :: M.Map QueryId (Eigenvector PageId Double)
                          eigvs' = fmap snd iterResult
                          rankings' :: M.Map QueryId (Ranking Double (PageId, IsRelevant))
                          rankings' = M.mapWithKey (\q (r,_) -> augmentWithQrels q r) iterResult
                          score' = metric rankings'
                          printTopRanking rs =
                              unlines $ take 3 $ M.elems $ fmap (show . take 2 . Ranking.toSortedList) rs
                          !x = Debug.trace ("trainwalk score " <> (show score') <> "\n topRankEntries \n"<> (printTopRanking rankings') <> "\n params' "<> show params') $ 0

                      in (params', eigvs',  rankings') : iterate gen2 params' eigvs'

                  iters = iterate gen0 initParams' (fmap initialEigenv featureGraphs)

                  dropQrels :: Ranking Double (PageId, IsRelevant) -> (Ranking Double PageId)
                  dropQrels annRanking = fmap fst annRanking
              mapM_ (storeRankingData' outputFilePrefix ("trainwalk-tele-" <> show teleportation)) $  [(i, fmap dropQrels r) | (i, (_,_,r)) <- zip [1..4] iters]-- zip [1..20] eigvss -- zip [1..20] iters -- eigenvs

              let
                  (newParams, _eigenvs, trainRanking):_ = drop 2 iters
                  trainScore :: Double
                  trainScore = metric trainRanking

              putStrLn $ "new model params " <> show newParams
              let model = Model newParams
                  formatQuery pageId = CAR.RunFile.QueryId $ T.pack $ unpackPageId pageId

              storeModelData outputFilePrefix updatedModelFile model trainScore "trainwalk"



      GraphWalkModelFromFile modelFile -> do
          putStrLn "loading model"
          Just (SomeModel model) <-  Data.Aeson.decode @(SomeModel CombinedFeature) <$> BSL.readFile modelFile
          mkFeatureSpaces (modelFeatures model) $ \(F.FeatureMappingInto modelToCombinedFeatureVec) (fspaces :: FeatureSpaces entityPh edgePh) -> do
              let
                  nodeDistr :: M.Map QueryId (HM.HashMap PageId Double) -- xyes, only positive entries, expected to sum to 1.0
                  !nodeDistr =
                      nodeDistrPriorForGraphwalk fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup (coerce modelToCombinedFeatureVec $ modelWeights' model) collapsedEntityRun collapsedEdgedocRun collapsedAspectRun
                  -- todo nodeDistr should not be strict, as some graph walks don't use it

                  -- only edge model weights
                  params' :: WeightVec EdgeFeature edgePh
                  params' = coerce (toEdgeVecSubset . modelToCombinedFeatureVec) $ modelWeights' model

                  featureGraphs :: ML.Map QueryId (Graph PageId (EdgeFeatureVec edgePh))
                  featureGraphs = makeFeatureGraphs (edgeFSpace fspaces)


                  Just (F.FeatureMappingInto toEdgeVecSubset) =
                    F.mapFeaturesInto (combinedFSpace fspaces) (edgeFSpace fspaces) (either (const Nothing) Just)


                  initialEigenv :: Graph PageId a -> Eigenvector PageId Double
                  initialEigenv graph' = PageRank.uniformInitial mapping
                    where !mapping = DenseMapping.mkDenseMapping (nodeSet graph')

                  initialEigenVmap :: M.Map QueryId (Eigenvector PageId Double)
                  initialEigenVmap = fmap initialEigenv featureGraphs

              forM_ teleportations $ \teleportation -> do
                  forM_ posifyEdgeWeightsOpts $ \posifyEdgeWeightsOpt -> do
                      let
                          nextPageRankIter :: M.Map QueryId ( WeightVec EdgeFeature edgePh
                                                            -> M.Map QueryId (Eigenvector PageId Double)
                                                            -> (Ranking Double PageId, Eigenvector PageId Double)
                                                            )
                          nextPageRankIter = makeNextPageRankIter (edgeFSpace fspaces)
                                                                  (PageRankHyperParams pageRankExperimentSettings posifyEdgeWeightsOpt graphWalkModel teleportation )
                                                                  pageRankConvergence queries featureGraphs nodeDistr

                          eigvs = initialEigenVmap
                          emptyRanking :: Ranking Double PageId
                          emptyRanking = Ranking.fromSortedList []

                          eigvss :: [M.Map QueryId (Ranking Double PageId, Eigenvector PageId Double)]
                          eigvss = iterate f $ fmap (\eigv -> (emptyRanking, eigv)) initialEigenVmap
                            where
                              f :: M.Map QueryId (Ranking Double PageId, Eigenvector PageId Double) -> M.Map QueryId (Ranking Double PageId, Eigenvector PageId Double)
                              f accum = withStrategy (parTraversable $ evalTraversable rseq)
                                             $ fmap (\f -> f params' eigvs) nextPageRankIter
                                where eigvs = fmap snd accum


                          pageRankExceptionHandler :: PageRankException -> IO ()
                          pageRankExceptionHandler ex = putStrLn $ show ex
                      mapM_ ( handle pageRankExceptionHandler .
                                storeRankingData' outputFilePrefix ("walk-tele-" <> show teleportation<> "-posify-" <> show posifyEdgeWeightsOpt) )

                          $  [(i, fmap fst iterResult) | (i, iterResult) <- zip [2..9] eigvss]


      ModelFromFile modelFile -> do
          Just (SomeModel model) <-  trace "loading model" $ Data.Aeson.decode @(SomeModel CombinedFeature) <$> BSL.readFile modelFile
          mkFeatureSpaces (modelFeatures model) $ \(F.FeatureMappingInto modelToCombinedFeatureVec) (fspaces :: FeatureSpaces entityPh edgePh) -> do
              case trainDataFileOpt of
                  Just trainDataFile -> do
                       SerialisedTrainingData dataFspaces allData <- CBOR.deserialise <$> BSL.readFile (serialisedDataFile)
                       let Just featProj = F.project (combinedFSpace fspaces) (combinedFSpace dataFspaces)
                           model' = coerce featProj (modelWeights' model)

                           totalElems = getSum . foldMap ( Sum . length ) $ allData
                           totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

                       putStrLn $ "Test model with (trainData) "++ show (M.size allData) ++
                                 " queries and "++ show totalElems ++" items total of which "++
                                 show totalPos ++" are positive."

                       let trainRanking = withStrategy (parTraversable rseq)
                                        $ rerankRankings' model' allData
                       storeRankingDataNoMetric outputFilePrefix trainRanking "learn2walk-degreecentrality"

                  Nothing ->  do
                          let model' = coerce modelToCombinedFeatureVec model          -- todo: maybe rename to modelFeatureSubsetProjection

                          let augmentNoQrels     :: forall docId queryId f s.
                                                    (Ord queryId, Ord docId)
                                                 => M.Map (queryId, docId) (FeatureVec f s Double)
                                                 -> M.Map queryId [(docId, FeatureVec f s Double, IsRelevant)]
                              augmentNoQrels docFeatures =
                                    let franking :: M.Map queryId [(docId, FeatureVec f s Double, IsRelevant)]
                                        franking = M.fromListWith (++)
                                                   [ (qid, [(doc, features, Relevant)])
                                                   | ((qid, doc), features) <- M.assocs docFeatures
                                                   ]
                                    in franking


                          let docFeatures = makeStackedFeatures fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

                          putStrLn $ "Made docFeatures: "<>  show (length docFeatures)
                          let allData :: TrainData CombinedFeature (F.Stack '[entityPh, edgePh])
                              allData = augmentWithQrels qrel docFeatures

                --               !metric = avgMetricQrel qrel
                              totalElems = getSum . foldMap ( Sum . length ) $ allData
                              totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

                          putStrLn $ "Test model with (trainData) "++ show (M.size allData) ++
                                    " queries and "++ show totalElems ++" items total of which "++
                                    show totalPos ++" are positive."

                          let trainRanking = withStrategy (parTraversable rseq)
                                           $ rerankRankings' model' allData
                          storeRankingDataNoMetric outputFilePrefix trainRanking "learn2walk-degreecentrality"


      TrainModel modelFile ->
          let F.SomeFeatureSpace features = F.mkFeatureSpace
                                            $ S.filter (filterFeaturesByExperimentSetting experimentSettings)
                                            $ F.featureNameSet allCombinedFSpace
          in mkFeatureSpaces features $ \_ fspaces -> do


              let docFeatures = makeStackedFeatures fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

                  augmentWithQrels :: forall f s.
                                      [QRel.Entry QueryId  QRel.DocumentName IsRelevant]
                                   -> M.Map (QueryId,  QRel.DocumentName) (FeatureVec f s Double)
                                   -> M.Map QueryId [( QRel.DocumentName, FeatureVec f s Double, IsRelevant)]
                  augmentWithQrels qrel docFeatures =
                      let relevance :: M.Map (QueryId,  QRel.DocumentName) IsRelevant
                          relevance = M.fromList [ ((qid, doc), rel)
                                                 | QRel.Entry qid doc rel <- qrel
                                                 ]

                          -- | when query starts with document, then its relevant even if there is no explicit qrels entry
                          queryMatchRelevance :: QueryId ->  QRel.DocumentName -> IsRelevant
                          queryMatchRelevance qid doc =
                              let query' = CAR.RunFile.unQueryId qid
                                  doc' =  doc
                                  doc'' = packPageId $ T.unpack doc'
                              in if (query' == doc') || (aspectHasPageId doc'' $ parseAspectId query')
                                        then Relevant
                                        else NotRelevant

                          franking :: M.Map QueryId [( QRel.DocumentName, FeatureVec f s Double, IsRelevant)]
                          franking = M.fromListWith (++)
                                     [ (qid, [(doc, features, relDocs)])
                                     | ((qid, doc), features) <- M.assocs docFeatures
                                     , let def = queryMatchRelevance qid doc
                                     , let relDocs = M.findWithDefault def (qid, doc) relevance
                                     ]
                      in franking



                  allData :: TrainData CombinedFeature _
                  allData = augmentWithQrels qrel docFeatures


              putStrLn $ "Made docFeatures: "<>  show (length docFeatures)
              BSL.writeFile (outputFilePrefix <.> "alldata.cbor") $ CBOR.serialise
                  $ SerialisedTrainingData fspaces allData

              train fspaces allData qrel miniBatchParams outputFilePrefix modelFile


train :: FeatureSpaces entityPh edgePh
      ->  TrainData CombinedFeature _
      -> [QRel.Entry CAR.RunFile.QueryId doc IsRelevant]
      -> MiniBatchParams
      -> FilePath
      -> FilePath
      -> IO()
train fspaces allData qrel miniBatchParams outputFilePrefix modelFile =  do
              let metric :: ScoringMetric IsRelevant CAR.RunFile.QueryId
                  !metric = meanAvgPrec (totalRelevantFromQRels qrel) Relevant
                  totalElems = getSum . foldMap ( Sum . length ) $ allData
                  totalPos = getSum . foldMap ( Sum . length . filter (\(_,_,rel) -> rel == Relevant)) $ allData

              putStrLn $ "Feature dimension: "++show (F.dimension $ F.featureSpace $ (\(_,a,_) -> a) $ head' $ snd $ M.elemAt 0 allData)
              putStrLn $ "Training model with (trainData) "++ show (M.size allData) ++
                        " queries and "++ show totalElems ++" items total of which "++
                        show totalPos ++" are positive."

              let displayTrainData :: Show f => TrainData f s -> [String]
                  displayTrainData trainData =
                    [ show k ++ " -> "++ show elm
                    | (k,list) <- M.toList trainData
                    , elm <- list]

              putStrLn $ "Training Data = \n" ++ intercalate "\n" (take 10 $ displayTrainData $ force allData)
              gen0 <- newStdGen  -- needed by learning to rank
              trainMe miniBatchParams (EvalCutoffAt 100) gen0 allData (combinedFSpace fspaces) metric outputFilePrefix modelFile



-- --------------------------------------

storeRankingData' :: String -> String -> (Int, M.Map QueryId (Ranking Double PageId)) -> IO ()
storeRankingData' outputFilePrefix modelDesc0 (iterNo, ranking) = do
      let modelDesc = modelDesc0 <> "-iteration-"<> show iterNo -- "walk-tele-" <> show teleportation <> "-iteration-"<> show iterNo

      CAR.RunFile.writeEntityRun (outputFilePrefix++"-model-"++modelDesc++".run")
           $ l2rRankingToRankEntries (CAR.RunFile.MethodName $ T.pack $ "l2r "++modelDesc)
           $ ranking

    where l2rRankingToRankEntries methodName rankings =
              [ CAR.RunFile.RankingEntry { carQueryId = query
                                         , carDocument = doc
                                         , carRank = rank
                                         , carScore = rankScore
                                         , carMethodName = methodName
                                         }
              | (query, ranking) <- M.toList rankings
              , ((rankScore, doc), rank) <- (Ranking.toSortedList ranking) `zip` [1..]
              ]

makeNextPageRankIter ::  FeatureSpace EdgeFeature edgePh
                     -> PageRankHyperParams
                     -> PageRankConvergence
                     -> [QueryDoc]
                     -> M.Map
                          CAR.RunFile.QueryId
                          (Graph PageId (EdgeFeatureVec edgePh))
                     -> M.Map QueryId (HM.HashMap PageId Double)
                     -> M.Map QueryId (  WeightVec EdgeFeature edgePh
                                      -> M.Map QueryId (Eigenvector PageId Double)
                                      -> (Ranking Double PageId, Eigenvector PageId Double)
                                      )
makeNextPageRankIter edgeFSpace (prh@PageRankHyperParams {..}) conv queries featureGraphs nodeDistr =
    M.fromList
    $  [ (qid, \w eigvs -> produceWalkingGraph edgeFSpace prh conv featureGraph qid nodeDistr w (eigvs >!< qid)) -- eigv0 as lamba
       | q <- queries
       , let qid = queryDocQueryId q
             featureGraph = featureGraphs >!< qid
       ]


produceWalkingGraph :: forall edgePh. ()
                    => F.FeatureSpace EdgeFeature edgePh
                    -> PageRankHyperParams
                    -> PageRankConvergence
                    -> Graph PageId (EdgeFeatureVec edgePh)
                    -> QueryId
                    -> M.Map QueryId (HM.HashMap PageId Double)
                    -> WeightVec EdgeFeature edgePh
                    -> Eigenvector PageId Double
                    -> (Ranking Double PageId, Eigenvector PageId Double)
produceWalkingGraph edgeFSpace (prh@PageRankHyperParams {..}) conv featureGraph query nodeDistr =
-- prh : pageRankExperimentSettings posifyEdgeWeights graphWalkModel teleportation
  \params eigv ->
    nextRerankIter params eigv
  where
    normalizer :: Normalisation EdgeFeature edgePh Double
    !normalizer = zNormalizer $ Foldable.toList featureGraph

    walkingGraph :: WeightVec EdgeFeature edgePh -> Graph PageId Double
    walkingGraph params' =
        let graph = fmap (posifyDot pageRankExperimentSettings posifyEdgeWeights normalizer params' (Foldable.toList featureGraph)) featureGraph
            graph' = dropLowEdges graph
        in graph'


    walkIters :: Eigenvector PageId Double
              -> Graph PageId Double
              -> [Eigenvector PageId Double]
    walkIters initial graph' =
        case graphWalkModel of
          PageRankWalk ->
                pageRankWithInitial teleportation graph' initial
          BiasedPersPageRankWalk ->
              biasedPersPageRankWithInitial alpha seedNodeDistr graph' initial
      where
        betaTotal = teleportation/2
        seedNodeDistr = fmap (* betaTotal) (nodeDistr M.! query )
        alpha = (teleportation/2)

    nextRerankIter :: WeightVec EdgeFeature edgePh -> Eigenvector PageId Double
                   -> (Ranking Double PageId, Eigenvector PageId Double)
    nextRerankIter params initial  =
          let graph = walkingGraph params
              nexteigen = graphWalkToConvergence conv $ walkIters initial $ graph
              ranking = eigenvectorToRanking nexteigen
--                           debugRanking = unlines $ fmap show $ take 3 $ Ranking.toSortedList ranking
          in (ranking, nexteigen)



biasedPersPageRankWithInitial
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> HM.HashMap n a     -- ^ teleportation probability \(\beta\) for each seed
    -> Graph n a          -- ^ the graph
    -> Eigenvector n a  -- ^ initial page rank vector
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
biasedPersPageRankWithInitial alpha seeds graph initial =
--      Debug.trace ("biasedPersPageRankWithInitial with initial "<> show initial) $
    persPageRankWithSeedsAndInitial mapping initial alpha seeds graph
  where
    !mapping  = mkDenseMapping (nodeSet graph)

pageRankWithInitial
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> Graph n a          -- ^ the graph
    -> Eigenvector n a  -- ^ initial page rank vector
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
pageRankWithInitial alpha graph initial =
    persPageRankWithSeedsAndInitial mapping initial alpha HM.empty graph
  where
    !mapping  = mkDenseMapping (nodeSet graph)

graphWalkToConvergence :: PageRankConvergence -> [Eigenvector PageId Double] -> Eigenvector PageId Double
graphWalkToConvergence conv walkIters =
   let pageRankIters = zip walkIters (tail walkIters)

   in case conv of
        L2Convergence -> snd
                       $ head'
                       $ dropWhile (\(x,y) -> relChange x y > 1e-3)
                       $ pageRankIters
        Iteration10   -> snd $ (!! 10)  pageRankIters
        Iteration2    -> snd $ (!! 2)  pageRankIters
        Iteration1    -> snd $ (!! 1)  pageRankIters

eigenvectorToRanking :: Eigenvector doc Double -> Ranking Double doc
eigenvectorToRanking = Ranking.fromList . map swap . toEntries

nodeDistrPriorForGraphwalk
    :: forall entityPh edgePh.
       FeatureSpaces entityPh edgePh
    -> FeatureGraphSettings
    -> CandidateGraphGenerator
    -> PagesLookup
    -> AspectLookup
    -> WeightVec CombinedFeature (F.Stack '[entityPh, edgePh])
    -> M.Map QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map QueryId [MultiRankingEntry AspectId GridRun]
    -> M.Map QueryId (HM.HashMap PageId Double)
nodeDistrPriorForGraphwalk
    fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup model collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =

  let docFeatures :: M.Map (QueryId, QRel.DocumentName) (CombinedFeatureVec entityPh edgePh)
      docFeatures = makeStackedFeatures fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

      degreeCentrality = fmap (model `score`) docFeatures
      queryToScoredList = M.fromListWith (<>) [(q, [(d, score)]) | ((q,d), score) <- M.toList degreeCentrality ]
      ranking :: M.Map QueryId (Ranking.Ranking Double QRel.DocumentName)
      ranking = fmap (Ranking.fromList . map swap) queryToScoredList

      rankingPageId :: M.Map QueryId (Ranking.Ranking Double PageId)
      rankingPageId = fmap (fmap qrelDocNameToPageId) ranking

      nodeDistr :: M.Map QueryId (HM.HashMap PageId Double) -- only positive entries, expected to sum to 1.0
      nodeDistr = fmap nodeRankingToDistribution rankingPageId
  in nodeDistr


dropLowEdges :: Graph PageId Double -> Graph PageId Double
dropLowEdges graph = filterEdges significantEdgeWeight graph
                        where significantEdgeWeight :: PageId ->  PageId -> Double -> Bool
                              significantEdgeWeight _ _ e = e > 1e-8


-- --------------------------------------


qrelDocNameToPageId :: QRel.DocumentName -> PageId
qrelDocNameToPageId docname = packPageId $ T.unpack docname


filterFeaturesByExperimentSetting :: [ExperimentSettings] ->  (CombinedFeature -> Bool)
filterFeaturesByExperimentSetting settings fname =
    all (`convert` fname) settings
  where
    convert :: ExperimentSettings -> (CombinedFeature -> Bool)
    convert setting = case setting of
                    AllExp -> const True
                    NoEdgeFeats -> noEdge
                    NoEntityFeats -> noEntity
                    NoNeighborFeats -> noNeighborFeats
                    NoRawEdgeFeats -> noRawEdge
                    AllEdgeWeightsOne -> const True -- needs to be handled elsewhere
                    JustAggr -> onlyAggr
                    NoAggr -> not . onlyAggr
                    JustScore -> onlyScore
                    JustRecip -> onlyRR
                    JustCount -> onlyCount
                    LessFeatures -> onlyLessFeatures
                    JustNone -> onlyNoneFeatures
                    ExpPage -> onlyPage
                    ExpSection -> onlySection
                    JustSimpleRm -> onlySimpleRmFeatures
                    JustSimpleRmCount -> onlySimpleRmCountFeatures
                    JustTitleAndSectionPath -> onlyTitleAndSectionPath
                    NoEdgesFromParas -> noEdgesFromParas
                    NoEdgesFromAspects -> noEdgesFromAspects
                    NoEdgesFromPages -> noEdgesFromPages
                    NoEdgesFromLinkLink -> noEdgesFromLinkLink
                    ExpEcmTestFeature -> onlyExpEcmTestFeature
                    OnlyNoneXFeature -> onlyNoneX
                    JustSourceNeighbors -> onlySourceNeighbors
                    JustScaledSourceNeighbors -> onlyScaledSourceNeighbors
                    JustUnsourcedNeighbors -> onlyUnsourcedNeighbors


                    CandidateNoEdgeDocs -> const True
                    CandidateNoPageDocs -> const True
                    CandidateDisableDivideEdgeFeats -> const True
                    CandidateStrict -> const True
                    CandidateGenerous -> const True
                    CandidateRemoveLowNodes -> const True
                    CandidatesMadeNotFromEntityRuns -> const True
                    CandidatesMadeNotFromEdgeRuns -> const True
                    CandidatesMadeNotFromAspectRuns -> const True

                    x -> error $ " No information on what to do with ExperimentSettings "<> show x

-- ----------------

-- ========================




dropUnjudged :: Ord q
             => M.Map q [(QRel.DocumentName, FeatureVec f s Double, Maybe IsRelevant)]
             -> M.Map q [(QRel.DocumentName, FeatureVec f s Double, IsRelevant)]
dropUnjudged featureMap =
    M.filter (not . null)   -- drop entries with empty lists
    $ M.map (mapMaybe dropUnjudged') featureMap
   where dropUnjudged' (doc, feat, Just rel) = Just $ (doc, feat, rel)
         dropUnjudged' (_ , _, Nothing) = Nothing


-- -----------------------------------
-- iterative optimization
-- -----------------------------------




logistic :: Double -> Double
logistic t =
    1.0 / (1.0 + exp (-1 * t))


-- | Compute a dot product between a feature and weight vector, ensuring
-- positivity.
posifyDot :: forall s.
             PageRankExperimentSettings -> PosifyEdgeWeights
          -> Normalisation EdgeFeature s Double
          -> WeightVec EdgeFeature s  -- ^ parameter vector
          -> [EdgeFeatureVec s] -- ^ all features
          -> EdgeFeatureVec s
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

    denormWeights' :: WeightVec EdgeFeature s
    denormWeights' =
        WeightVec $ denormWeights normalizer (getWeightVec params')

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

