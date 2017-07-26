{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Maybe
import Data.Semigroup hiding (All, Any, option)
import Data.Foldable
import Data.Coerce
import Options.Applicative
import System.IO
import Data.Time.Clock
import Numeric
import GHC.TypeLits
import Data.Aeson

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL

import CAR.Types
import CAR.AnnotationsFile as AnnsFile
import CAR.Retrieve as Retrieve

import EdgeDocCorpus
import EdgeDocIndex
import WriteRanking
import GraphExpansion
import GraphExpansionExperiments
import SimplIR.WordEmbedding
import SimplIR.WordEmbedding.GloVe
import ZScore

data QuerySource = QueriesFromCbor FilePath
                 | QueriesFromJson FilePath

opts :: Parser (FilePath, FilePath, FilePath, QuerySource, Maybe [Method], Int)
opts =
    (,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> option str (short 'e' <> long "embedding" <> metavar "FILE" <> help "Glove embeddings file")
    <*> querySource
    <*> optional methods
    <*> option auto (long "hops" <> metavar "INT" <> help "number of hops for initial outward expansion" <> value 3)
    where
      methods :: Parser [Method]
      methods = fmap concat $ some
          $ option (fmap (\x->[x]) method) (short 'm' <> long "method" <> metavar "METHOD" <> help "Methods to run")
        <|> option (str >>= methodSet) (long "method-set" <> metavar "METHODSET" <> help "Set of methods to run")

      method :: ReadM Method
      method = do name <- str
                  case M.lookup name methodMap of
                    Just m  -> return m
                    Nothing -> fail $ unlines $ [ "unknown method "++name
                                                , "known methods:"
                                                ] ++ map ("  "++) (M.keys methodMap)

      methodSet :: String -> ReadM [Method]
      methodSet "all"  = pure allMethods
      methodSet "topn" = pure topNPerGraphMethods
      methodSet "core" = pure coreMethods
      methodSet "prio0" = pure prio0Methods
      methodSet "prio1" = pure prio1Methods
      methodSet "prio2" = pure prio2Methods
      methodSet "prio3" = pure prio3Methods
      methodSet "prio4" = pure prio4Methods
      methodSet "fix1" = pure fix1Methods
      methodSet "fix2" = pure fix2Methods
      methodSet "test" = pure testMethods
      methodSet "candidateset" = pure [CandidateSet]
      methodSet _      = fail "unknown method set"

      methodMap = M.fromList [ (showMethodName m, m) | m <- allMethods ]

      querySource =
              option (fmap QueriesFromCbor str) (short 'q' <> long "queries" <> metavar "CBOR" <> help "Queries from CBOR pages")
          <|> option (fmap QueriesFromJson str) (short 'j' <> long "queries-json" <> metavar "JSON" <> help "Queries from JSON")


candidateSetList :: HS.HashSet PageId -> Int -> BinarySymmetricGraph
                ->  [(PageId, Double)]
candidateSetList seeds radius binarySymmetricGraph  =
    let nodeSet :: HS.HashSet PageId
        nodeSet = expandNodesK binarySymmetricGraph seeds radius
   in [ (pageId, 1.0) | pageId <- HS.toList nodeSet]



computeRankingsForQuery :: forall n. (KnownNat n)
                        => RetrievalFunction EdgeDoc
                        -> AnnotationsFile
                        -> [Term] -> HS.HashSet PageId -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> WordEmbedding n
                        -> [(Method, [(PageId, Double)])]

computeRankingsForQuery retrieveDocs annsFile query seeds radius universeGraph binarySymmetricGraph wordEmbedding =
    let nodeSet :: HS.HashSet PageId
        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        nodeToAttributes :: HM.HashMap PageId (Attributes (EmbeddingDim n))
        nodeToAttributes =
            zScoreStandardize
            $ foldMap (\pid -> HM.singleton pid $ toWordVec pid) (toList nodeSet)
          where
            toWordVec pid =
                wordVecToAttributes
                $ maybe mempty (pageTextEmbeddingAttributes wordEmbedding)
                $ AnnsFile.lookupPage pid annsFile

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
--         universeSubset = trace (" empty in nodeSet " ++ show ("" `HS.member` nodeSet)) $ subsetOfUniverseGraph universeGraph nodeSet
        universeSubset = subsetOfUniverseGraph universeGraph nodeSet


        edgeDocsSubset :: [EdgeDoc]
        edgeDocsSubset = HS.toList $ HS.fromList $ concat $ HM.elems universeSubset

        edgeFilters :: [(EdgeFilteringNames, [EdgeDoc] -> [EdgeDoc])]
        edgeFilters = [(BidiFiltered,  onlySymmetricEdges)
                      ,(Unfiltered,    id)
                      ]

        fancyGraphs :: [(GraphNames, [EdgeDoc] -> HM.HashMap PageId [EdgeDocWithScores])]
        fancyGraphs = [--(Top5PerNode,     const $ filterGraphByTop5NodeEdges  retrieveDocs      query)
                       (Top100PerGraph,  const $ filterGraphByTopNGraphEdges retrieveDocs 100  query)
                      ,(Top10PerGraph,   const $ filterGraphByTopNGraphEdges retrieveDocs 10   query)
                      ,(Top50PerGraph,   const $ filterGraphByTopNGraphEdges retrieveDocs 50   query)
                      ,(Top200PerGraph,  const $ filterGraphByTopNGraphEdges retrieveDocs 200  query)
                      ,(Top2000PerGraph, const $ filterGraphByTopNGraphEdges retrieveDocs 2000 query)
                      ]

        simpleGraphs :: [(GraphNames, [EdgeDoc] -> HM.HashMap PageId (HM.HashMap PageId [EdgeDoc]))]
        simpleGraphs =  [(SimpleGraph, noFilterTwice )
                        ,(RandomGraph, randomFilter 100)
                        ,(Random2000Graph, randomFilter 2000)
                        ]

        weightings :: [(WeightingNames, EdgeDocWithScores -> Double)]
        weightings =  [ (Count,  realToFrac . withScoreCount)
                      , (Score, withScoreScore)
                      , (RecipRank,   (\edge ->  1.0 / (realToFrac $ withScoreRank edge )))
                      , (LinearRank,  (\edge -> realToFrac (101 - (withScoreRank edge))))
                      , (BucketRank,  (\edge ->  let rank = withScoreRank $ edge
                                                 in if rank <= 5 then 3.0 else
                                                    if rank <= 20 then 2.0 else
                                                    1.0
                                      ))
                      ]


                     --rankByAttriPageRank graph teleport numAttrs nodeAttrs iterations =

        graphRankings :: [(GraphRankingNames, (HM.HashMap PageId (HM.HashMap PageId Double) -> [(PageId, Double)]))]
        graphRankings = [(PageRank, \graph ->  let wgraph = toGraph graph
                                               in rankByPageRank wgraph 0.15 20)
                        ,(PersPageRank, \graph -> let wgraph = toGraph graph
                                                  in rankByPersonalizedPageRank wgraph 0.15 seeds 20)
                        ,(AttriRank, \graph ->  let wgraph = toGraph graph
                                                    numAttrs = wordEmbeddingDim wordEmbedding
                                                in rankByAttriPageRank wgraph 0.15 numAttrs nodeToAttributes 20)
                        ,(ShortPath, \graph -> let wgraph = toGraph graph
                                               in rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce wgraph) (toList seeds))
                        ,(MargEdges, \graph -> marginalizeEdges graph)
                        ]

        fancyWeightedGraphs ::  [((GraphNames, EdgeFilteringNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        fancyWeightedGraphs =  [((gname, ename, wname), accumulateEdgeWeights graph weighting)
                               | (ename, edgeFilter) <- edgeFilters
                               , (gname, mkGraph) <- fancyGraphs
                               , (wname, weighting) <- weightings
                               , let graph = mkGraph $ edgeFilter edgeDocsSubset
                               ]

        simpleWeightedGraphs :: [((GraphNames, EdgeFilteringNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        simpleWeightedGraphs = concat [ [ ((gname, ename, Count),  fmap (fmap (realToFrac . length)) graph)
                                        , ((gname, ename, Binary), fmap (fmap (const 1))             graph)
                                        ]
                                      | (ename, edgeFilter) <- edgeFilters
                                      , (gname, mkGraph) <- simpleGraphs
                                      , let graph = mkGraph $ edgeFilter edgeDocsSubset
                                      ]

        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname ename wname rname,  graphRanking graph )
            | ((gname, ename, wname), graph) <- simpleWeightedGraphs ++ fancyWeightedGraphs,
              (rname, graphRanking) <- graphRankings
            ] ++ [(CandidateSet, candidateSetList seeds radius binarySymmetricGraph)]

--     in (fancyGraphs, simpleGraphs) `deepseq` computeRankings'
    in computeRankings'

main :: IO ()
main = do
    (articlesFile, outputFilePrefix, embeddingsFile, querySrc, runMethods, expansionHops) <-
        execParser $ info (helper <*> opts) mempty
    annsFile <- AnnsFile.openAnnotations articlesFile
    putStrLn $ "# Running methods: " ++ show runMethods

    SomeWordEmbedding wordEmbeddings <- readGlove embeddingsFile -- "/home/dietz/trec-car/code/lstm-car/data/glove.6B.50d.txt"

    let universeGraph :: UniverseGraph
        !universeGraph = edgeDocsToUniverseGraph $ pagesToEdgeDocs $ AnnsFile.pages annsFile

    putStrLn ("nodes in KB = " <> show (HM.size universeGraph))

    let binarySymmetricGraph :: BinarySymmetricGraph
        !binarySymmetricGraph = universeToBinaryGraph universeGraph

    queriesWithSeedEntities <-
        case querySrc of
          QueriesFromCbor queryFile -> pagesToLeadEntities . decodeCborList <$> BSL.readFile queryFile
          QueriesFromJson queryFile -> do
              QueryDocList queriesWithSeedEntities <- either error id . Data.Aeson.eitherDecode <$> BSL.readFile queryFile
              return queriesWithSeedEntities

    retrieveDocs <- fmap (map (\(a,b)->(b,a))) <$> retrievalRanking "index"

    handles <- sequence $ M.fromList  -- todo if we run different models in parallel, this will overwrite previous results.
      [ (method, openFile (outputFilePrefix ++ showMethodName method ++ ".run") WriteMode >>= newMVar)
      | method <- fromMaybe allMethods runMethods ]
        :: IO (M.Map Method (MVar Handle))

    ncaps <- getNumCapabilities
    let --forM_' = forM_
        forM_' = forConcurrentlyN_ ncaps
            --forM_' xs f = void $ runEffect $ ForkMap.mapIO 16 16 f xs
    forM_' queriesWithSeedEntities $ \query -> do
        when (null $ queryDocLeadEntities query) $
            T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

        T.putStr $ T.pack $ "# Processing query "++ show query++"\n"
        let queryId = queryDocQueryId query
            rankings = computeRankingsForQuery retrieveDocs annsFile (queryDocRawTerms query) (queryDocLeadEntities query) expansionHops
                                  universeGraph binarySymmetricGraph wordEmbeddings

            runMethod :: Method -> [(PageId, Double)] -> IO ()
            runMethod method ranking = do
                let Just hdl = M.lookup method handles

                let logMsg t = T.putStr $ T.pack $ unpackPageId queryId++"\t"++showMethodName method++"\t"++t++"\n"
                    logTimed t doIt = do
                        logMsg t
                        t0 <- getCurrentTime
                        !r <- doIt
                        t1 <- getCurrentTime
                        let dt = t1 `diffUTCTime` t0
                        logMsg $ t++"\ttime="++(showFFloat (Just 3) (realToFrac dt / 60 :: Double) "")
                        return r

                logMsg $ "evaluating"
                --logTimed "evaluating graph" $ evaluate $ rnf graph
                --logMsg $ "graph size: "++show (graphSize graph)
                --ranking <- logTimed "computing ranking" $ evaluate $ force $ computeRanking graph
                logMsg $ "ranking entries="++show (length ranking)
                let formatted = WriteRanking.formatEntityRankings
                                (T.pack $ show method)
                                (T.pack $ unpackPageId queryId)
                                ranking
                bracket (takeMVar hdl) (putMVar hdl) $ \ h ->
                    logTimed "writing ranking" $ TL.hPutStrLn h formatted

        let methodsAvailable = S.fromList (map fst rankings)
            badMethods
              | Just ms <- runMethods = S.fromList ms `S.difference` methodsAvailable
              | otherwise             = S.empty
            filterMethods
              | Just ms <- runMethods = (`elem` ms)
              | otherwise             = const True
        when (not $ S.null badMethods) $ putStrLn $ "\n\nwarning: unknown methods: "++show badMethods++"\n"
        mapM_ (uncurry runMethod) $ filter (filterMethods . fst) rankings

    mapM_ (\h -> takeMVar h >>= hClose) handles

forConcurrentlyN_ :: Foldable f => Int -> f a -> (a -> IO ()) -> IO ()
forConcurrentlyN_ n xs f = do
    sem <- atomically $ newTSem n
    let run x = bracket (atomically $ waitTSem sem) (const $ atomically $ signalTSem sem) (const $ f x)
    forConcurrently_ xs run
