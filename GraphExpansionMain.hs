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
import Control.DeepSeq
import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import Data.Maybe
import Data.Semigroup hiding (All, Any, option)
import Data.Foldable
import Data.Coerce
import Data.List (intercalate)
import Options.Applicative
import System.IO
import Data.Time.Clock
import Numeric
import GHC.Generics
import GHC.TypeLits

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
import qualified ExtractKnowledgeBase as KB

--import qualified Control.Concurrent.ForkMap as ForkMap
import WriteRanking
import Retrieve
import GraphExpansion
import GraphExpansionExperiments
import GloveEmbedding
import ZScore

import Debug.Trace

opts :: Parser (FilePath, FilePath, FilePath, FilePath, Maybe [Method], Int)
opts =
    (,,,,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'q' <> long "outlines file" <> metavar "FILE" <> help "Outline file (queries)")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    <*> option str (short 'e' <> long "embedding" <> metavar "FILE" <> help "Glove embeddings file")
    <*> optional (some (option method $ short 'm' <> long "method" <> metavar "METHOD" <> help "Methods to run"))
    <*> option auto (long "hops" <> metavar "INT" <> help "number of hops for initial outward expansion" <> value 3)
    where
      method = fmap (methodMap M.!) str
      methodMap = M.fromList [ (showMethodName m, m) | m <- allMethods ]




computeRankingsForQuery :: forall n. (KnownNat n)
                        => RankingFunction
                        -> AnnotationsFile
                        -> QueryDoc -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> WordEmbedding n
                        -> [(Method, [(PageId, Double)])]

computeRankingsForQuery rankDocs annsFile queryDoc radius universeGraph binarySymmetricGraph wordEmbedding =
    let seeds :: HS.HashSet PageId
        seeds = queryDocLeadEntities $ queryDoc
        query = queryDocRawTerms $ queryDoc

        nodeSet :: HS.HashSet PageId
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

        fancyGraphs :: [(GraphNames, HM.HashMap PageId [EdgeDocWithScores])]
        fancyGraphs = [(Top5PerNode,  filterGraphByTop5NodeEdges rankDocs query $ edgeDocsSubset)
                      ,(Top100PerGraph,  filterGraphByTop100GraphEdges rankDocs query $ edgeDocsSubset)
                      ]

        simpleGraphs :: [(GraphNames, HM.HashMap PageId (HM.HashMap PageId [EdgeDoc]))]
        simpleGraphs =  [(SimpleGraph, noFilterTwice $ edgeDocsSubset)
                        ,(RandomGraph, random100Filter $ edgeDocsSubset)
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

        fancyWeightedGraphs ::  [((GraphNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        fancyWeightedGraphs =  [((gname, wname), accumulateEdgeWeights graph weighting)
                               | (gname, graph) <- fancyGraphs
                               , (wname, weighting) <- weightings
                               ]

        simpleWeightedGraphs :: [((GraphNames, WeightingNames), HM.HashMap PageId (HM.HashMap PageId Double))]
        simpleWeightedGraphs = concat [
                                         [ ((gname, Count), fmap (fmap (realToFrac . length) )  $ graph)
                                         , ((gname, Binary),      fmap (fmap (const 1))         $ graph)
                                         ]
                                      | (gname, graph) <- simpleGraphs]


        computeRankings' :: [(Method,  [(PageId, Double)])]
        computeRankings' =
            [ (Method gname wname rname,  graphRanking (graph) )
            | ((gname, wname), graph) <- fancyWeightedGraphs ++ simpleWeightedGraphs,
              (rname, graphRanking) <- graphRankings
            ]

--     in (fancyGraphs, simpleGraphs) `deepseq` computeRankings'
    in computeRankings'

main :: IO ()
main = do
    (articlesFile, queryFile, outputFilePrefix, embeddingsFile, runMethods, expansionHops) <-
        execParser $ info (helper <*> opts) mempty
    annsFile <- AnnsFile.openAnnotations articlesFile
    putStrLn $ "# Running methods: " ++ show runMethods

    SomeWordEmbedding wordEmbeddings <- parseGlove embeddingsFile -- "/home/dietz/trec-car/code/lstm-car/data/glove.6B.50d.txt"

    let universeGraph :: UniverseGraph
        !universeGraph = edgeDocsToUniverseGraph $ emitEdgeDocs $ AnnsFile.pages annsFile

    let binarySymmetricGraph :: BinarySymmetricGraph
        !binarySymmetricGraph = universeToBinaryGraph universeGraph


    queriesToSeedEntities <- pagesToLeadEntities . decodeCborList <$> BSL.readFile queryFile

    let queryTermsAll = foldMap (queryDocRawTerms) $ queriesToSeedEntities
    putStrLn $ "# queryTermsAll " ++ show queryTermsAll

    let !corpusStatistics = Retrieve.computeTermCounts queryTermsAll
                          $ map (\edgeDoc -> Doc edgeDoc (edgeDocContent edgeDoc))
                          $ emitEdgeDocs $ AnnsFile.pages annsFile

    putStrLn $ "# corpus statistics " ++ show corpusStatistics

    let rankDoc q docs =
            map (\(Doc a b) -> (a,b))
            $ Retrieve.retrieve corpusStatistics q
            $ map (uncurry Doc) docs

    handles <- sequence $ M.fromList  -- todo if we run different models in parallel, this will overwrite previous results.
      [ (method, openFile (outputFilePrefix ++ showMethodName method ++ ".run") WriteMode)
      | method <- fromMaybe allMethods runMethods ]
        :: IO (M.Map Method Handle)

    ncaps <- getNumCapabilities
    let --forM_' = forM_
        forM_' = forConcurrentlyN_ ncaps
        --forM_' xs f = void $ runEffect $ ForkMap.mapIO 16 16 f xs
    forM_' queriesToSeedEntities $ \query -> do
        when (null $ queryDocLeadEntities query) $
            T.putStr $ T.pack $ "# Query with no lead entities: "++show query++"\n"

        T.putStr $ T.pack $ "# Processing query "++ show query++"\n"
        let queryId = queryDocQueryId query
            rankings = computeRankingsForQuery rankDoc annsFile query expansionHops
                                  universeGraph binarySymmetricGraph wordEmbeddings

            runMethod :: Method -> [(PageId, Double)] -> IO ()
            runMethod method ranking = do
                let hdl = handles M.! method

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
                logTimed "writing ranking" $ TL.hPutStr hdl formatted

        let methodsAvailable = S.fromList (map fst rankings)
            badMethods
              | Just ms <- runMethods = S.fromList ms `S.difference` methodsAvailable
              | otherwise             = S.empty
            filterMethods
              | Just ms <- runMethods = (`elem` ms)
              | otherwise             = const True
        when (not $ S.null badMethods) $ fail $ "unknown methods: "++show badMethods
        mapM_ (uncurry runMethod) $ filter (filterMethods . fst) rankings

    mapM_ hClose handles

forConcurrentlyN_ :: Foldable f => Int -> f a -> (a -> IO ()) -> IO ()
forConcurrentlyN_ n xs f = do
    sem <- atomically $ newTSem n
    let run x = bracket (atomically $ waitTSem sem) (const $ atomically $ signalTSem sem) (const $ f x)
    forConcurrently_ xs run
