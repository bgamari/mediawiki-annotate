{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

import Debug.Trace

import Control.Monad (when)
import Data.Monoid hiding (All, Any)
import Data.Functor.Compose
import Data.Foldable
import Data.Coerce
import Options.Applicative
import System.IO

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import CAR.Types
import qualified ExtractKnowledgeBase as KB

import WriteRanking
import Retrieve
import GraphExpansion
import Dijkstra (Graph)


opts :: Parser (FilePath, FilePath, FilePath)
opts =
    (,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'q' <> long "outlines file" <> metavar "FILE" <> help "Outline file (queries)")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


data QueryDoc = QueryDoc { queryDocQueryId :: PageId
                         , queryDocQueryText :: PageName
                         , queryDocLeadEntities ::  HS.HashSet PageId
                         , queryDocRawTerms :: [Term]
                         }
           deriving Show

pagesToLeadEntities :: [Page] -> [QueryDoc]
pagesToLeadEntities pages =
        map (\page -> let kbDoc = KB.transformContent inlinkCounts page
                      in QueryDoc { queryDocQueryId        = KB.kbDocPageId kbDoc
                                  , queryDocQueryText      = pageName page
                                  , queryDocLeadEntities   = HS.fromList $ fmap pageNameToId $ KB.kbDocOutLinks kbDoc
                                  , queryDocRawTerms       = textToTokens' $ getPageName $ pageName page
                                  }
            )

        $ pages
      where
        inlinkInfo   = KB.collectInlinkInfo pages
        inlinkCounts = KB.resolveRedirects inlinkInfo
        inlinkTotals = mconcat $ HM.elems inlinkCounts

type (:.) f g = Compose f g

type Rankings = Methods :. (Weighted :. Graphs)

liftMethods :: Methods a -> Rankings a
liftMethods = Compose . fmap pure

liftWeighteds :: Weighted a -> Rankings a
liftWeighteds = Compose . pure . Compose . fmap pure

liftGraphs :: Graphs a -> Rankings a
liftGraphs = Compose . pure . Compose . pure

data Methods a = Methods { pageRank, path :: a }
               deriving (Show, Functor, Foldable, Traversable)

instance Applicative Methods where
    pure x = Methods x x
    Methods a b <*> Methods w x = Methods (a w) (b x)

methodNames :: Methods String
methodNames = Methods { pageRank = "pr"
                      , path     = "path"
                      }


data Graphs a = Graphs { merelyCountEdges, countTop100Edges, top5 , rank100EdgesInGraph  :: a}
                deriving (Show, Functor, Foldable, Traversable)


instance Applicative Graphs where
    pure x = Graphs x x x x
    Graphs a b c d <*> Graphs w x y z =
        Graphs (a w) (b x) (c y) (d z)

graphNames :: Graphs String
graphNames = Graphs { merelyCountEdges = "countEdges"
                    , countTop100Edges = "countTop100Edges"
                    , top5 = "top5"
                    , rank100EdgesInGraph = "rank100EdgesInGraph"
                    }

data Weighted a = Weighted { weighted, unweighted :: a }
                deriving (Show, Functor, Foldable, Traversable)


instance Applicative Weighted where
    pure x = Weighted x x
    Weighted a b  <*> Weighted w x =
        Weighted (a w) (b x)

weightedNames :: Weighted String
weightedNames = Weighted "weighted" "unweighted"

-- | weight edges by number of (source,target) occurrences in edgeDocs
countEdges ::  [EdgeDoc] -> OutWHyperEdges Double
countEdges edgeDocs =
      foldMap (foldMap singleWHyperEdge . edgeDocNeighbors) edgeDocs



-- | Only consider top 5 edgeDocs by query likelihood
-- | Then: weight edges by number of (source, target) occurrences in edgeDocs
top5Edges :: RankingFunction -> [Term] -> [EdgeDoc] -> OutWHyperEdges Double
top5Edges rankDocs query edgeDocs =
      foldMap (foldMap singleWHyperEdge . edgeDocNeighbors)
      $ fmap fst
      $ take 5
      $ rankDocs query
      $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent edgeDoc ))
      $ edgeDocs


-- | (source, target) edgeweight = sum _kbdocs rscore(kbdoc)
rankWeightEdgeDocsRecipRank :: RankingFunction -> [Term] -> [EdgeDoc] -> OutWHyperEdges Double
rankWeightEdgeDocsRecipRank rankDocs query edgeDocs =
      let !ranking = rankDocs query
                  $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent edgeDoc ))
                  $ edgeDocs
      in fold [ singleWHyperEdgeWithWeight (1.0/realToFrac rank) target
              | ((edgeDoc, score), rank) <- zip ranking [(1::Int)..]
              , target <- edgeDocNeighbors edgeDoc ]

-- Todo clean up this mess

rankWeightEdgeDocs = rankWeightEdgeDocsRecipRank


-- | (source, target) edgeweight = sum _kbdocs rscore(kbdoc)
rankWeightEdgeDocsScore :: RankingFunction -> [Term] -> [EdgeDoc] -> OutWHyperEdges Double
rankWeightEdgeDocsScore rankDocs query edgeDocs =
      let !ranking = rankDocs query
                  $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent edgeDoc ))
                  $ edgeDocs
      in fold [ singleWHyperEdgeWithWeight score target
              | (edgeDoc, score) <- ranking
              , target <- edgeDocNeighbors edgeDoc ]



-- | Filter the whole graph by only considering edgeDocs in the top of the ranking
rankFilterGraph :: RankingFunction
                -> [Term] -> HM.HashMap PageId [EdgeDoc] -> HM.HashMap PageId [EdgeDoc]
rankFilterGraph rankDocs query graph =
    let edgeDocs = HS.toList $ HS.fromList $ fold graph -- all kbdocs in this graph
        topRankingEdgeDocs = fmap fst
                           $ take 100
                           $ rankDocs query
                           $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent $ edgeDoc))
                           $ edgeDocs

    in edgeDocsToUniverseGraph topRankingEdgeDocs

type RankingFunction = forall elem. [Term] -> [(elem, T.Text)] -> [(elem, Double)]

computeRankingsForQuery :: RankingFunction
                        -> QueryDoc -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> (PageId, Rankings [(PageId, Double)])
computeRankingsForQuery rankDocs queryDoc radius universeGraph binarySymmetricGraph =
    let seeds :: HS.HashSet PageId
        seeds = queryDocLeadEntities $ queryDoc
        queryId = queryDocQueryId $ queryDoc
        queryTerms =  queryDocRawTerms $ queryDoc

        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset = subsetOfUniverseGraph universeGraph nodeSet
--         universeSubset = foldMap top5Edges universeSubset'  -- Todo This actually belongs up here
--         universeSubset = foldMap rankWeightEdgeDocs universeSubset'  -- Todo which one to use?
--         universeSubset = rankFilterGraph universeSubset'

        -- filter universe to nodeSet
        -- then turn into Hyperedges

        wHyperGraph :: WHyperGraph Double
        wHyperGraph = fmap countEdges $  universeSubset  -- Todo Which one to use?
--         wHyperGraph = fmap countEdges $ rankFilterGraph rankDocs queryTerms universeSubset  -- Todo Which one to use?
--         wHyperGraph = fmap (top5Edges rankDocs queryTerms) universeSubset                          -- Todo Which one to use?
        --wHyperGraph = trace ("subset size = "++show (fmap length universeSubset)) $ fmap (rankWeightEdgeDocs rankDocs queryTerms) universeSubset                 -- Todo Which one to use?

        graphs :: Graphs (WHyperGraph Double)
        graphs = Graphs { merelyCountEdges    = fmap countEdges $ universeSubset
                        , countTop100Edges    = fmap countEdges $ rankFilterGraph rankDocs queryTerms universeSubset
                        , top5                = fmap (top5Edges rankDocs queryTerms) universeSubset
                        , rank100EdgesInGraph = fmap (rankWeightEdgeDocs rankDocs queryTerms) universeSubset
                        }

        weighteds :: Weighted (WHyperGraph Double -> Graph PageId Double)
        weighteds = Weighted { weighted   = wHyperGraphToGraph
                             , unweighted = wHyperGraphToGraph . fmap (fmap (const 1)) }

        methods :: Methods (Graph PageId Double -> [(PageId, Double)])
        methods = Methods { pageRank = \graph -> rankByPageRank graph 0.15 20
                          , path     = \graph -> rankByShortestPaths (fmap (max $ Sum 0.001) $ coerce graph) (toList seeds)
                          }

        rankings :: Rankings [(PageId, Double)]
        rankings = liftMethods methods <*> (liftWeighteds weighteds <*> liftGraphs graphs)
    in (queryId, rankings)

main :: IO ()
main = do
    (articlesFile, queryFile, outputFilePrefix) <- execParser $ info (helper <*> opts) mempty
    pagesForLinkExtraction <- decodeCborList <$> BSL.readFile articlesFile

    let universeGraph :: UniverseGraph
        universeGraph = edgeDocsToUniverseGraph $ emitEdgeDocs pagesForLinkExtraction

    let binarySymmetricGraph :: BinarySymmetricGraph
        binarySymmetricGraph = universeToBinaryGraph universeGraph


    queriesToSeedEntities <- pagesToLeadEntities . decodeCborList <$> BSL.readFile queryFile

    let queryTermsAll = foldMap (queryDocRawTerms) $ queriesToSeedEntities
    putStrLn $ "queryTermsAll " ++ show queryTermsAll

    let corpusStatistics = Retrieve.computeTermCounts queryTermsAll
                          $ map (\edgeDoc -> Doc edgeDoc (edgeDocContent edgeDoc))
                          $ emitEdgeDocs pagesForLinkExtraction

    putStrLn $ "corpus statistics " ++ show corpusStatistics

    let rankDoc q docs =
            map (\(Doc a b) -> (a,b))
            $ Retrieve.retrieve corpusStatistics q
            $ map (uncurry Doc) docs

    let rankingNames :: Rankings String
        rankingNames = (\weighted graph method -> concat [weighted,"-",graph,"-",method,"-"])
                    <$> liftWeighteds weightedNames
                    <*> liftGraphs graphNames
                    <*> liftMethods methodNames

    handles <- mapM (\name -> openFile (outputFilePrefix ++ name ++ ".run") WriteMode) rankingNames

    forM_ queriesToSeedEntities $ \query -> do
        when (null $ queryDocLeadEntities query) $
            putStrLn $ "Query with no lead entities: "++show query

        putStrLn $ "Processing query "++ show query
        let (queryId, rankings) = computeRankingsForQuery rankDoc query 3
                                  universeGraph binarySymmetricGraph
            formattedRankings :: Rankings TL.Text
            formattedRankings =
                WriteRanking.formatEntityRankings
                    <$> fmap T.pack rankingNames
                    <*> pure (T.pack $ unpackPageId queryId)
                    <*> rankings

            actions :: Rankings (IO ())
            actions =
                 TL.hPutStr
             <$> handles
             <*> formattedRankings

        sequence_ actions

    mapM_ hClose handles
