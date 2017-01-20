{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad (when)
import Data.Monoid hiding (All, Any)
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


opts :: Parser (FilePath, FilePath, FilePath)
opts =
    (,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'q' <> long "outlines file" <> metavar "FILE" <> help "Outline file (queries)")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


data QueryDoc = QueryDoc { queryDocQueryId :: PageId
                         , queryDocQueryText :: PageName
                         , queryDocLeadEntities ::  HS.HashSet PageId
                         }
           deriving Show

pagesToLeadEntities :: [Page] -> [QueryDoc]
pagesToLeadEntities pages =
        map (\page -> let kbDoc = KB.transformContent inlinkCounts page
                      in QueryDoc { queryDocQueryId      = KB.kbDocPageId kbDoc
                                  , queryDocQueryText    = pageName page
                                  , queryDocLeadEntities = HS.fromList $ fmap pageNameToId $ KB.kbDocOutLinks kbDoc
                                  }
            )

        $ pages
      where
        inlinkInfo   = KB.collectInlinkInfo pages
        inlinkCounts = KB.resolveRedirects inlinkInfo
        inlinkTotals = mconcat $ HM.elems inlinkCounts

data Methods a = Methods { pageRankU, pageRankW, pathU, pathW :: a }
               deriving (Show, Functor, Foldable, Traversable)

instance Applicative Methods where
    pure x = Methods x x x x
    Methods a b c d <*> Methods w x y z =
        Methods (a w) (b x) (c y) (d z)

methodNames :: Methods String
methodNames = Methods { pageRankU = "page-rank-u"
                      , pageRankW = "page-rank-w"
                      , pathU     = "path-u"
                      , pathW     = "path-w"
                      }

-- | weight edges by number of (source,target) occurrences in edgeDocs
countEdges :: RankingFunction -> [EdgeDoc] -> OutWHyperEdges Double
countEdges _rankDocs edgeDocs =
      foldMap (foldMap singleWHyperEdge . edgeDocNeighbors) edgeDocs



-- | Only consider top 5 edgeDocs by query likelihood
-- | Then: weight edges by number of (source, target) occurrences in edgeDocs
top5Edges :: RankingFunction -> T.Text -> [EdgeDoc] -> OutWHyperEdges Double
top5Edges rankDocs query edgeDocs =
      foldMap (foldMap singleWHyperEdge . edgeDocNeighbors)
      $ fmap fst
      $ take 5
      $ rankDocs query
      $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent edgeDoc ))
      $ edgeDocs


-- | (source, target) edgeweight = sum _kbdocs rscore(kbdoc)
rankWeightEdgeDocs :: RankingFunction -> T.Text -> [EdgeDoc] -> OutWHyperEdges Double
rankWeightEdgeDocs rankDocs query edgeDocs =
      let ranking = rankDocs query
                  $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent edgeDoc ))
                  $ edgeDocs
      in fold [ singleWHyperEdgeWithWeight score target
              | (edgeDoc, score) <- ranking
              , target <- edgeDocNeighbors edgeDoc ]



-- | Filter the whole graph by only considering edgeDocs in the top of the ranking
rankFilterGraph :: RankingFunction
                -> T.Text -> HM.HashMap PageId [EdgeDoc] -> HM.HashMap PageId [EdgeDoc]
rankFilterGraph rankDocs query graph =
    let edgeDocs = HS.toList $ HS.fromList $ fold graph -- all kbdocs in this graph
        topRankingEdgeDocs = fmap fst
                           $ take 100
                           $ rankDocs query
                           $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent $ edgeDoc))
                           $ edgeDocs

    in edgeDocsToUniverseGraph topRankingEdgeDocs

type RankingFunction = forall elem. T.Text -> [(elem, T.Text)] -> [(elem, Double)]

computeRankingsForQuery :: RankingFunction
                        -> QueryDoc -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> (PageId, Methods [(PageId, Double)])
computeRankingsForQuery rankDocs queryDoc radius universeGraph binarySymmetricGraph =
    let seeds :: HS.HashSet PageId
        seeds = queryDocLeadEntities $ queryDoc
        queryId = queryDocQueryId $ queryDoc
        queryText =  getPageName $ queryDocQueryText $ queryDoc

        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset = subsetOfUniverseGraph universeGraph nodeSet
--         universeSubset = foldMap top5Edges universeSubset'  -- Todo This actually belongs up here
--         universeSubset = foldMap rankWeightEdgeDocs universeSubset'  -- Todo which one to use?
--         universeSubset = rankFilterGraph universeSubset'

        -- filter universe to nodeSet
        -- then turn into Hyperedges

        wHyperGraph :: WHyperGraph Double
--         wHyperGraph = fmap countEdges $ rankFilterGraph queryText universeSubset  -- Todo Which one to use?
        wHyperGraph = fmap (top5Edges rankDocs queryText) universeSubset                          -- Todo Which one to use?
--         wHyperGraph = rankWeightEdgeDocs queryText universeSubset                 -- Todo Which one to use?


        unwGraph =
            fmap (fmap (const 1)) $ wHyperGraph


        graph = wHyperGraphToGraph wHyperGraph
        ugraph = wHyperGraphToGraph unwGraph

        rankings = Methods { pageRankW = rankByPageRank graph 0.15 20
                           , pageRankU = rankByPageRank ugraph 0.15 20
                           , pathW     = rankByShortestPaths (coerce graph) (toList seeds)
                           , pathU     = rankByShortestPaths (coerce ugraph) (toList seeds)
                           }

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

    let queryTexts = T.unwords $ map (getPageName . queryDocQueryText) $ queriesToSeedEntities
    let corpusStatistics = Retrieve.computeTermCounts queryTexts
                          $ map (\edgeDoc -> Doc edgeDoc (edgeDocContent edgeDoc))
                          $ emitEdgeDocs pagesForLinkExtraction
        rankDoc q docs =
            map (\(Doc a b) -> (a,b))
            $ Retrieve.retrieve corpusStatistics q
            $ map (uncurry Doc) docs

    handles <- mapM (\name -> openFile (outputFilePrefix ++ name ++ ".run") WriteMode) methodNames

    forM_ queriesToSeedEntities $ \query -> do
        when (null $ queryDocLeadEntities query) $
            putStrLn $ "Query with no lead entities: "++show query

        let (queryId, rankings) = computeRankingsForQuery rankDoc query 3
                                  universeGraph binarySymmetricGraph
            formattedRankings :: Methods TL.Text
            formattedRankings =
                WriteRanking.formatEntityRankings
                    <$> fmap T.pack methodNames
                    <*> pure (T.pack $ unpackPageId queryId)
                    <*> rankings

            actions :: Methods (IO ())
            actions =
                 TL.hPutStr
             <$> handles
             <*> formattedRankings

        sequence_ actions

    mapM_ hClose handles
