{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}

import Control.Monad (when)
import Data.Monoid hiding (All, Any)
import Data.Foldable
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

computeRankingsForQuery :: QueryDoc -> Int -> UniverseGraph -> BinarySymmetricGraph
                        -> (PageId, Methods [(PageId, Double)])
computeRankingsForQuery queryDoc radius universeGraph binarySymmetricGraph =
    let seeds :: HS.HashSet PageId
        seeds = queryDocLeadEntities $ queryDoc
        queryId = queryDocQueryId $ queryDoc

        nodeSet = expandNodesK binarySymmetricGraph seeds radius

        universeSubset ::  HM.HashMap PageId [EdgeDoc]
        universeSubset =  subsetOfUniverseGraph universeGraph $ HS.toList nodeSet
        -- filter universe to nodeSet
        -- then turn into Hyperedges

        wHyperGraph :: WHyperGraph Double
        wHyperGraph = fmap countEdges $ universeSubset

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
        universeGraph = hashUniverseGraph pagesForLinkExtraction

    let binarySymmetricGraph :: BinarySymmetricGraph
        binarySymmetricGraph = universeToBinaryGraph universeGraph


    queriesToSeedEntities <- pagesToLeadEntities . decodeCborList <$> BSL.readFile queryFile

    handles <- mapM (\name -> openFile (outputFilePrefix ++ name ++ ".run") WriteMode) methodNames

    forM_ queriesToSeedEntities $ \query -> do
        when (null $ queryDocLeadEntities query) $
            putStrLn $ "Query with no lead entities: "++show query

        let (queryId, rankings) = computeRankingsForQuery query 3 universeGraph binarySymmetricGraph
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