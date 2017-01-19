{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}


import Data.Monoid hiding (All, Any)
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import Data.Coerce
import Options.Applicative

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Dijkstra
import PageRank
import CAR.Utils
import CAR.Types

import WriteRanking

import GraphExpansion


opts :: Parser (FilePath, FilePath, FilePath)
opts =
    (,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'q' <> long "outlines file" <> metavar "FILE" <> help "Outline file (queries)")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


main :: IO ()
main = do
    (articlesFile, outlinesFile, outputFilePrefix) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile articlesFile


    let universeGraph :: UniverseGraph
        universeGraph = hashUniverseGraph pages

    let binarySymmetricGraph :: BinarySymmetricGraph
        binarySymmetricGraph = universeToBinaryGraph universeGraph

    -- Todo do for each query:
    let seeds :: HS.HashSet PageId
        seeds = HS.fromList [ "Thorium", "Plutonium",  "Burnup"]
--         seeds = HS.fromList [ "Cladding%20(nuclear%20fuel)"]

    let nodeSet = expandNodesK binarySymmetricGraph seeds 1


    let universeSubset ::  HM.HashMap PageId [KbDoc]
        universeSubset =  subsetOfUniverseGraph universeGraph $ HS.toList nodeSet
        -- filter universe to nodeSet
        -- then turn into Hyperedges

    let wHyperGraph :: WHyperGraph Double
        wHyperGraph = fmap countEdges $ universeSubset

        unwGraph =
            fmap (fmap (const 1)) $ wHyperGraph
--             [ (sourceId, [ (targetId, 1)
--                          | (targetId, w) <- outEdges
--                          ]
--             queryId)  )
--             | (sourceId, outEdges) <- wHyperGraph
--             ]


    putStrLn $ "\nnode set:"
    print    $ nodeSet
    putStrLn $ "\nhyper graph:"
    print    $ wHyperGraph

    let graph = wHyperGraphToGraph wHyperGraph


--     putStrLn $ "\n\nPageRank on weighted graph: \n" <> unlines (take 20 $ map show $ toRanking $ rankByPageRank graph 0.15 20)
--
--     putStrLn $ "\n\nPageRank unweighted: \n" <> unlines (take 20 $ map show $ toRanking $ rankByPageRank (wHyperGraphToGraph unwGraph) 0.15 20)
--
--     putStrLn $ "\n\nshortest path ranking: \n" <> unlines (take 20 $ map show $ toRanking $ rankByShortestPaths (coerce graph) (toList seeds) )
--
--     putStrLn $ "\n\nunweighted shortest path: \n" <> unlines (take 20 $ map show $ toRanking $ rankByShortestPaths (coerce $ wHyperGraphToGraph unwGraph) (toList seeds) )

    let write runname ranking =
          writeEntityRanking (outputFilePrefix ++ runname ++ ".run") (T.pack runname) (T.pack "Spent%20nuclear%20fuel") $ ranking

        ugraph = wHyperGraphToGraph unwGraph

    write "w-pageRank" $ rankByPageRank graph 0.15 20
    write "u-pageRank" $ rankByPageRank ugraph 0.15 20
    write "w-path"     $ rankByShortestPaths (coerce graph) (toList seeds)
    write "u-path"     $ rankByShortestPaths (coerce ugraph) (toList seeds)

