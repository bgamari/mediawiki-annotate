{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}


import Data.Bits
import Data.Foldable
import Data.Monoid
import Data.List
import Data.Hashable
import Control.Monad (when, replicateM, replicateM_)
import Control.DeepSeq
import GHC.TypeLits
import GHC.Conc
import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import System.IO

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Sort
import qualified Data.Map.Strict as M
import qualified Data.HashSet as HS
import Options.Applicative
import Control.Parallel.Strategies
import System.Random.MWC
import System.Random.MWC.Distributions (standard)

import SimplIR.WordEmbedding
import SimplIR.WordEmbedding.Parse

import CAR.Types
import CAR.Types.Files
import CAR.Utils
import qualified IntSet as IS
import Utils
{-# ANN module ("HLint: ignore Redundant $"::String) #-}


modes = subparser
    $ command "filter"  (info (helper <*> filterparasModes) mempty)
   <> command "test" (info (helper <*> testMode) mempty)

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> modes) mempty
    mode


-- opts :: Parser (FilePath, FilePath)
-- opts = (,)
filterparasModes :: Parser (IO ())
filterparasModes = go
    <$> option auto (long "duplicates" <> metavar "FILE" <> help "True duplicate table for comparison")
    <*> option auto (long "paragraphs" <> metavar "CBOR" <> help "paragraph CBOR")
    <*> optional (option auto (short 'n' <> long "take" <> metavar "INT" <> help "number of edges to include"))
    <*> option auto (short 'o' <> long "output" <> metavar "FILE" <> help "file to write filtered paragraphs to")
  where
    go :: FilePath -> FilePath -> Maybe Int -> FilePath -> IO ()
    go duplicatesFile paragraphsFile nTake outFile = do
        (prov, paras) <- readParagraphsFileWithProvenance paragraphsFile

        edges <- parseDuplicates <$> readFile duplicatesFile
        let duplicateParagraphIds = HS.fromList
                                  $ foldMap (\(a,b) -> [a, b])
                                  $ maybe id take nTake
                                  $ edges

            filteredParagraphs = filter inDuplicates paras
                where inDuplicates para = (paraId para) `HS.member` duplicateParagraphIds
        writeCarFile outFile prov filteredParagraphs



testMode :: Parser (IO ())
testMode = go
    <$> option auto (short 'T' <> long "true-duplicates" <> metavar "FILE" <> help "True duplicate table for comparison")
    <*> option auto (short 't' <> long "test-duplicates" <> metavar "FILE" <> help "True duplicate table for comparison")
  where
    go :: FilePath -> FilePath -> IO ()
    go trueDuplicatesFile testDuplicatesFile = do
        trueEdges <- parseDuplicates <$> readFile trueDuplicatesFile
        testEdges <- parseDuplicates <$> readFile testDuplicatesFile

        let trueEdgeSet = HS.fromList $ [ HS.fromList [a,b] | (a,b) <- trueEdges]
            testEdgeSet = HS.fromList $ [ HS.fromList [a,b] | (a,b) <- testEdges]

            missingEdges = trueEdgeSet `HS.difference` testEdgeSet

        putStrLn $ "Number of missing edges: " <> show (HS.size missingEdges)

        putStrLn $ "Sample: "
                <> unlines
                 ( fmap prettyEdge
                 $ take 5
                 $ HS.toList missingEdges
                 )
          where prettyEdge :: HS.HashSet ParagraphId -> String
                prettyEdge edge =
                  let [a, b] =  HS.toList edge
                  in show a <> " -- " <> show b