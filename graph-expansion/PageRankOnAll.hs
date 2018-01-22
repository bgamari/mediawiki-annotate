{-# LANGUAGE TypeApplications #-}

import Options.Applicative
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import PageRank
import EdgeDocCorpus
import Graph
import GraphExpansion
import CAR.Types
import CAR.Types.CborList
import Codec.Serialise

opts :: Parser (FilePath, FilePath)
opts =
    (,) <$> option str (long "output" <> help "output path (CBOR eigenvector representation)")
        <*> argument str (help "input EdgeDoc list path")

main :: IO ()
main = do
    (outPath, inPath) <- execParser $ info (helper <*> opts) mempty
    ((), edgeDocs) <- readCborList inPath
    let uniGraph = edgeDocsToUniverseGraph edgeDocs
        binGraph = universeToBinaryGraph uniGraph
        graph :: Graph PageId Float
        graph = Graph $ fmap (HM.fromList . flip zip (repeat 0) . HS.toList) binGraph
        res = pageRank 0 graph
    writeFileSerialise outPath $ toEntries $ head $ drop 10 res
