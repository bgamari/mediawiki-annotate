{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Options.Applicative
import Data.Monoid
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import PageRank
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
    (Just 0 :: Maybe Int, edgeDocs) <- readCborList inPath
    let uniGraph = edgeDocsToUniverseGraph edgeDocs
        binGraph = universeToBinaryGraph uniGraph
        graph :: Graph PageId Double
        graph = Graph $ fmap (HM.fromList . flip zip (repeat 1) . HS.toList) binGraph
        res = pageRank 0 graph
    --mapM_ (print . take 100 . map snd . toEntries) $ res
    writeFileSerialise outPath $ toEntries $ head $ drop 20 res
