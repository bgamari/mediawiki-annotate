{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Monoid hiding (All, Any)
import Options.Applicative
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable
import Data.Maybe
import Data.Bifunctor

import Dijkstra
import PageRank
import CAR.Utils
import CAR.Types


opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


data KbDoc = KbDoc { kbDocParagraphId :: ParagraphId
                   , kbDocArticleId :: PageId
                   , kbDocSourceEntityId :: PageId
                   , kbDocOutlinkIds ::  [PageId]
                   }
           deriving Show


transformContent :: Page -> [KbDoc]
transformContent (Page pageName' pageId pageSkeleta) =
    foldMap go pageSkeleta
  where
    pageName = normTargetPageName pageName'
    go :: PageSkeleton -> [KbDoc]
    go (Section heading _ children) =
       concatMap go  children
    go (Para paragraph) =
      [convertPara paragraph ]

    convertPara :: Paragraph -> KbDoc
    convertPara paragraph =
      let
        kbDocParagraphId    = paraId $ paragraph
        kbDocArticleId      = pageId
        kbDocSourceEntityId = pageNameToId pageName
        kbDocOutlinks       = fmap (first normTargetPageName) $ paraLinks $ paragraph
        kbDocOutlinkIds     = fmap (pageNameToId . fst) $ kbDocOutlinks
      in KbDoc {..}


dropKbDocsNoLinks :: [KbDoc] -> [KbDoc]
dropKbDocsNoLinks =
    filter (\kbDoc -> not ( null (kbDocOutlinkIds $kbDoc)))

hashEdgeNodes :: KbDoc -> [(PageId, [KbDoc])]
hashEdgeNodes kbDoc =
  [(kbDocSourceEntityId $kbDoc, [kbDoc])] ++ [(target, [kbDoc])  | target <- kbDocOutlinkIds $kbDoc]

type UniverseGraph = HM.HashMap PageId [KbDoc]
hashUniverseGraph :: [Page] -> UniverseGraph
hashUniverseGraph pages = HM.fromListWith (++) $ foldMap hashEdgeNodes
      $ foldMap ( dropKbDocsNoLinks . transformContent)
      $ pages


-- ------------------------------------------------
type BinarySymmetricGraph = HM.HashMap PageId (HS.HashSet PageId)

universeToBinaryGraph :: UniverseGraph -> BinarySymmetricGraph
universeToBinaryGraph universeGraph =
  fmap (foldMap (\kbDoc -> HS.fromList $ [kbDocSourceEntityId $ kbDoc] ++ (kbDocOutlinkIds $kbDoc))) $ universeGraph


expandNodes :: BinarySymmetricGraph -> HS.HashSet PageId -> HS.HashSet PageId
expandNodes binarySymmetricGraph seeds =
  seeds <> foldMap (fromMaybe mempty . (`HM.lookup` binarySymmetricGraph)) seeds

expandNodesK binarySymmetricGraph seeds k =
  iterate (expandNodes binarySymmetricGraph) seeds !! k



-- ------------------------------------------------

-- | Outward weighted hyper-edges
newtype OutWHyperEdges = OutWHyperEdges (HM.HashMap PageId Int)     -- ^ a set of outward wHyperEdges and their weights
        deriving Show
data WHyperEdges = WHyperEdges PageId OutWHyperEdges  -- ^ sourceNode and its outward wHyperEdges
        deriving Show


singleWHyperEdge :: PageId -> OutWHyperEdges
singleWHyperEdge target = OutWHyperEdges $ HM.singleton target 1

type WHyperGraph = HM.HashMap PageId OutWHyperEdges

instance Monoid OutWHyperEdges where
    mempty = OutWHyperEdges mempty
    OutWHyperEdges a `mappend` OutWHyperEdges b = OutWHyperEdges (HM.unionWith (+) a b)



countEdges :: [KbDoc] -> OutWHyperEdges
countEdges kbDocs =
      foldMap (singleWHyperEdge . kbDocSourceEntityId) kbDocs
   <> foldMap (foldMap singleWHyperEdge . kbDocOutlinkIds) kbDocs


lookupNeighbors :: Monoid v =>  HM.HashMap PageId v -> PageId -> v
lookupNeighbors graph node =
    fromMaybe mempty $ HM.lookup node graph


subsetOfUniverseGraph :: UniverseGraph -> [PageId] -> UniverseGraph
subsetOfUniverseGraph universe nodeset =
    foldMap (\node -> HM.singleton node (universe `lookupNeighbors` node) ) $ nodeset


main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
--     withFile outputFile WriteMode $ \h ->
--         BSL.hPutStr h $ Galago.toWarc
--             $ map toGalagoDoc

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

    let wHyperGraph :: WHyperGraph
        wHyperGraph = fmap countEdges $ universeSubset

    putStrLn $ "\nnode set:"
    print    $ nodeSet
    putStrLn $ "\nhyper graph:"
    print    $ wHyperGraph

    putStrLn "\n\n\n"
    let graph = wHyperGraphToGraph wHyperGraph
    mapM_ print [ (n1, n2, shortestPaths (dijkstra graph n1) n2)
                | n1 <- toList seeds
                , n2 <- toList seeds
                ]




wHyperGraphToGraph :: WHyperGraph -> Graph PageId (Sum Double)
wHyperGraphToGraph =
    Graph . fmap (\(OutWHyperEdges x) -> fmap (fmap $ Sum . recip . realToFrac) $ HM.toList x)