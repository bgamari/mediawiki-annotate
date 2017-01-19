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

-- import ExtractKnowledgeBase as KB

import WriteRanking

opts :: Parser (FilePath, FilePath, FilePath)
opts =
    (,,)
    <$> argument str (help "articles file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'q' <> long "outlines file" <> metavar "FILE" <> help "Outline file (queries)")
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
newtype OutWHyperEdges weight = OutWHyperEdges (HM.HashMap PageId weight)     -- ^ a set of outward wHyperEdges and their weights
        deriving (Show, Functor)
data WHyperEdges weight = WHyperEdges PageId (OutWHyperEdges weight) -- ^ sourceNode and its outward wHyperEdges
        deriving (Show, Functor)


singleWHyperEdge :: Num weight => PageId -> OutWHyperEdges weight
singleWHyperEdge target = OutWHyperEdges $ HM.singleton target 1

type WHyperGraph weight = HM.HashMap PageId (OutWHyperEdges weight)

instance Num weight => Monoid (OutWHyperEdges weight) where
    mempty = OutWHyperEdges mempty
    OutWHyperEdges a `mappend` OutWHyperEdges b = OutWHyperEdges (HM.unionWith (+) a b)



countEdges :: (Num weight) => [KbDoc] -> OutWHyperEdges weight
countEdges kbDocs =
      foldMap (singleWHyperEdge . kbDocSourceEntityId) kbDocs
   <> foldMap (foldMap singleWHyperEdge . kbDocOutlinkIds) kbDocs


lookupNeighbors :: Monoid v =>  HM.HashMap PageId v -> PageId -> v
lookupNeighbors graph node =
    fromMaybe mempty $ HM.lookup node graph


subsetOfUniverseGraph :: UniverseGraph -> [PageId] -> UniverseGraph
subsetOfUniverseGraph universe nodeset =
    foldMap (\node -> HM.singleton node (universe `lookupNeighbors` node) ) $ nodeset


rankByPageRank :: Graph PageId Double -> Double -> Int -> [(PageId, Double)]
rankByPageRank graph teleport iterations =
  let pr = (!! iterations)  $ PageRank.pageRank teleport graph
      prRanking  =  PageRank.toEntries $ pr
  in prRanking


rankByShortestPaths :: Dijkstra.Graph PageId (Sum Double) -> [PageId] -> [(PageId, Double)]
rankByShortestPaths graph seeds =
    let shortestPaths =  [ (n1, n2, Dijkstra.shortestPaths paths n2)
                         | n1 <- toList seeds
                         , let paths = Dijkstra.dijkstra (graph) n1
                         , n2 <- toList seeds
                         ]

        pathRanking = shortestPathsToNodeScores shortestPaths
    in pathRanking
takeMiddle :: Seq.Seq a -> Seq.Seq a
takeMiddle s =
    case Seq.viewl s of
      Seq.EmptyL  -> mempty
      _ Seq.:< s' ->
       case Seq.viewr s' of
         Seq.EmptyR   -> mempty
         s'' Seq.:> _ -> s''

shortestPathsToNodeScores :: [(PageId, PageId, [Dijkstra.Path PageId])] -> [(PageId, Double)]
shortestPathsToNodeScores paths =
    let innerPaths :: [Seq.Seq PageId]
        innerPaths = fmap takeMiddle [ Seq.fromList path
                                     | (_, _, pathList) <- paths
                                     , path <- pathList
                                     ]

        numPaths = realToFrac $ length innerPaths
    in HM.toList
     $ HM.fromListWith (+) [ (elem, 1 / numPaths)
                           | path <- innerPaths
                           , elem <- toList path
                           ]

wHyperGraphToGraph :: WHyperGraph Double -> Graph PageId Double
wHyperGraphToGraph =
    Graph . fmap (\(OutWHyperEdges x) -> fmap (fmap $ recip . realToFrac) $ HM.toList x)



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

