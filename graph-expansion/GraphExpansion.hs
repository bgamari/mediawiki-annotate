{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module GraphExpansion where

import Data.Monoid hiding (All, Any)
import Data.Foldable
import Data.Maybe
import Data.Ix
import GHC.TypeLits

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector.Indexed as VI

import Graph
import Dijkstra
import PageRank
import EdgeDocCorpus
import CAR.Utils
import CAR.Types
import CAR.Retrieve

import qualified SimplIR.Term as Term
import AttriRank
import SimplIR.WordEmbedding
import ZScore (Attributes(..))

type UniverseGraph = HM.HashMap PageId [EdgeDoc]

edgeDocsToUniverseGraph :: [EdgeDoc] -> UniverseGraph
edgeDocsToUniverseGraph edgeDocs =
    HM.fromListWith (++)
    $ foldMap symmetrizeEdge edgeDocs
  where
    symmetrizeEdge :: EdgeDoc -> [(PageId, [EdgeDoc])]
    symmetrizeEdge edgeDoc =
           [ (target, [edgeDoc])
           | target <- edgeDocNeighbors edgeDoc]

computeTextEmbedding :: KnownNat n => WordEmbedding n -> T.Text -> WordVec n
computeTextEmbedding wordEmbedding text =
    mconcat $ mapMaybe toWordVec
    $ fmap Term.toText $ textToTokens' text
  where
    toWordVec x = x `HM.lookup` wordEmbedding

wordVecToAttributes :: WordVec n -> Attributes (EmbeddingDim n)
wordVecToAttributes = Attrs . VI.map realToFrac . unWordVec

pageTextEmbeddingAttributes :: KnownNat n => WordEmbedding n -> Page
                            -> WordVec n
pageTextEmbeddingAttributes wordEmbedding (Page pageName pageId pageSkeleta) =
     mconcat  -- merge wordvectors per skeleton
     $ fmap (computeTextEmbedding wordEmbedding)
     $ mconcat $ pageText
    where
      pageText = fmap (map TL.toStrict . pageSkeletonText) pageSkeleta

-- ------------------------------------------------
type BinarySymmetricGraph = HM.HashMap PageId (HS.HashSet PageId)

universeToBinaryGraph :: UniverseGraph -> BinarySymmetricGraph
universeToBinaryGraph universeGraph =
  fmap (foldMap (\edgeDoc -> HS.fromList $ (edgeDocNeighbors $edgeDoc))) $ universeGraph


expandNodes :: BinarySymmetricGraph -> HS.HashSet PageId -> HS.HashSet PageId
expandNodes binarySymmetricGraph seeds =
  seeds <> foldMap (fromMaybe mempty . (`HM.lookup` binarySymmetricGraph)) seeds

expandNodesK :: BinarySymmetricGraph -> HS.HashSet PageId -> Int -> HS.HashSet PageId
expandNodesK binarySymmetricGraph seeds k =
  iterate (expandNodes binarySymmetricGraph) seeds !! k



-- ------------------------------------------------



lookupNeighbors :: Monoid v =>  HM.HashMap PageId v -> PageId -> v
lookupNeighbors graph node =
    fromMaybe mempty $ HM.lookup node graph


subsetOfUniverseGraph :: UniverseGraph -> HS.HashSet PageId -> UniverseGraph
subsetOfUniverseGraph universe nodeset =
    foldMap (\node -> HM.singleton node (map pruneEdges $ universe `lookupNeighbors` node) ) $ nodeset
  where
    -- Throw out neighbors not in our subgraph
    pruneEdges :: EdgeDoc -> EdgeDoc
    pruneEdges edoc = edoc { edgeDocNeighbors = filter (`HS.member` nodeset) (edgeDocNeighbors edoc) }

rankByPageRank :: Graph PageId Double -> Double -> Int -> [(PageId, Double)]
rankByPageRank graph teleport iterations =
  let pr = (!! iterations)  $ PageRank.pageRank teleport graph
      prRanking  =  PageRank.toEntries $ pr
  in prRanking

rankByPersonalizedPageRank :: Graph PageId Double -> Double -> HS.HashSet PageId -> Int -> [(PageId, Double)]
rankByPersonalizedPageRank graph teleport seeds iterations
  | HM.null $ getGraph graph `HM.intersection` HS.toMap seeds = []
  | otherwise =
  let pr = (!! iterations) $ PageRank.persPageRankWithSeeds 0 teleport seeds graph
      prRanking  =  PageRank.toEntries $ pr
  in prRanking

-- gamma dDist nodeAttrs graph
rankByAttriPageRank :: Ix t
                    => Graph PageId Double -> Double -> Int
                    -> HM.HashMap PageId (Attributes t) -> Int -> [(PageId, Double)]
rankByAttriPageRank graph teleport numAttrs nodeAttrs iterations =
  let gamma = 1/(realToFrac numAttrs)
      getAttr n = fromMaybe (error $ "rankByAttriPageRank: no attributes for "++show n)
                  $ HM.lookup n nodeAttrs
      pr = snd $ (!! iterations) $ AttriRank.attriRank gamma AttriRank.Uniform getAttr graph
      prRanking  =  PageRank.toEntries $ pr
  in prRanking


filterEdges :: (PageId -> PageId -> Bool)
            -> Graph.Graph PageId w -> Graph.Graph PageId w
filterEdges pred (Graph.Graph graph ) =
    Graph.Graph $ HM.mapWithKey f $ graph
  where f :: PageId -> [(PageId, w)] -> [(PageId, w) ]
        f source =
          filter (\(target, _) -> pred source target)

rankByShortestPaths :: Graph.Graph PageId (Sum Double) -> [PageId] -> [(PageId, Double)]
rankByShortestPaths graph seeds =
    let seeds' = HS.fromList seeds
        graph' = filterEdges (\s t -> not (s `HS.member` seeds' && t `HS.member` seeds'))  $ graph
        shortestPaths =  [ (n1, n2, Dijkstra.shortestPaths paths n2)
                         | n1 <- toList seeds
                         , let paths = Dijkstra.dijkstra (graph') n1
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



toGraph :: HM.HashMap PageId (HM.HashMap PageId Double) -> Graph PageId Double
toGraph graph =
    Graph $ fmap HM.toList graph
