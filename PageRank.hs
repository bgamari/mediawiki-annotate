{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module PageRank
   ( Eigenvector
   , toEntries
   , pageRank
   ) where

import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.IntMap as IntMap
import Data.Hashable
import Data.Ix
import Data.Maybe
import Data.Bifunctor
import System.Random

import Dijkstra (Graph(..))

data Matrix

data Eigenvector n a = Eigenvector (n -> NodeId) (NodeId -> n) (A.UArray NodeId a)

-- | A transition matrix
type Transition = A.UArray (NodeId, NodeId)

toMap :: (A.IArray A.UArray a, Hashable n, Eq n)
      => Eigenvector n a -> HM.HashMap n a
toMap (Eigenvector _ toNode arr) =
    HM.fromList [ (toNode n, w)
                | (n, w) <- A.assocs arr
                ]

toEntries :: (A.IArray A.UArray a)
          => Eigenvector n a -> [(n, a)]
toEntries (Eigenvector _ fromNode arr) =
    map (first fromNode) (A.assocs arr)

square x = x * x
norm arr = sum $ map square $ A.elems arr
normalize arr = let n = norm arr in A.amap (/ n) arr

relChange :: (A.IArray A.UArray a, RealFrac a)
          => Eigenvector n a -> Eigenvector n a -> a
relChange (Eigenvector _ _ a) (Eigenvector _ _ b) =
    delta / norm a
  where
    delta = sum $ map square $ zipWith (-) (A.elems a) (A.elems b)


pageRank :: forall n a g. (RealFrac a, RandomGen g, Random a, A.IArray A.UArray a, Eq n, Hashable n)
         => g -> a -> Graph n a -> [Eigenvector n a]
pageRank gen teleportation graph =
    let (adj, nodeRange, toNode, fromNode) = weightedAdjacency graph
        numNodes = rangeSize nodeRange
        mat = addTeleportation nodeRange teleportation
              $ normRows nodeRange
              $ adj

        --initial = normalize $ A.listArray nodeRange (randoms gen) -- Todo uniform vector perturbed with random
        initial = A.accumArray (const id) (1 / realToFrac numNodes) nodeRange []

        mult :: A.UArray NodeId a -> A.UArray NodeId a
        mult arr = A.accumArray (+) 0 nodeRange
                   [ (i, (mat A.! (j,i)) * (arr A.! j))
                   | i <- range nodeRange
                   , j <- range nodeRange
                   ]

    in map (Eigenvector fromNode toNode)
       $ initial : iterate (mult) initial  -- normalization should be optional, but we are paranoid.

nodes :: (Hashable n, Eq n) => Graph n a -> HS.HashSet n
nodes = foldMap (HS.fromList . map fst) . getGraph

newtype NodeId = NodeId Int
               deriving (Eq, Ord, Show, Enum, Ix)


-- | Smooth transition matrix with teleportation:  (1-teleport) X + teleport 1/N
addTeleportation :: (RealFrac a, A.IArray A.UArray a)
                 => (NodeId, NodeId) -> a -> Transition a -> Transition a
addTeleportation nodeRange teleportation =
    A.amap (\w -> (1-teleportation) * w  + teleportation / realToFrac numNodes)
  where numNodes = rangeSize nodeRange

iamap :: (A.IArray a e', A.IArray a e, Ix i)
      => (i -> e' -> e) -> a i e' -> a i e
iamap f arr =
    A.listArray bounds $ zipWith f (range bounds) (A.elems arr)
  where
    bounds = A.bounds arr

-- | normalize rows to sum to one (also handle case of no outedges)
normRows :: forall a. (RealFrac a, A.IArray A.UArray a)
         => (NodeId, NodeId) -> Transition a -> Transition a
normRows nodeRange trans =
    iamap (\(i,j) w ->
        let total = totals A.! i
        in if abs total < 1e-6
             then 1 / realToFrac (rangeSize nodeRange)  -- handle case of no outedges: every node is reachable by 1/N
             else w / total                             -- outedges are normalized to sum to one
        ) trans
  where
    totals :: A.UArray NodeId a
    totals = A.accumArray (+) 0 nodeRange
             [ (i, w)
             | i <- range nodeRange
             , j <- range nodeRange
             , let w = trans A.! (i,j)
             ]


weightedAdjacency :: forall n a. (A.IArray A.UArray a, Eq n, Hashable n, Num a)
          => Graph n a
          -> ( Transition a
             , (NodeId, NodeId)
             , NodeId -> n
             , n -> NodeId )
weightedAdjacency g@(Graph nodeMap) =
    (arr, nodeRange, toNode, fromNode)
  where
    allNodes = nodes g
    minNodeId = NodeId 0
    maxNodeId = NodeId $ HS.size allNodes - 1
    nodeRange = (minNodeId, maxNodeId)

    toNodeMap = IntMap.fromList $ zip [0..] (HS.toList allNodes)
    fromNodeMap = HM.fromList $ zip (HS.toList allNodes) [NodeId 0..]

    toNode (NodeId n) = fromMaybe (error "PageRank.adjacency.toNode") $ IntMap.lookup n toNodeMap
    fromNode n = fromMaybe (error "PageRank.adjacency.fromNode") $ HM.lookup n fromNodeMap

    g' :: HM.HashMap n (HM.HashMap n a)
    g' = fmap HM.fromList nodeMap
    !arr = A.array ((minNodeId, minNodeId), (maxNodeId, maxNodeId))
                   [ ((i,j), weightIJ)
                   | (i, nodeI) <- zip [minNodeId..maxNodeId] (HS.toList allNodes)
                   , (j, nodeJ) <- zip [minNodeId..maxNodeId] (HS.toList allNodes)
                   , let weightIJ = fromMaybe 0 $ HM.lookup nodeI g' >>= HM.lookup nodeJ  --  get the weight of edge i -> j
                   -- HM.lookup nodeI g' ..  means out-edges of node i  -- as a hashmap
                   -- then lookup nodeJ in that hashmap to get the weights
                   ]
    -- spelling out the >>= stuff would result in this:
--     getWeight :: n -> n -> a
--     getWeight nodeI nodeJ =
--         case  HM.lookup nodeI g' of
--           Just outedges ->
--             case HM.lookup nodeJ outedges of
--               Just weight -> weight
--               Nothing -> 0
--           Nothing -> 0



test :: Graph Char Double
test = Graph $ HM.fromList
    [ d0 .= [ d2 .= 1],
      d1 .= [ d1 .= 1, d2 .= 1],
      d2 .= [ d0 .= 1, d2 .= 1, d3 .= 1],
      d3 .= [ d3 .= 1, d4 .= 1],
      d4 .= [ d6 .= 1 ],
      d5 .= [ d5 .= 1, d6 .= 1],
      d6 .= [ d3 .= 1, d4 .= 1, d6 .= 1]
    ]
  where
    [d0,d1,d2,d3,d4,d5,d6] = ['0'..'6']

    a .= b = (a, b)

testW :: Graph Char Double
testW = Graph $ HM.fromList
    [ d0 .= [ d2 .= 0.0001],
      d1 .= [ d1 .= 1, d2 .= 100],
      d2 .= [ d0 .= 1, d2 .= 1, d3 .= 1],
      d3 .= [ d3 .= 100, d4 .= 100],
      d4 .= [  ],
      d5 .= [ d5 .= 1, d6 .= 1],
      d6 .= [ d3 .= 1, d4 .= 0.003, d6 .= 0.002]
    ]
  where
    [d0,d1,d2,d3,d4,d5,d6] = ['0'..'6']

    a .= b = (a, b)