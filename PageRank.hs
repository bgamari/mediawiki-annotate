{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module PageRank
   ( Eigenvector
   , toHashMap
   , toEntries
   , pageRank
   ) where

import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Ix
import Data.Maybe
import Data.Bifunctor

import Dijkstra (Graph(..))

data Matrix

data Eigenvector n a = Eigenvector (n -> NodeId) (NodeId -> n) (A.UArray NodeId a)

-- | A transition matrix
type Transition = A.UArray (NodeId, NodeId)

toHashMap :: (A.IArray A.UArray a, Hashable n, Eq n)
          => Eigenvector n a -> HM.HashMap n a
toHashMap = HM.fromList . toEntries

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


pageRank :: forall n a. (RealFrac a, A.IArray A.UArray a, Eq n, Hashable n)
         => a -> Graph n a -> [Eigenvector n a]
pageRank alpha graph@(Graph nodeMap) =
    let (nodeRange, toNode, fromNode) = computeNodeMapping graph
        numNodes = rangeSize nodeRange

        initial = A.accumArray (const id) (1 / realToFrac numNodes) nodeRange []

        -- normalized flow of nodes flowing into each node
        inbound :: A.Array NodeId [(NodeId, a)]
        inbound = A.accumArray (++) [] nodeRange
                  [ (fromNode v, [(fromNode u, weightUV / weightUSum)])
                  | (u, outEdges) <- HM.toList nodeMap
                  , let !weightUSum = sum $ map snd outEdges
                  , (v, weightUV) <- outEdges
                  ]

        mult :: A.UArray NodeId a -> A.UArray NodeId a
        mult arr = A.accumArray (+) 0 nodeRange
                   [ (v, teleportationSum + sumU)
                   | (v, inEdges) <- A.assocs inbound
                   , let sumU = sum [ aU * normWeight * (1 - alpha)
                                    | (u, normWeight) <- inEdges
                                    , let aU = arr A.! u
                                    ]
                         teleportationSum = alpha / realToFrac numNodes * c
                   ]
          where
            !c = sum $ A.elems arr

    in map (Eigenvector fromNode toNode)
       $ initial : iterate (mult) initial  -- normalization should be optional, but we are paranoid.
{-# SPECIALISE pageRank :: (Eq n, Hashable n) => Double -> Graph n Double -> [Eigenvector n Double] #-}

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


computeNodeMapping
    :: forall n a. (A.IArray A.UArray a, Eq n, Hashable n, Num a)
    => Graph n a
    -> ( (NodeId, NodeId)
       , NodeId -> n
       , n -> NodeId )
computeNodeMapping g@(Graph nodeMap) =
    (nodeRange, toNode, fromNode)
  where
    allNodes = nodes g
    minNodeId = NodeId 0
    maxNodeId = NodeId $ HS.size allNodes - 1
    nodeRange = (minNodeId, maxNodeId)

    toNodeMap :: A.Array NodeId n
    toNodeMap = A.listArray nodeRange (HS.toList allNodes)
    fromNodeMap :: HM.HashMap n NodeId
    fromNodeMap = HM.fromList $ zip (HS.toList allNodes) [NodeId 0..]

    toNode = (toNodeMap A.!)
    fromNode n = fromMaybe (error "PageRank.adjacency.fromNode") $ HM.lookup n fromNodeMap


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