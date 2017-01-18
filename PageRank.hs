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
pageRank gen teleportation g =
    let (adj, nodeRange, toNode, fromNode) = adjacency g
        mat = A.amap (+teleportation) adj


        initial = normalize $ A.listArray nodeRange (randoms gen)

        mult :: A.UArray NodeId a -> A.UArray NodeId a
        mult arr = A.accumArray (+) 0 nodeRange
                   [ (i, (adj A.! (i,j)) * (arr A.! i))
                   | i <- range nodeRange
                   , j <- range nodeRange
                   ]

    in map (Eigenvector fromNode toNode)
       $ initial : iterate (normalize . mult) initial

nodes :: (Hashable n, Eq n) => Graph n a -> HS.HashSet n
nodes = foldMap (HS.fromList . map fst) . getGraph

newtype NodeId = NodeId Int
               deriving (Eq, Ord, Show, Enum, Ix)

adjacency :: forall n a. (A.IArray A.UArray a, Eq n, Hashable n, Num a)
          => Graph n a -> ( A.UArray (NodeId, NodeId) a
                          , (NodeId, NodeId)
                          , NodeId -> n
                          , n -> NodeId )
adjacency g@(Graph nodeMap) =
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
                   [ ((i,j), v)
                   | (i, m) <- zip [minNodeId..maxNodeId] (HS.toList allNodes)
                   , (j, n) <- zip [minNodeId..maxNodeId] (HS.toList allNodes)
                   , let v = fromMaybe 0 $ HM.lookup m g' >>= HM.lookup n
                   ]