{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module PageRank
   ( Eigenvector
   , toHashMap
   , toEntries
   , pageRank
   , pageRankWithSeeds
   ) where

import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Ix
import Data.Bifunctor

import DenseMapping
import Graph

data Eigenvector n a = Eigenvector (DenseMapping n) (A.UArray (DenseId n) a)

-- | A transition matrix
type Transition n = A.UArray (DenseId n, DenseId n)

toHashMap :: (A.IArray A.UArray a, Hashable n, Eq n)
          => Eigenvector n a -> HM.HashMap n a
toHashMap = HM.fromList . toEntries

toEntries :: (A.IArray A.UArray a)
          => Eigenvector n a -> [(n, a)]
toEntries (Eigenvector mapping arr) =
    map (first $ fromDense mapping) (A.assocs arr)

square x = x * x
norm arr = sum $ map square $ A.elems arr
normalize arr = let n = norm arr in A.amap (/ n) arr

relChange :: (A.IArray A.UArray a, RealFrac a)
          => Eigenvector n a -> Eigenvector n a -> a
relChange (Eigenvector _ a) (Eigenvector _ b) =
    delta / norm a
  where
    delta = sum $ map square $ zipWith (-) (A.elems a) (A.elems b)

-- | Plain PageRank with uniform teleportation.
--
-- Solving for the principle eigenvector of the transport operator,
-- \[
-- a_{ij} =  (1-\alpha) \frac{e_{ij}}{\sum_j e_{ij}} + \frac{\alpha}{N}
-- \]
-- given a graph with edge weights \(e_{ij}\).
pageRank
    :: forall n a. (RealFrac a, A.IArray A.UArray a, Eq n, Hashable n, Show n)
    => a                  -- ^ teleportation probability \(\alpha\)
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
pageRank alpha = pageRankWithSeeds alpha 0 HS.empty

-- | Personalized PageRank.
--
-- Solving for the principle eigenvector of the transport operator,
-- \[
-- a_{ij} =  (1-\alpha-\beta) \frac{e_{ij}}{\sum_j e_{ij}} + \frac{\alpha}{N} + b_j
-- \]
-- where
-- \[
-- b_j =
-- \begin{cases}
--   \frac{\beta}{\vert \mathcal{S}\vert} & \mathrm{if} j \in \mathcal{S} \\
--   0 & \mathrm{otherwise}
-- \end{cases}
-- \]
-- given a graph with edge weights \(e_{ij}\) and a seed node set
-- \(\mathcal{S}\).
pageRankWithSeeds
    :: forall n a. (RealFrac a, A.IArray A.UArray a, Eq n, Hashable n, Show n)
    => a                  -- ^ uniform teleportation probability \(\alpha\)
    -> a                  -- ^ seed teleportation probability \(\beta\)
    -> HS.HashSet n       -- ^ seed node set
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
pageRankWithSeeds alpha beta seeds graph@(Graph nodeMap) =
    let mapping  = mkDenseMapping (nodeSet graph)
        nodeRng  = denseRange mapping
        numNodes = rangeSize nodeRng
        numSeeds = HS.size seeds

        initial = A.accumArray (const id) (1 / realToFrac numNodes) nodeRng []

        -- normalized flow of nodes flowing into each node
        inbound :: A.Array (DenseId n) [(DenseId n, a)]
        inbound = A.accumArray (++) [] nodeRng
                  [ ( toDense mapping v,
                      [(toDense mapping u, weightUV / weightUSum)]
                    )
                  | (u, outEdges) <- HM.toList nodeMap
                  , let !weightUSum = sum $ map snd outEdges
                  , (v, weightUV) <- outEdges
                  ]

        nextiter :: A.UArray (DenseId n) a -> A.UArray (DenseId n) a
        nextiter pagerank = A.accumArray (+) 0 nodeRng
                   [ (v, teleportationSum + outlinkSum + seedTeleportSum)
                   | (v, inEdges) <- A.assocs inbound
                   , let outlinkSum = sum [ uPR * normWeight * (1 - alpha - beta')
                                          | (u, normWeight) <- inEdges
                                          , let uPR = pagerank A.! u
                                          ]
                         teleportationSum = alpha / realToFrac numNodes * c
                         seedTeleportSum = beta' / realToFrac numSeeds * c
                         beta'
                           | fromDense mapping v `HS.member` seeds = beta
                           | otherwise = 0
                   ]
          where
            !c = sum $ A.elems pagerank

    in map (Eigenvector mapping)
       $ initial : iterate nextiter initial
{-# SPECIALISE pageRankWithSeeds
                   :: (Eq n, Hashable n, Show n)
                   => Double -> Double -> HS.HashSet n
                   -> Graph n Double -> [Eigenvector n Double] #-}


-- | Smooth transition matrix with teleportation:  (1-teleport) X + teleport 1/N
addTeleportation :: (RealFrac a, A.IArray A.UArray a)
                 => (DenseId n, DenseId n) -> a
                 -> Transition n a -> Transition n a
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
normRows :: forall a n. (RealFrac a, A.IArray A.UArray a)
         => (DenseId n, DenseId n) -> Transition n a -> Transition n a
normRows nodeRange trans =
    iamap (\(i,j) w ->
        let total = totals A.! i
        in if abs total < 1e-6
             then 1 / realToFrac (rangeSize nodeRange)  -- handle case of no outedges: every node is reachable by 1/N
             else w / total                             -- outedges are normalized to sum to one
        ) trans
  where
    totals :: A.UArray (DenseId n) a
    totals = A.accumArray (+) 0 nodeRange
             [ (i, w)
             | i <- range nodeRange
             , j <- range nodeRange
             , let w = trans A.! (i,j)
             ]

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
