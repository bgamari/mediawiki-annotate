{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module PageRank
   ( -- * Principle eigenvector representation
     Eigenvector(..)
   , relChange
   , toHashMap
   , toEntries
     -- * PageRank
   , pageRank
     -- * Personalized PageRank
   , persPageRankWithSeeds
   , persPageRankWithNonUniformSeeds
   , persPageRankWithSeedsAndInitial
   ) where

import GHC.Stack

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Indexed as VI
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Monoid
import Data.Hashable
import Data.Ix
import Data.Bifunctor

import DenseMapping
import Graph

data Eigenvector n a = Eigenvector { eigenvectorMapping :: !(DenseMapping n)
                                   , eigenvectorValues  :: !(VI.Vector VU.Vector (DenseId n) a)
                                   }

-- | A transition matrix
type Transition n = VI.Vector VU.Vector (DenseId n, DenseId n)

toHashMap :: (VG.Vector VU.Vector a, Hashable n, Eq n)
          => Eigenvector n a -> HM.HashMap n a
toHashMap = HM.fromList . toEntries

toEntries :: (VG.Vector VU.Vector a)
          => Eigenvector n a -> [(n, a)]
toEntries (Eigenvector mapping arr) =
    map (first $ fromDense mapping) (VI.assocs arr)

normalize arr = VI.map (/ n) arr
  where n = case VI.norm arr of
              0 -> error "PageRank.normalize: zero"
              n -> n

relChange :: (VG.Vector VU.Vector a, RealFrac a)
          => Eigenvector n a -> Eigenvector n a -> a
relChange (Eigenvector _ a) (Eigenvector _ b) =
    delta / VI.quadrance a
  where
    delta = VI.sum $ VI.map square $ VI.zipWith (-) a b
    square x = x*x

-- | Plain PageRank with uniform teleportation.
--
-- Solving for the principle eigenvector of the transport operator,
-- \[
-- a_{ij} =  (1-\alpha) \frac{e_{ij}}{\sum_j e_{ij}} + \frac{\alpha}{N}
-- \]
-- given a graph with edge weights \(e_{ij}\).
pageRank
    :: forall n a. (RealFrac a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\)
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
pageRank alpha = persPageRankWithSeeds alpha 0 HS.empty

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
persPageRankWithSeeds
    :: forall n a. (RealFrac a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> a                  -- ^ teleportation probability \(\beta\) to be uniformly distributed
                          -- across the seeds
    -> HS.HashSet n       -- ^ seed node set
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
persPageRankWithSeeds alpha beta seeds graph =
    persPageRankWithSeedsAndInitial mapping initial alpha seeds' graph
  where
    !mapping  = mkDenseMapping (nodeSet graph)
    !numNodes = rangeSize (denseRange mapping)
    !initial = VI.replicate (denseRange mapping) (1 / realToFrac numNodes)
    seeds' = w <$ HS.toMap seeds
      where w = beta / realToFrac (HS.size seeds)

-- | Like 'persPageRankWithSeeds' but allowing the weight of each seed to be
-- given independently. Note that \( \alpha + \sum_i \beta_i \) must sum to less
-- than one.
persPageRankWithNonUniformSeeds
    :: forall n a. (RealFrac a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> HM.HashMap n a     -- ^ teleportation probability \(\beta\) for each seed
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
persPageRankWithNonUniformSeeds alpha seeds graph =
    persPageRankWithSeedsAndInitial mapping initial alpha seeds graph
  where
    !mapping  = mkDenseMapping (nodeSet graph)
    !numNodes = rangeSize (denseRange mapping)
    !initial = VI.replicate (denseRange mapping) (1 / realToFrac numNodes)

-- | Like 'persPagerankWithSeeds' but allowing the user to specify a
-- 'DenseMapping' and an initial principle eigenvector.
persPageRankWithSeedsAndInitial
    :: forall n a. (RealFrac a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => DenseMapping n
    -> VI.Vector VU.Vector (DenseId n) a
    -> a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> HM.HashMap n a     -- ^ teleportation probability \(\beta\) for each seed
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
persPageRankWithSeedsAndInitial _ _ alpha seeds _
  | alpha + sum seeds > 1 = error $ unlines
                               [ "persPageRank: teleportation probability exceeds 1."
                               , "alpha = " <> show alpha
                               , "seeds = " <> show seeds
                               ]

persPageRankWithSeedsAndInitial mapping initial alpha seeds graph@(Graph nodeMap)
  | otherwise =
    let !nodeRng  = denseRange mapping
        !numNodes = rangeSize nodeRng

        -- normalized flow of nodes flowing into each node
        inbound :: VI.Vector V.Vector (DenseId n) (HM.HashMap (DenseId n) a)
        !inbound = VI.accum' nodeRng (HM.unionWith (+)) mempty -- TODO mappend?
                  [ ( toDense mapping v,
                      HM.singleton (toDense mapping u) (weightUV / weightUSum)
                    )
                  | (u, outEdges) <- HM.toList nodeMap
                  , let !weightUSum = sum outEdges
                  , (v, weightUV) <- HM.toList outEdges
                  , weightUV > 0
                  ]

        nextiter :: VI.Vector VU.Vector (DenseId n) a -> VI.Vector VU.Vector (DenseId n) a
        nextiter pagerank = VI.accum' nodeRng (+) 0
                   [ (v, outlinkSum + teleport)
                   | (v, inEdges) <- VI.assocs inbound
                   , let !outlinkSum = sum [ uPR * normWeight * (1 - alpha - beta)
                                           | (u, normWeight) <- HM.toList inEdges
                                           , let uPR = pagerank VI.! u
                                           ]
                         -- probability mass added by teleportation
                         !teleport = (teleportation VI.! v) * c
                   ]
          where
            !c = VI.sum pagerank

        beta = sum seeds

        teleportation :: VI.Vector VU.Vector (DenseId n) a
        teleportation = VI.accum' nodeRng (+) (alpha / realToFrac numNodes)
            [ (toDense mapping n, w)
            | (n, w) <- HM.toList seeds
            ]

    in map (Eigenvector mapping)
       $ initial : iterate nextiter initial
{-# SPECIALISE persPageRankWithSeeds
                   :: (Eq n, Hashable n, Show n)
                   => Double -> Double -> HS.HashSet n
                   -> Graph n Double -> [Eigenvector n Double] #-}

-- | Smooth transition matrix with teleportation:  (1-teleport) X + teleport 1/N
addTeleportation :: (RealFrac a, VG.Vector VU.Vector a)
                 => (DenseId n, DenseId n) -> a
                 -> Transition n a -> Transition n a
addTeleportation nodeRange teleportation =
    VI.map (\w -> (1-teleportation) * w  + teleportation / realToFrac numNodes)
  where numNodes = rangeSize nodeRange

-- | normalize rows to sum to one (also handle case of no outedges)
normRows :: forall a n. (RealFrac a, VG.Vector VU.Vector a)
         => (DenseId n, DenseId n) -> Transition n a -> Transition n a
normRows nodeRange trans =
    VI.imap (\(i,j) w ->
        let total = totals VI.! i
        in if abs total < 1e-6
             then 1 / realToFrac (rangeSize nodeRange)  -- handle case of no outedges: every node is reachable by 1/N
             else w / total                             -- outedges are normalized to sum to one
        ) trans
  where
    totals :: VI.Vector VU.Vector (DenseId n) a
    totals = VI.accum' nodeRange (+) 0
             [ (i, w)
             | i <- range nodeRange
             , j <- range nodeRange
             , let w = trans VI.! (i,j)
             ]

test :: Graph Char Double
test = Graph $ fmap HM.fromList $ HM.fromList
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
testW = Graph $ fmap HM.fromList $ HM.fromList
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
