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
   , uniformInitial
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

uniformInitial :: (Hashable n, Eq n, VG.Vector VU.Vector a, Fractional a)
               => DenseMapping n -> Eigenvector n a
uniformInitial mapping =
    Eigenvector mapping $ VI.replicate (denseRange mapping) (1 / realToFrac numNodes)
  where
    !numNodes = rangeSize (denseRange mapping)
{-# SPECIALISE uniformInitial
                 :: (Hashable n, Eq n) => DenseMapping n -> Eigenvector n Double #-}

-- | Plain PageRank with uniform teleportation.
--
-- Solving for the principle eigenvector of the transport operator,
-- \[
-- a_{ij} =  (1-\alpha) \frac{e_{ij}}{\sum_j e_{ij}} + \frac{\alpha}{N}
-- \]
-- given a graph with edge weights \(e_{ij}\).
pageRank
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\)
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
pageRank alpha = persPageRankWithSeeds alpha 0 HS.empty
{-# SPECIALISE pageRank
                   :: (Eq n, Hashable n, Show n)
                   => Double
                   -> Graph n Double -> [Eigenvector n Double] #-}

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
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> a                  -- ^ teleportation probability \(\beta\) to be uniformly distributed
                          -- across the seeds
    -> HS.HashSet n       -- ^ seed node set
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
persPageRankWithSeeds alpha beta seeds graph =
    persPageRankWithSeedsAndInitial mapping initial alpha seeds' graph
  where
    !mapping = mkDenseMapping (nodeSet graph)
    !initial = uniformInitial mapping
    seeds' = w <$ HS.toMap seeds
      where w = beta / realToFrac (HS.size seeds)
{-# SPECIALISE persPageRankWithSeeds
                   :: (Eq n, Hashable n, Show n)
                   => Double -> Double -> HS.HashSet n
                   -> Graph n Double -> [Eigenvector n Double] #-}

-- | Like 'persPageRankWithSeeds' but allowing the weight of each seed to be
-- given independently. Note that \( \alpha + \sum_i \beta_i \) must sum to less
-- than one.
persPageRankWithNonUniformSeeds
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> HM.HashMap n a     -- ^ teleportation probability \(\beta\) for each seed
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
persPageRankWithNonUniformSeeds alpha seeds graph =
    persPageRankWithSeedsAndInitial mapping initial alpha seeds graph
  where
    !mapping  = mkDenseMapping (nodeSet graph)
    !initial = uniformInitial mapping
{-# SPECIALISE persPageRankWithNonUniformSeeds
    :: (Eq n, Hashable n, Show n, HasCallStack)
    => Double
    -> HM.HashMap n Double
    -> Graph n Double
    -> [Eigenvector n Double] #-}

-- | Like 'persPagerankWithSeeds' but allowing the user to specify a
-- 'DenseMapping' and an initial principle eigenvector.
persPageRankWithSeedsAndInitial
    :: forall n a. (RealFloat a, VU.Unbox a, VG.Vector VU.Vector a, Eq n, Hashable n, Show n, Show a, HasCallStack)
    => DenseMapping n
    -> Eigenvector n a
    -> a                  -- ^ teleportation probability \(\alpha\) to be uniformly distributed
    -> HM.HashMap n a     -- ^ teleportation probability \(\beta\) for each seed
    -> Graph n a          -- ^ the graph
    -> [Eigenvector n a]  -- ^ principle eigenvector iterates
persPageRankWithSeedsAndInitial _ _ alpha seeds _
  | not (alpha + sum seeds <= 1) = error $ unlines
                               [ "persPageRank: teleportation probability exceeds 1."
                               , "alpha = " <> show alpha
                               , "seeds = " <> show seeds
                               ]

persPageRankWithSeedsAndInitial _ _ _ _ (Graph nodeMap)
  | not $ null badEdges
  = error $ unlines $
    [ "persPageRank: negative edge weights"
    , ""
    ] ++ map show badEdges
  where badEdges = [ (u,v,weight)
                   | (u, outEdges) <- HM.toList nodeMap
                   , (v, weight) <- HM.toList outEdges
                   , not $ weight >= 0
                   ]

persPageRankWithSeedsAndInitial mapping initial alpha seeds graph@(Graph nodeMap)
  | numNodes == 0 = error "persPageRank: no nodes"

  | uncovered <- HS.toMap (nodeSet graph) `HM.difference` toDenseMap mapping
  , not $ HM.null uncovered
  = error $ "persPageRank: graph nodes not covered by dense mapping: "<>show (HM.keys uncovered)

  | uncovered <- seeds `HM.difference` toDenseMap mapping
  , not $ HM.null uncovered
  = error $ "persPageRank: seed nodes not covered by dense mapping: "<>show (HM.keys uncovered)

  | otherwise =
    let -- normalized flow of nodes flowing into each node
        inbound :: VI.Vector V.Vector (DenseId n) (HM.HashMap (DenseId n) a)
        !inbound = VI.accum' nodeRng (HM.unionWith (+)) mempty -- TODO mappend?
                  [ ( toDense mapping v,
                      HM.singleton (toDense mapping u) (weightUV / weightUSum)
                    )
                  | (u, outEdges) <- HM.toList nodeMap
                  , let !weightUSum = sum outEdges
                  , (v, weightUV) <- HM.toList outEdges
                  , if weightUSum < 1e-14
                    then error ("persPageRank: zero sum" ++ show outEdges)
                    else weightUV > 0
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

    in initial : map (Eigenvector mapping . checkNaN) (iterate nextiter (eigenvectorValues initial))
  where
    checkNaN xs
      | VU.any isNaN $ VI.vector xs = error $ unlines $
        [ "persPageRank: NaN in result"
        , ""
        , "alpha = " ++ show alpha
        , "seeds = " ++ show seeds
        , "graph = " ++ show graph
        , "eigenvector = " ++ show xs
        ]
      | otherwise = xs
    !nodeRng  = denseRange mapping
    !numNodes = rangeSize nodeRng
{-# SPECIALISE persPageRankWithSeedsAndInitial
                   :: (Eq n, Hashable n, Show n)
                   => DenseMapping n
                   -> Eigenvector n Double
                   -> Double
                   -> HM.HashMap n Double
                   -> Graph n Double -> [Eigenvector n Double] #-}

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
