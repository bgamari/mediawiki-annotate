{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-- | The AttriRank algorithm due to,
--
-- * Hsu, Lai, Chen, Feng, Lin. "Unsupervised Ranking using Graph Structures and
-- * Node Attributes." (WSDM 2017)
--
module AttriRank
   ( Attributes(..)
   , DDist(..)
   , attriRank
   ) where

import Control.Exception (assert)
import qualified Data.Array as A (Array)
import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A (UArray)
import qualified Data.HashMap.Strict as HM
import Data.Foldable as F
import Data.Hashable
import Data.Ix
import Data.Maybe
import Data.Bifunctor
import Prelude hiding (pi)

import ZScore
import DenseMapping
import Graph
import PageRank (Eigenvector(..))

data DDist = Uniform
           | Beta Double Double

attriRank :: forall n t a. (a ~ Double, Show n, Eq n, Hashable n, Ix t)
          => a     -- ^ the RBF kernel parameter, \(\gamma\)
          -> DDist -- ^ distribution over \(d\).
          -> (n -> Attributes t)
          -> Graph n a
          -> [(a, Eigenvector n a)]
attriRank _ _ _ graph | nullGraph graph = error "attriRank: null graph"
attriRank gamma dDist getAttrs graph =
    -- See Algorithm 1
    let mapping :: DenseMapping n
        mapping = mkDenseMapping (nodeSet graph)
        nNodes = rangeSize (denseRange mapping)

        attrRange :: (t, t)
        attrRange@(attr0,attr1) = A.bounds $ unAttrs $ getAttrs $ head $ elems mapping

        attrs :: A.Array (DenseId n) (Attributes t)
        attrs = A.listArray (denseRange mapping) $ map getAttrs $ elems mapping

        ws :: A.UArray (DenseId n) a
        ws = A.array (denseRange mapping)
             [ (i, exp $ negate $ gamma * quadrance attr)
             | i <- range (denseRange mapping)
             , let Attrs attr = attrs A.! i ]
        !a = sum $ A.elems ws
        b :: A.UArray t Double
        b = (2 * gamma) *^
            A.accumArray (+) 0 attrRange
            [ (attr, v * wj)
            | (j, Attrs attrs) <- A.assocs attrs
            , let wj = ws A.! j
            , (attr, v) <- A.assocs attrs
            ]
        c :: A.UArray (t,t) a
        c = (2 * squared gamma) *!
            A.accumArray (+) 0 ((attr0,attr0), (attr1,attr1))
            [ ((n,m), wj * xn * xm)
            | (j, Attrs xj) <- A.assocs attrs
            , let wj = ws A.! j
            , (n, xn) <- A.assocs xj
            , (m, xm) <- A.assocs xj
            ]
        rs :: A.UArray (DenseId n) a
        rs = A.array (denseRange mapping)
             [ (i, wi * (a + xi ^*^ b + xi ^*^ (c !*^ xi)))
             | (i, wi) <- A.assocs ws
             , let Attrs xi = attrs A.! i
             ]
        rVec :: A.UArray (DenseId n) a
        rVec = let z = foldl' (+) 0 (A.elems rs) in z *^ rs

        -- generate P matrix
        outDegree :: HM.HashMap n Double
        outDegree = fmap (sum . map snd) (getGraph graph)

        p :: [Edge n]
        p = [ Edge i' (toDense mapping j) v
            | (i, js) <- HM.toList (getGraph graph)
            , let i' = toDense mapping i
            , (j, edgeW) <- js
            , let v = case HM.lookup j outDegree of
                        Nothing     -> 1 / realToFrac nNodes
                        Just outDeg -> edgeW / outDeg
            ]
        pi0 = case dDist of
                Uniform         -> 0.5 *^ rVec
                Beta alpha beta -> (beta / (alpha + beta)) *^ rVec

        go :: Int -> A.UArray (DenseId n) a -> A.UArray (DenseId n) a
           -> [(Double, A.UArray (DenseId n) a)]
        go k rho pi = (norm rho, pi')  : go (k+1) rho' pi'
          where
            k' = realToFrac k
            rho' = case dDist of
                     Uniform         -> (k' / (k'+2)) *^ pRho
                     Beta alpha beta -> ((k'+alpha-1) / (k'+alpha+beta)) *^ pRho
            pi' = pi ^+^ rho'
            pRho = A.accumArray (+) 0 (denseRange mapping)
                   [ (i, v * rhoJ)
                   | Edge i j v <- p
                   , let rhoJ = rho A.! j
                   ]
    in map (second $ Eigenvector mapping) $ (norm pi0, pi0) : go 1 pi0 pi0

data Edge n = Edge !(DenseId n) !(DenseId n) !Double

(^+^) :: (Num a, Ix i, A.IArray arr a) => arr i a -> arr i a -> arr i a
x ^+^ y =
    assert (A.bounds x == A.bounds y)
    $ A.listArray (A.bounds x)
      [ a + b
      | (a, b) <- zip (A.elems x) (A.elems y) ]

(^*^) :: (Num a, Ix i, A.IArray arr a) => arr i a -> arr i a -> a
x ^*^ y =
    assert (A.bounds x == A.bounds y)
    $ foldl' (+) 0
    $ zipWith (*) (A.elems x) (A.elems y)

(*^) :: (Num a, Ix i, A.IArray arr a) => a -> arr i a -> arr i a
s *^ x = A.amap (s *) x

(*!) :: (Num a, Ix i, Ix j, A.IArray arr a) => a -> arr (i,j) a -> arr (i,j) a
s *! m = A.amap (s *) m

(!*^) :: (Num a, Ix i, Ix j, A.IArray arr a) => arr (i,j) a -> arr j a -> arr i a
m !*^ x =
    A.accumArray (+) 0 (bimap fst fst $ A.bounds m)
    [ (i, a*b)
    | ((i,j), a) <- A.assocs m
    , let b = x A.! j
    ]

squared :: Num a => a -> a
squared x = x * x

quadrance :: (Num a, Ix i, A.IArray arr a) => arr i a -> a
quadrance = foldl' f 0 . A.elems
  where f acc x = acc + squared x

norm :: (RealFloat a, Ix i, A.IArray arr a) => arr i a -> a
norm = sqrt . quadrance


graph :: Graph Char Double
graph = Graph $ HM.fromList
    [ a .= [ b .= 1, c .= 2, d .= 10, e .= 1, f .= 1 ]
    , b .= [ a .= 1, c .= 1 ]
    , c .= [ a .= 2, b .= 1, d .= 1, e .= 1 ]
    , d .= [ c .= 1, a .= 10 ]
    , e .= [ a .= 1, c .= 1 ]
    , f .= [ a .= 1 ]
    ]
  where
    [a,b,c,d,e,f] = ['a'..'f']

    a .= b = (a, b)

graphAttrs :: HM.HashMap Char (Attributes Int)
graphAttrs = HM.fromList
    [ 'a' .= attrs [0,1,4,2,1]
    , 'b' .= attrs [2,0,0,3,1]
    , 'c' .= attrs [1,3,5,2,0]
    , 'd' .= attrs [0,3,1,5,1]
    , 'e' .= attrs [4,5,2,1,2]
    , 'f' .= attrs [2,4,1,2,3]
    ]
  where
    attrs = Attrs . A.listArray (0,4)
    a .= b = (a, b)
