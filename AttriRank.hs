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
   , attriRank
   ) where

import Control.Exception (assert)
import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict as HM
import Data.Foldable as F
import Data.Hashable
import Data.Ix
import Data.Maybe
import Data.Bifunctor
import Prelude hiding (pi)

import ZScore
import NodeMapping
import Dijkstra (Graph(..))

data DDist = Uniform
           | Beta Double Double

attriRank :: forall n t a. (a ~ Double, Show n, Eq n, Hashable n, Ix t)
          => a     -- ^ the RBF kernel parameter, \(\gamma\)
          -> DDist -- ^ distribution over \(d\).
          -> HM.HashMap n (Attributes t)
          -> Graph n a
          -> [(a, A.UArray NodeId a)]
attriRank _ _ attrs _ | HM.null attrs = error "attriRank: No attributes"
attriRank gamma dDist nodeAttrs graph =
    -- See Algorithm 1
    let mapping = mkNodeMapping graph
        attrRange@(attr0,attr1) = A.bounds $ getAttrs $ head $ HM.elems nodeAttrs
        nNodes = rangeSize attrRange
        --ws :: HM.HashMap n a
        --ws = fmap (\(Attrs attr) -> exp $ negate $ gamma * quadrance attr) attrs
        ws :: A.UArray NodeId a
        ws = A.array (nodeRange mapping)
             [ (i, exp $ negate $ gamma * quadrance attr)
             | (n, Attrs attr) <- HM.toList nodeAttrs
             , let i = fromNode mapping n ]
        !a = sum $ A.elems ws
        b :: A.UArray t Double
        b = (2 * gamma) *^
            A.accumArray (+) 0 attrRange
            [ (attr, v * wj)
            | (j, Attrs attrs) <- HM.toList nodeAttrs
            , let wj = ws A.! fromNode mapping j
            , (attr, v) <- A.assocs attrs
            ]
        c :: A.UArray (t,t) a
        c = (2 * squared gamma) *!
            A.accumArray (+) 0 ((attr0,attr0), (attr1,attr1))
            [ ((n,m), wj * xn * xm)
            | (j, Attrs xj) <- HM.toList nodeAttrs
            , let wj = ws A.! fromNode mapping j
            , (n, xn) <- A.assocs xj
            , (m, xm) <- A.assocs xj
            ]
        rs :: A.UArray NodeId a
        rs = A.array (nodeRange mapping)
             [ (i, wi * (a + xi ^*^ b + xi ^*^ (c !*^ xi)))
             | (i, wi) <- A.assocs ws
             , let Just (Attrs xi) = HM.lookup (toNode mapping i) nodeAttrs
             ]
        rVec :: A.UArray NodeId a
        rVec = let z = foldl' (+) 0 (A.elems rs) in z *^ rs

        -- generate P matrix
        outDegree :: HM.HashMap n Int
        outDegree = fmap length (getGraph graph)

        p :: [Edge]
        p = [ Edge i' (fromNode mapping j) v
            | (i, js) <- HM.toList (getGraph graph)
            , let i' = fromNode mapping i
            , (j, _) <- js
            , let v = case fromMaybe 0 $ HM.lookup j outDegree of
                        0  -> 1 / realToFrac nNodes
                        n  -> 1 / realToFrac n
            ]
        pi0 = case dDist of
                Uniform         -> 0.5 *^ rVec
                Beta alpha beta -> (beta / (alpha + beta)) *^ rVec

        go :: Int -> A.UArray NodeId a -> A.UArray NodeId a
           -> [(Double, A.UArray NodeId a)]
        go k rho pi = (norm rho, pi')  : go (k+1) rho' pi'
          where
            k' = realToFrac k
            rho' = case dDist of
                     Uniform         -> (k' / (k'+2)) *^ pRho
                     Beta alpha beta -> ((k'+alpha-1) / (k'+alpha+beta)) *^ pRho
            pi' = pi ^+^ rho'
            pRho = A.accumArray (+) 0 (nodeRange mapping)
                   [ (i, v * rhoJ)
                   | Edge i j v <- p
                   , let rhoJ = rho A.! j
                   ]
    in (norm pi0, pi0) : go 1 pi0 pi0

data Edge = Edge !NodeId !NodeId !Double

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
