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
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Indexed as VI
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
        attrRange@(attr0,attr1) = VI.bounds $ unAttrs $ getAttrs $ head $ elems mapping

        attrs :: VI.Vector V.Vector (DenseId n) (Attributes t)
        attrs = VI.fromList (denseRange mapping) $ map getAttrs $ elems mapping

        ws :: VI.Vector VU.Vector (DenseId n) a
        ws = VI.convert $ VI.map (\(Attrs attr) -> exp $ negate $ gamma * VI.quadrance attr) attrs

        !a = VI.sum ws
        b :: VI.Vector VU.Vector t Double
        b = (2 * gamma) *^
            VI.accum' attrRange (+) 0
            [ (attr, v * wj)
            | (j, Attrs attrs) <- VI.assocs attrs
            , let wj = ws VI.! j
            , (attr, v) <- VI.assocs attrs
            ]
        c :: VI.Vector VU.Vector (t,t) a
        c = (2 * gamma * gamma) *!
            VI.accum' ((attr0,attr0), (attr1,attr1)) (+) 0
            [ ((n,m), wj * xn * xm)
            | (j, Attrs xj) <- VI.assocs attrs
            , let wj = ws VI.! j
            , (n, xn) <- VI.assocs xj
            , (m, xm) <- VI.assocs xj
            ]
        rs :: VI.Vector VU.Vector (DenseId n) a
        rs = let f wi (Attrs xi) = wi * (a + xi ^*^ b + xi ^*^ (c !*^ xi))
             in VI.convert $ VI.zipWith f (VI.convert ws) attrs

        rVec :: VI.Vector VU.Vector (DenseId n) a
        rVec = let z = VI.sum rs in z *^ rs

        -- generate P matrix
        outDegree :: HM.HashMap n Double
        outDegree = fmap sum (getGraph graph)

        p :: [Edge n]
        p = [ Edge i' (toDense mapping j) v
            | (i, js) <- HM.toList (getGraph graph)
            , let i' = toDense mapping i
            , (j, edgeW) <- HM.toList js
            , let v = case HM.lookup j outDegree of
                        Nothing     -> 1 / realToFrac nNodes
                        Just outDeg -> edgeW / outDeg
            ]
        pi0 = case dDist of
                Uniform         -> 0.5 *^ rVec
                Beta alpha beta -> (beta / (alpha + beta)) *^ rVec

        go :: Int -> VI.Vector VU.Vector (DenseId n) a
           -> VI.Vector VU.Vector (DenseId n) a
           -> [(Double, VI.Vector VU.Vector (DenseId n) a)]
        go k rho pi = (VI.norm rho, pi')  : go (k+1) rho' pi'
          where
            k' = realToFrac k
            rho' = case dDist of
                     Uniform         -> (k' / (k'+2)) *^ pRho
                     Beta alpha beta -> ((k'+alpha-1) / (k'+alpha+beta)) *^ pRho
            pi' = pi ^+^ rho'
            pRho = VI.accum' (denseRange mapping) (+) 0
                   [ (i, v * rhoJ)
                   | Edge i j v <- p
                   , let rhoJ = rho VI.! j
                   ]
    in map (second $ Eigenvector mapping) $ (VI.norm pi0, pi0) : go 1 pi0 pi0

data Edge n = Edge !(DenseId n) !(DenseId n) !Double

(^+^) :: (Num a, Ix i, VG.Vector arr a)
      => VI.Vector arr i a -> VI.Vector arr i a -> VI.Vector arr i a
x ^+^ y = VI.zipWith (+) x y

(^*^) :: (Num a, Ix i, VG.Vector arr a)
      => VI.Vector arr i a -> VI.Vector arr i a -> a
x ^*^ y = VI.sum $ VI.zipWith (*) x y

(*^) :: (Num a, Ix i, VG.Vector arr a)
     => a -> VI.Vector arr i a -> VI.Vector arr i a
s *^ x = VI.map (s *) x

(*!) :: (Num a, Ix i, Ix j, VG.Vector arr a)
     => a -> VI.Vector arr (i,j) a -> VI.Vector arr (i,j) a
s *! m = VI.map (s *) m

(!*^) :: (Num a, Ix i, Ix j, VG.Vector arr a)
      => VI.Vector arr (i,j) a -> VI.Vector arr j a -> VI.Vector arr i a
m !*^ x =
    VI.accum' (bimap fst fst $ VI.bounds m) (+) 0
    [ (i, a*b)
    | ((i,j), a) <- VI.assocs m
    , let b = x VI.! j
    ]

graph :: Graph Char Double
graph = Graph $ fmap HM.fromList $ HM.fromList
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
    attrs = Attrs . VI.fromList (0,4)
    a .= b = (a, b)
