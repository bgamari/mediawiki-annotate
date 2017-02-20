{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ZScore
   ( Attributes(..)
   , zScoreStandardize
   ) where

import qualified Data.Array.Unboxed as A
import Data.Foldable as F
import Data.Ix

newtype Attributes a = Attrs { unAttrs :: A.UArray a Double }
                     deriving (Show)

zScoreStandardize :: forall a f. (Ix a, Functor f, Foldable f)
                  => f (Attributes a) -> f (Attributes a)
zScoreStandardize attrs
  | n == 0 = attrs
  | otherwise =
      let attrRng = A.bounds $ unAttrs $ head $ toList attrs

          vsum :: A.UArray a Double
          vsum = A.accumArray (+) 0 attrRng
                 [ (i, v)
                 | Attrs x <- toList attrs
                 , (i, v) <- A.assocs x
                 ]
          mean = A.amap (/ realToFrac n) vsum

          vsumSqr :: A.UArray a Double
          vsumSqr = A.accumArray (+) 0 attrRng
                    [ (i, (v - mu)^(2::Int))
                    | Attrs x <- toList attrs
                    , (i, v) <- A.assocs x
                    , let mu = mean A.! i
                    ]
          stdDev = A.amap (\v -> sqrt (v / realToFrac n)) vsumSqr

          standardize (Attrs xs) =
              Attrs $ iamap (\i v -> let mu = mean A.! i
                                         sig = stdDev A.! i
                                     in (v - mu) / sig
                            ) xs
      in fmap standardize attrs
  where n = F.length attrs

iamap :: (A.IArray a e', A.IArray a e, Ix i)
      => (i -> e' -> e) -> a i e' -> a i e
iamap f arr =
    A.listArray bounds $ zipWith f (range bounds) (A.elems arr)
  where
    bounds = A.bounds arr
