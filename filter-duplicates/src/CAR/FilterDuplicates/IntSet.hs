{-# LANGUAGE BangPatterns #-}

module CAR.FilterDuplicates.IntSet where

import qualified Data.Vector.Unboxed as VU
import Control.DeepSeq

newtype IntSet = IntSet (VU.Vector Int)

instance NFData IntSet where
    rnf x = x `seq` ()

fromAscList :: [Int] -> IntSet
fromAscList = IntSet . VU.fromList

toAscList :: IntSet -> [Int]
toAscList (IntSet xs) = VU.toList xs

size :: IntSet -> Int
size (IntSet xs) = VU.length xs

unionIntersectSize :: IntSet -> IntSet -> (Int, Int)
unionIntersectSize = \(IntSet a) (IntSet b) -> go 0 0 a b
  where
    go !union !intersect !a !b
      | VU.null a, VU.null b
      = (union, intersect)

      | VU.null a
      = (union + VU.length b, intersect)

      | VU.null b
      = (union + VU.length a, intersect)

    -- non-empty
    go union intersect a b
      | aa == bb
      = go (union+1) (intersect+1) (VU.tail a) (VU.tail b)

      | aa <  bb
      = go (union+1) intersect (VU.tail a) b

      | aa >  bb
      = go (union+1) intersect a (VU.tail b)

      | otherwise
      = error "impossible?"
      where
        aa = VU.head a
        bb = VU.head b
