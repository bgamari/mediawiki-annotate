module CAR.FilterDuplicates.Bloom.IntSet where

import Data.Hashable
import qualified Data.IntervalSet as IS

newtype Bloom = Bloom { getBloom :: IS.IntSet }
              deriving (Eq)

instance Show Bloom where
    showsPrec _ (Bloom b) = shows b

zeroBloom :: Bloom
zeroBloom = Bloom 0
{-# INLINEABLE zeroBloom #-}

bloomJaccard :: Bloom -> Bloom -> Double
bloomJaccard (Bloom a) (Bloom b) =
    let num = IS.size (a `IS.intersection` b)
        denom = IS.size (a `IS.union` b)
    in realToFrac num / realToFrac denom
{-# INLINE bloomJaccard #-}

unionBlooms :: [Bloom] -> Bloom
unionBlooms = Bloom . IS.unions . map getBloom
{-# INLINE unionBlooms #-}

toBloom :: Hashable a => [a] -> Bloom
toBloom = Bloom . IS.fromList . map hash
{-# INLINE toBloom #-}

boundedJaccard :: Bloom -> Bloom -> Double
boundedJaccard (Bloom a) (Bloom b) =
    let num = IS.size (b `IS.intersection` a)
        denom = IS.size a
    in --traceShow (num, denom) $
       realToFrac num / realToFrac denom
{-# INLINE boundedJaccard #-}
