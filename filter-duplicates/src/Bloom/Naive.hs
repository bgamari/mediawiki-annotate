{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bloom.Naive where

import Data.Hashable
import Data.Foldable
import Numeric
import Data.Bits

newtype Bloom = Bloom { getBloom :: Integer }
              deriving (Eq)

instance Show Bloom where
    showsPrec _ (Bloom b) = showHex b

zeroBloom :: Bloom
zeroBloom = Bloom 0
{-# INLINEABLE zeroBloom #-}

bloomJaccard :: Bloom -> Bloom -> Double
bloomJaccard (Bloom a) (Bloom b) =
    realToFrac (popCount (a .&. b)) / realToFrac (popCount (a .|. b))
{-# INLINE bloomJaccard #-}

unionBlooms :: [Bloom] -> Bloom
unionBlooms = Bloom . foldl' (.|.) 0 . map getBloom
{-# INLINE unionBlooms #-}

toBloom :: Hashable a => [a] -> Bloom
toBloom = Bloom . foldl' (.|.) 0 . map toBit
  where toBit x = bit $ hash x .&. 1023
{-# INLINE toBloom #-}

boundedJaccard :: Bloom -> Bloom -> Double
boundedJaccard (Bloom a) (Bloom b) =
    realToFrac (popCount (b .&. a)) / realToFrac (popCount a)
{-# INLINE boundedJaccard #-}
