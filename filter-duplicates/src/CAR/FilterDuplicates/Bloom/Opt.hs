{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module CAR.FilterDuplicates.Bloom.Opt where

import Numeric
import Data.Bits
import Foreign.C.Types
import GHC.Exts
import Data.Hashable
import Data.Primitive

import CAR.FilterDuplicates.Word1024 as Word1024

data Bloom = Bloom { getBloom :: !Word1024 }
           deriving (Eq)

instance Show Bloom where
    showsPrec _ (Bloom b) = showHex $ word1024ToInteger b

zeroBloom :: Bloom
zeroBloom = Bloom $ Word1024.fromBits []
{-# INLINEABLE zeroBloom #-}

bloomJaccard :: Bloom -> Bloom -> Double
bloomJaccard a b =
    realToFrac $ c_jaccard (getArray a) (getArray b)
{-# INLINE bloomJaccard #-}

unionBlooms :: [Bloom] -> Bloom
unionBlooms = Bloom . Word1024.orWord1024s . map getBloom
{-# INLINE unionBlooms #-}

toBloom :: Hashable a => [a] -> Bloom
toBloom = Bloom . Word1024.fromBits . map toBit
  where toBit x = hash x .&. 1023
{-# INLINE toBloom #-}

boundedJaccard :: Bloom -> Bloom -> Double
boundedJaccard a b =
    realToFrac $ c_boundedJaccard (getArray a) (getArray b)
{-# INLINE boundedJaccard #-}

getArray :: Bloom -> ByteArray#
getArray (Bloom (Word1024 (ByteArray ba))) = ba

foreign import ccall unsafe "bounded_jaccard"
    c_boundedJaccard :: ByteArray# -> ByteArray# -> CDouble

foreign import ccall unsafe "jaccard"
    c_jaccard :: ByteArray# -> ByteArray# -> CDouble
