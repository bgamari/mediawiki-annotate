{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Word1024 where

import Foreign.C.Types
import Data.Bits
import Data.Word
import GHC.Exts
import Data.Primitive
import Control.Monad.ST
import Control.Monad.ST.Unsafe

data Word1024 = Word1024 ByteArray

instance Show Word1024 where
    show = show . word1024ToInteger

word1024ToInteger :: Word1024 -> Integer
word1024ToInteger (Word1024 ba) = go 0 (word1024Bytes `div` 8)
  where
    go accum 0 = accum
    go accum b =
        let accum' = accum .|. fromIntegral (indexByteArray ba b :: Word64)
        in go accum' (b+1)

word1024Bytes = 1024 `div` 8

newWord1024Buf :: ST s (MutableByteArray s)
newWord1024Buf = newAlignedPinnedByteArray 64 word1024Bytes

fromBits :: [Int] -> Word1024
fromBits xs = runST $ do
    ba <- newWord1024Buf
    let f x rest = do
            let (byteN,bitN) = (x `mod` word1024Bytes) `divMod` 64
            n <- readByteArray ba byteN
            writeByteArray ba byteN (n .|. (bit bitN :: Word64))
            rest
    foldr f (Word1024 <$> unsafeFreezeByteArray ba) xs
{-# INLINEABLE fromBits #-}

orWord1024s :: [Word1024] -> Word1024
orWord1024s xs = runST $ do
    ba <- newWord1024Buf
    let f x rest = doUpdate c_orWord1024 ba x >> rest
    foldr f (Word1024 <$> unsafeFreezeByteArray ba) xs

andWord1024s :: [Word1024] -> Word1024
andWord1024s xs = runST $ do
    ba <- newWord1024Buf
    let f x rest = doUpdate c_orWord1024 ba x >> rest
    foldr f (Word1024 <$> unsafeFreezeByteArray ba) xs

instance Eq Word1024 where
    Word1024 (ByteArray x) == Word1024 (ByteArray y) = c_eqWord1024 x y == 1

doUpdate :: (MutableByteArray# s -> ByteArray# -> IO a)
         -> MutableByteArray s
         -> Word1024
         -> ST s a
doUpdate f (MutableByteArray mba) (Word1024 (ByteArray ba)) = unsafeIOToST $ f mba ba

foreign import ccall unsafe "eq_word1024" c_eqWord1024 :: ByteArray# -> ByteArray# -> CInt

foreign import ccall unsafe "or_word1024" c_orWord1024 :: MutableByteArray# s -> ByteArray# -> IO ()

foreign import ccall unsafe "and_word1024" c_andWord1024 :: MutableByteArray# s -> ByteArray# -> IO ()

