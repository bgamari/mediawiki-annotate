{-# LANGUAGE CPP #-}

#if __GLASGOW_HASKELL__ > 802
#define USE_COMPACT
#endif

module CAR.Utils.Compact
    ( inCompactM
    , inCompact
    ) where

import Control.DeepSeq
#ifdef USE_COMPACT
import System.IO.Unsafe
import qualified GHC.Compact as Compact
#endif

inCompactM :: NFData a => IO a -> IO a
inCompact :: NFData a => a -> a
#ifdef USE_COMPACT
inCompactM action = action >>= fmap Compact.getCompact . Compact.compact
inCompact         = unsafePerformIO . fmap Compact.getCompact . Compact.compact
#else
inCompactM = id
inCompact  = id
#endif
