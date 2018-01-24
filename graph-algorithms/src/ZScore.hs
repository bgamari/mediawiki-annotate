{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module ZScore
   ( Attributes(..)
   , zScoreStandardize
   ) where

import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Unboxed as VU
import Data.Foldable as F
import Data.Ix

newtype Attributes a = Attrs { unAttrs :: VI.Vector VU.Vector a Double }
                     deriving (Show)

zScoreStandardize :: forall a f. (Ix a, Functor f, Foldable f)
                  => f (Attributes a) -> f (Attributes a)
zScoreStandardize attrs
  | n == 0 = attrs
  | otherwise =
      let attrRng = VI.bounds $ unAttrs $ head $ toList attrs

          vsum :: VI.Vector VU.Vector a Double
          vsum = VI.accum' attrRng (+) 0
                 [ (i, v)
                 | Attrs x <- toList attrs
                 , (i, v) <- VI.assocs x
                 ]
          mean = VI.map (/ realToFrac n) vsum

          vsumSqr :: VI.Vector VU.Vector a Double
          vsumSqr = VI.accum' attrRng (+) 0
                    [ (i, (v - mu)^(2::Int))
                    | Attrs x <- toList attrs
                    , (i, v) <- VI.assocs x
                    , let mu = mean VI.! i
                    ]
          stdDev = VI.map (\v -> sqrt (v / realToFrac n)) vsumSqr

          standardize (Attrs xs) =
              Attrs $ VI.imap (\i v -> let mu = mean VI.! i
                                           sig = stdDev VI.! i
                                       in (v - mu) / sig
                            ) xs
      in fmap standardize attrs
  where n = F.length attrs
