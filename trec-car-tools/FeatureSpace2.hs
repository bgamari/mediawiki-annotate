module FeatureSpace2
    ( FeatureVec
    , FeatureVec'
    , concat
    , index
    ) where

import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unbox as VU

type FeatureVec = FeatureVec' VU.Vector

data FeatureVec' v (fs :: [Type]) a = FeatureVec (v a)

concat :: (VG.Vector v a)
       => FeatureVec' v fs a
       -> FeatureVec' v gs a
       -> FeatureVec' v (fs ++ gs) a
concat (FeatureVec xs) (FeatureVec ys) =
    FeatureVec (xs `VU.concat` ys)

index :: (VG.Vector v a, f `Elem` fs)
      => FeatureVec' fs a -> f -> a
index = undefined

restrict :: (VG.Vector )
