{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module DenseMapping
    ( -- * Dense Node indexes
      DenseId(..)
      -- * Mapping to index space
    , DenseMapping
    , denseRange
    , assocs
    , elems
    , toDense
    , fromDense
    , mkDenseMapping
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Indexed as VI
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Ix
import Data.Maybe
import GHC.Stack

-- | A mapping of a 'Hashable' type to a dense index space ('DenseId').
data DenseMapping a = DenseMapping { denseRange :: (DenseId a, DenseId a)
                                   , fromDenseArr :: VI.Vector V.Vector (DenseId a) a
                                   , toDenseMap :: HM.HashMap a (DenseId a)
                                   }

assocs :: DenseMapping a -> [(DenseId a, a)]
assocs = VI.assocs . fromDenseArr

elems :: DenseMapping a -> [a]
elems = map snd . assocs

-- | A dense node index.
newtype DenseId a = DenseId Int
               deriving (Eq, Ord, Show, Enum, Ix, Hashable)

fromDense :: DenseMapping a -> DenseId a -> a
fromDense m = (fromDenseArr m VI.!)

toDense :: (HasCallStack, Eq a, Hashable a, Show a)
        => DenseMapping a -> a -> DenseId a
toDense m x =
    fromMaybe (error $ "DenseMapping.toDense: "++show x)
    $ HM.lookup x (toDenseMap m)
{-# INLINE toDense #-}

mkDenseMapping
    :: forall a. (Eq a, Hashable a)
    => HS.HashSet a
    -> DenseMapping a
mkDenseMapping things =
    DenseMapping nodeRange toNodeArr fromNodeMap
  where
    minNodeId = DenseId 0
    maxNodeId = DenseId $ HS.size things - 1
    nodeRange = (minNodeId, maxNodeId)

    toNodeArr :: VI.Vector V.Vector (DenseId a) a
    toNodeArr = VI.fromList nodeRange (HS.toList things)

    fromNodeMap :: HM.HashMap a (DenseId a)
    fromNodeMap = HM.fromList $ zip (HS.toList things) [DenseId 0..]
