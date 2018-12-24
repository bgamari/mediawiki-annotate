{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module DenseMapping
    ( -- * Dense Node indexes
      DenseId(..)
      -- * Mapping to index space
    , DenseMapping
    , denseRange
    , toDenseMap
    , assocs
    , elems
    , size
    , toDense
    , toDenseMaybe
    , fromDense
    , mkDenseMapping
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Indexed as VI
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Vector.Unboxed.Deriving
import Data.Hashable
import Data.Ix
import Data.Maybe
import GHC.Stack

-- | A mapping of a 'Hashable' type to a dense index space ('DenseId').
data DenseMapping a = DenseMapping { denseMin :: !(DenseId a)
                                   , denseMax :: !(DenseId a)
                                   , fromDenseArr :: !(VI.Vector V.Vector (DenseId a) a)
                                   , toDenseMap :: !(HM.HashMap a (DenseId a))
                                   }

denseRange :: DenseMapping a -> (DenseId a, DenseId a)
denseRange (DenseMapping a b _ _) = (a, b)

assocs :: DenseMapping a -> [(DenseId a, a)]
assocs = VI.assocs . fromDenseArr

elems :: DenseMapping a -> [a]
elems = map snd . assocs

-- | How many elements are in the domain of the mapping?
size :: DenseMapping a -> Int
size = rangeSize . denseRange

-- | A dense node index.
newtype DenseId a = DenseId Int
               deriving (Eq, Ord, Show, Enum, Ix, Hashable)

$(derivingUnbox "DenseId"
     [t| forall a. DenseId a -> Int |]
     [| \(DenseId n) -> n |]
     [| DenseId |]
 )

fromDense :: DenseMapping a -> DenseId a -> a
fromDense m = (fromDenseArr m VI.!)

toDenseMaybe :: (HasCallStack, Eq a, Hashable a, Show a)
             => DenseMapping a -> a -> Maybe (DenseId a)
toDenseMaybe m x =
    HM.lookup x (toDenseMap m)
{-# INLINE toDenseMaybe #-}

toDense :: (HasCallStack, Eq a, Hashable a, Show a)
        => DenseMapping a -> a -> DenseId a
toDense m x =
    fromMaybe (error $ "DenseMapping.toDense: "++show x)
    $ toDenseMaybe m x
{-# INLINE toDense #-}

mkDenseMapping
    :: forall a. (Eq a, Hashable a)
    => HS.HashSet a
    -> DenseMapping a
mkDenseMapping things =
    DenseMapping minNodeId maxNodeId toNodeArr fromNodeMap
  where
    minNodeId = DenseId 0
    maxNodeId = DenseId $ HS.size things - 1
    nodeRange = (minNodeId, maxNodeId)

    toNodeArr :: VI.Vector V.Vector (DenseId a) a
    toNodeArr = VI.fromList nodeRange (HS.toList things)

    fromNodeMap :: HM.HashMap a (DenseId a)
    fromNodeMap = HM.fromList $ zip (HS.toList things) [DenseId 0..]
