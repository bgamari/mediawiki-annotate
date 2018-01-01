{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CAR.FilterDuplicates.ConnectedComponent (connectedComponents, Edge(..)) where

import Data.Monoid
import Data.Foldable
import Data.Hashable
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM

import CAR.Types

data Edge = Edge ParagraphId ParagraphId

newtype ClusterId = ClusterId Int
                  deriving (Eq, Enum, Hashable)

data Accum = Accum { nextClusterId  :: !ClusterId
                   , paraToCluster  :: HM.HashMap ParagraphId ClusterId
                   , clusterToParas :: HM.HashMap ClusterId (HS.HashSet ParagraphId)
                   }

emptyAccum :: Accum
emptyAccum = Accum (ClusterId 0) mempty mempty

connectedComponents :: [Edge] -> [HS.HashSet ParagraphId]
connectedComponents edges = HM.elems $ clusterToParas $ foldl' f emptyAccum edges
  where
    setCluster :: ParagraphId -> ClusterId -> Accum -> Accum
    setCluster pid cid (Accum{..}) =
        Accum { nextClusterId = nextClusterId
              , paraToCluster = HM.insert pid cid paraToCluster
              , clusterToParas = HM.insertWith (<>) cid (HS.singleton pid) clusterToParas
              }

    f accum0@(Accum{..}) (Edge a b)
      -- A and B are already in the same cluster
      | Just ca <- mca
      , Just cb <- mcb
      , ca == cb
      = accum0

      -- Move elements of cluster A into cluster B
      | Just ca <- mca
      , Just cb <- mcb
      = let clusterAElems = clusterToParas HM.! ca
        in accum0 { paraToCluster  = foldl' (\m n -> HM.insert n cb m) paraToCluster (HS.toList clusterAElems)
                  , clusterToParas = HM.delete ca
                                   $ HM.insertWith (<>) cb clusterAElems
                                   $ clusterToParas
                  }

      | Just ca <- mca
      = setCluster b ca accum0

      | Just cb <- mcb
      = setCluster a cb accum0

      | otherwise
      = setCluster b nextClusterId
      $ setCluster a nextClusterId
      $ accum0 { nextClusterId = succ nextClusterId }
      where
        mca, mcb :: Maybe ClusterId
        mca = HM.lookup a paraToCluster
        mcb = HM.lookup b paraToCluster

