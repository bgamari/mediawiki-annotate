{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module NodeMapping
    ( -- * Dense Node indexes
      NodeId(..)
      -- * Mapping to index space
    , NodeMapping
    , nodeRange
    , nodes
    , toNode
    , fromNode
    , mkNodeMapping
    ) where

import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Ix
import Data.Monoid
import Data.Maybe

import Dijkstra (Graph(..))

-- | A mapping of a 'Hashable' node type to a dense index space ('NodeId').
data NodeMapping n = NodeMapping { nodeRange :: (NodeId, NodeId)
                                 , toNodeArr :: A.Array NodeId n
                                 , fromNodeMap :: HM.HashMap n NodeId
                                 }

nodes :: NodeMapping n -> [(NodeId, n)]
nodes = A.assocs . toNodeArr

-- | A dense node index.
newtype NodeId = NodeId Int
               deriving (Eq, Ord, Show, Enum, Ix)

nodeSet :: (Hashable n, Eq n) => Graph n a -> HS.HashSet n
nodeSet (Graph g) = HS.fromList (HM.keys g) <> foldMap (HS.fromList . map fst) g

toNode :: NodeMapping n -> NodeId -> n
toNode m = (toNodeArr m A.!)

fromNode :: (Eq n, Hashable n, Show n)
         => NodeMapping n -> n -> NodeId
fromNode m n =
    fromMaybe (error $ "PageRank.adjacency.fromNode: "++show n)
    $ HM.lookup n (fromNodeMap m)

mkNodeMapping
    :: forall n a. (Eq n, Hashable n)
    => Graph n a
    -> NodeMapping n
mkNodeMapping g =
    NodeMapping nodeRange toNodeArr fromNodeMap
  where
    allNodes = nodeSet g
    minNodeId = NodeId 0
    maxNodeId = NodeId $ HS.size allNodes - 1
    nodeRange = (minNodeId, maxNodeId)

    toNodeArr :: A.Array NodeId n
    toNodeArr = A.listArray nodeRange (HS.toList allNodes)
    fromNodeMap :: HM.HashMap n NodeId
    fromNodeMap = HM.fromList $ zip (HS.toList allNodes) [NodeId 0..]
