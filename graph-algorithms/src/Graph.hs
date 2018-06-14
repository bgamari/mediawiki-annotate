{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph
    ( Graph(..)
    , nullGraph
    , nodeSet
    , getNeighbors
    , filterEdges
    , dropDisconnected
    ) where

import Control.DeepSeq
import Data.Semigroup
import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- | For each node, its outgoing neighbors.
newtype Graph n e = Graph { getGraph :: HM.HashMap n (HM.HashMap n e) }
                  deriving (Functor, Show, NFData, Foldable)

instance (Hashable n, Eq n, Semigroup e) => Semigroup (Graph n e) where
    Graph a <> Graph b = Graph $ HM.unionWith (HM.unionWith (<>)) a b

instance (Hashable n, Eq n, Semigroup e) => Monoid (Graph n e) where
    mempty = Graph mempty
    mappend = (<>)

nullGraph :: Graph n e -> Bool
nullGraph = HM.null . getGraph

nodeSet :: (Hashable n, Eq n) => Graph n e -> HS.HashSet n
nodeSet (Graph g) = HS.fromMap (() <$ g) <> foldMap (HS.fromMap . (() <$)) g

getNeighbors :: (Eq n, Hashable n)
             => Graph n e -> n -> HM.HashMap n e
getNeighbors (Graph ns) n = fromMaybe mempty $ HM.lookup n ns

-- TODO: This also drops nodes!
filterEdges :: (n -> n -> e -> Bool)
            -> Graph.Graph n e -> Graph.Graph n e
filterEdges pred (Graph.Graph graph) =
    Graph.Graph $ HM.mapWithKey f $ graph
  where f source = HM.filterWithKey (\target x -> pred source target x)

-- | Filter nodes and their associated edges.
filterNodes :: (n -> Bool) -> Graph.Graph n e -> Graph.Graph n e
filterNodes pred (Graph.Graph g) =
    Graph.Graph $ fmap (HM.filterWithKey f) $ HM.filterWithKey f g
  where
    f src _ = pred src

nodeDegree :: (Eq n, Hashable n, Semigroup e) => Graph.Graph n e -> HM.HashMap n e
nodeDegree (Graph.Graph g) = HM.fromListWith (<>)
    [ (n, w)
    | (n, ms) <- HM.toList g
    , (m, w) <- HM.toList ms
    ]

dropDisconnected :: (Eq n, Hashable n) => Graph.Graph n e -> Graph.Graph n e
dropDisconnected g =
    filterNodes (not . isDisconnected) g
  where
    isDisconnected n
      | Just deg <- HM.lookup n symNodeDegree  = deg == 0
      | otherwise                              = True

    symNodeDegree = HM.fromListWith (+) $ concat
        [ [(n, 1), (m, 1)]
        | (n, ms) <- HM.toList $ getGraph g
        , (m, _) <- HM.toList ms
        ]
