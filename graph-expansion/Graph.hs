{-# LANGUAGE DeriveFunctor #-}

module Graph
    ( Graph(..)
    , nullGraph
    , nodeSet
    , getNeighbors
    , filterEdges
    ) where

import Data.Semigroup
import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- | For each node, its outgoing neighbors.
data Graph n e = Graph { getGraph :: HM.HashMap n (HM.HashMap n e) }
               deriving (Functor, Show)

instance (Hashable n, Eq n, Semigroup e) => Semigroup (Graph n e) where
    Graph a <> Graph b = Graph $ HM.unionWith (HM.unionWith (<>)) a b

instance (Hashable n, Eq n, Semigroup e) => Monoid (Graph n e) where
    mempty = Graph mempty
    mappend = (<>)

nullGraph :: Graph n e -> Bool
nullGraph = HM.null . getGraph

nodeSet :: (Hashable n, Eq n) => Graph n e -> HS.HashSet n
nodeSet (Graph g) = mapToSet g <> foldMap mapToSet g
  where mapToSet = HS.fromMap . fmap (const ())

getNeighbors :: (Eq n, Hashable n)
             => Graph n e -> n -> HM.HashMap n e
getNeighbors (Graph ns) n = fromMaybe mempty $ HM.lookup n ns

filterEdges :: (n -> n -> Bool)
            -> Graph.Graph n e -> Graph.Graph n e
filterEdges pred (Graph.Graph graph) =
    Graph.Graph $ HM.mapWithKey f $ graph
  where f source = HM.filterWithKey (\target _ -> pred source target)