{-# LANGUAGE DeriveFunctor #-}

module Graph
    ( Graph(..)
    , nullGraph
    , nodeSet
    , getNeighbors
    ) where

import Data.Monoid
import Data.Maybe
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

-- | For each node, its outgoing neighbors.
data Graph n e = Graph { getGraph :: HM.HashMap n [(n,e)] }
               deriving (Functor, Show)

nullGraph :: Graph n e -> Bool
nullGraph = HM.null . getGraph

nodeSet :: (Hashable a, Eq a) => Graph a e -> HS.HashSet a
nodeSet (Graph g) = HS.fromList (HM.keys g) <> foldMap (HS.fromList . map fst) g

getNeighbors :: (Eq n, Hashable n)
             => Graph n e -> n -> [(n,e)]
getNeighbors (Graph ns) n = fromMaybe [] $ HM.lookup n ns

