{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Graph
    ( Graph
    , getGraph

    , nullGraph
    , nodeSet
    , numNodes
    , getNeighbors
    , filterEdges
    , dropDisconnected

    -- * construct graphs
    , graphFromEdges, graphFromEdgesAndSingletons, graphFromNeighbors, graphUnions
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

graphFromEdges :: (Semigroup e, Eq n, Hashable n ) => [(n,n,e)] -> Graph n e
graphFromEdges edges = graphFromEdgesAndSingletons edges []

graphFromEdgesAndSingletons :: (Semigroup e, Eq n, Hashable n) =>  [(n,n,e)] -> [n]  -> Graph n e
graphFromEdgesAndSingletons edges singles =
    Graph
    $ HM.fromListWith (HM.unionWith (<>))
    $ concat
    [ [(n2, mempty), (n1, HM.singleton n2 e1)]
    | (n1,n2,e1) <- edges
    ]
    ++ [ (n, mempty) | n <- singles ]



graphFromNeighbors ::(Eq n, Hashable n) =>  [(n, [(n,e)])] -> Graph n e
graphFromNeighbors neighbors =
    Graph
    $ HM.fromListWith (<>)
    $ fmap (fmap HM.fromList) neighbors
   <> [ (n, mempty)                    -- include entries for "to"-nodes
      | (_, ns) <- neighbors
      , (n, _) <- ns
      ]

nullGraph :: Graph n e -> Bool
nullGraph = HM.null . getGraph


toEdgesAndSingletons :: Graph n e -> ( [(n,n,e)], [n] )
toEdgesAndSingletons graph =
    let edges = [ (n1, n2, e)
                | (n1, m) <- HM.toList $ getGraph graph
                , (n2, e) <- HM.toList m
                ]
        singles = [ n1
                  | (n1, m) <- HM.toList $ getGraph graph
                  , HM.null m
                  ]
    in (edges, singles)

toNeighbors :: Graph n e ->  [(n, [(n,e)])]
toNeighbors graph =
    [ (n1, neighs)
    | (n1, m) <- HM.toList $ getGraph graph
    , let neighs = HM.toList m
    ]

graphUnions :: (Eq n, Hashable n, Semigroup e) => [Graph n e] -> Graph n e
graphUnions graphs =
    Graph $ HM.fromListWith (HM.unionWith (<>)) $ foldMap (HM.toList . getGraph) graphs


nodeSet :: (Hashable n, Eq n) => Graph n e -> HS.HashSet n
nodeSet = HS.fromMap . fmap (const ()) . getGraph

numNodes :: Graph n e -> Int
numNodes = HM.size . getGraph

getNeighbors :: (Eq n, Hashable n)
             => Graph n e -> n -> HM.HashMap n e
getNeighbors (Graph ns) n = fromMaybe mempty $ HM.lookup n ns

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
