{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}

module Dijkstra
   ( Graph(..)
   , Distance(..)
   , dijkstra
   , lazyDijkstra
   , shortestPaths
   , test
   ) where

import Data.Foldable
import Data.Monoid
import Data.Maybe
import Data.Hashable
import qualified Data.HashPSQ as PSQ
import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.State.Strict

data Graph n e = Graph { getGraph :: HM.HashMap n [(n,e)] }
               deriving (Functor, Show)

getNeighbors :: (Eq n, Hashable n)
             => Graph n e -> n -> [(n,e)]
getNeighbors (Graph ns) n = fromMaybe [] $ HM.lookup n ns

data Distance e = Finite !e | Infinite
                deriving (Show, Eq, Ord)

instance Monoid e => Monoid (Distance e) where
    mempty = Finite mempty
    Finite a `mappend` Finite b = Finite (a <> b)
    _ `mappend` _               = Infinite

--------------------------------------------------
-- Dijkstra

data S n e = S { sAccum :: !(HM.HashMap n (Distance e, [n]))
               , sPSQ   :: !(PSQ.HashPSQ n (Distance e) ())
               }
           deriving (Show)

lookupDistance :: (Eq n, Hashable n) => n -> State (S n e) (Distance e)
lookupDistance n = maybe Infinite fst . HM.lookup n . sAccum <$> get

popS :: (Ord n, Hashable n, Ord e)
     => State (S n e) (Maybe (n, Distance e))
popS = do
    s <- get
    case PSQ.minView $ sPSQ s of
     Just (k,p,_,rest) -> do put $! s { sPSQ = rest }
                             return $ Just (k,p)
     Nothing           -> return Nothing

-- | Compute the shortest path lengths and predecessor nodes from the given source node.
-- By convention the source node has itself as a predecessor.
dijkstra :: forall n e.
            ( Hashable n, Monoid e, Ord e, Ord n )
         => Graph n e -> n -> HM.HashMap n (Distance e, [n])
dijkstra graph =
    \src -> let s0 = S { sAccum = HM.singleton src (Finite mempty, [src])
                       , sPSQ   = PSQ.singleton src (Finite mempty) ()
                           --foldl (\acc n -> PSQ.insert n (if n == src then Finite mempty else Infinite) () acc) PSQ.empty (range nodes)
                       }
            in sAccum $ execState go s0
  where
    go :: State (S n e) ()
    go = do
        mNext <- popS
        case mNext of
          Nothing -> return ()
          Just (u, distU) -> do
              forM_ (getNeighbors graph u) $ \(v, len) -> do
                  distV <- lookupDistance v
                  let alt = distU <> Finite len
                  if | alt < distV -> do
                         modify $ \s -> s { sAccum = HM.insert v (alt, [u]) (sAccum s)
                                          , sPSQ   = PSQ.insert v alt () (sPSQ s)
                                          }
                     | alt == distV ->
                         modify $ \s -> s { sAccum = HM.update (\(_, ns) -> Just (alt, u:ns)) v (sAccum s)
                                          }
                     | otherwise -> return ()

              go

----------------------------------------------------------
-- Lazy Dijkstra

data LS n e = LS { -- | Nodes which we already have a distance-from-source for
                   lsAccum :: !(HM.HashMap n (Distance e, [n]))
                   -- | Nodes which we will soon be able to emit distances for
                 , lsEmit  :: !(PSQ.HashPSQ n (Distance e) [n])
                   -- | Nodes which we have yet to look at
                 , lsTodo  :: !(PSQ.HashPSQ n (Distance e) ())
                 }
            deriving (Show)

-- | Compute the shortest path lengths and predecessor nodes from the given source node.
-- By convention the source node has itself as a predecessor.
--
-- The resulting paths are lazily constructed: the first elements of the result
-- will be direct neighbors of the source node, followed by their neighbors,
-- etc. This allows the consumer to choose how much of the graph they would
-- like the result to cover.
lazyDijkstra
    :: forall n e.
       ( Hashable n, Monoid e, Ord e, Ord n )
    => Graph n e -> n -> [(n, Distance e, [n])]
lazyDijkstra graph =
    \src -> let s0 = LS { lsAccum = HM.singleton src (Finite mempty, [src])
                        , lsEmit  = PSQ.empty
                        , lsTodo  = PSQ.singleton src (Finite mempty) ()
                        }
            in go s0
  where
    go :: LS n e -> [(n, Distance e, [n])]
    go s
      | Just (u, distU, _predsU, todo') <- PSQ.minView $ lsTodo s =
          let (emitted, lsEmit') = splitLessThan distU (lsEmit s)
              s' = s { lsTodo = todo'
                     , lsEmit = lsEmit'
                     }
              s'' = foldl' (tryNeighbor (u, distU)) s' (getNeighbors graph u)
          in emitted ++ go s''
      | otherwise = PSQ.toList (lsEmit s)
      where
        lookupDist :: n -> Distance e
        lookupDist n = maybe Infinite fst $ HM.lookup n (lsAccum s)

        -- | Probe a neighbor @v@ of @u@
        tryNeighbor :: (n, Distance e) -> LS n e -> (n, e) -> LS n e
        tryNeighbor (u, distU) s (v, len)
          | alt < distV
          = s { lsAccum = HM.insert v (alt, [u]) (lsAccum s)
              , lsEmit  = PSQ.insert v alt [u] (lsEmit s)
              , lsTodo  = PSQ.insert v alt () (lsTodo s)
              }
          | alt == distV
          = let f (Just (_, ns)) = Just (alt, u:ns)
                f Nothing        = error "lazyDijkstra: impossible"
            in s { lsAccum = HM.alter f v (lsAccum s)
                 , lsEmit  = snd $ PSQ.alter (((),) . f) v (lsEmit s)
                 }
          | otherwise
          = s
          where
            distV = lookupDist v
            alt = distU <> Finite len

splitLessThan :: forall n p v. (Ord p, Ord n, Hashable n)
              => p -> PSQ.HashPSQ n p v -> ([(n,p,v)], PSQ.HashPSQ n p v)
splitLessThan p0 = go []
  where
    go accum psq
      | Just (k, p, v, psq') <- PSQ.minView psq
      , p < p0
      = go ((k,p,v) : accum) psq'
    go accum psq = (accum, psq)

-------------------------------------------
-- Path-finding

-- | A path through a graph
type Path n = [n]

-- | Compute the shortest paths to @n@
shortestPaths :: forall e n. (Eq n, Hashable n)
              => HM.HashMap n (Distance e, [n]) -> n -> [Path n]
shortestPaths paths = go
  where
    go :: n -> [Path n]
    go n = [ n : rest
           | Just (_, preds) <- pure $ HM.lookup n paths -- find the predecessors of the current node
           , m <- preds                                  -- for each predecessor...
           , rest <- if n == m then pure [] else go m    -- check for arrival at source, if not draw remaining path
           ]

-------------------------------------------
-- Testing

test :: Graph Char (Sum Int)
test = Graph $ HM.fromList
    [ a .= [ b .= 1, c .= 2, d .= 10, e .= 1, f .= 1 ]
    , b .= [ a .= 1, c .= 1 ]
    , c .= [ a .= 2, b .= 1, d .= 1, e .= 1 ]
    , d .= [ c .= 1, a .= 10 ]
    , e .= [ a .= 1, c .= 1 ]
    , f .= [ a .= 1 ]
    ]
  where
    [a,b,c,d,e,f] = ['a'..'f']

    a .= b = (a, b)
