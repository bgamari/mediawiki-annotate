{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Dijkstra
   ( Distance(..)
   , Path
   , dijkstra
   , denseDijkstra
   , lazyDijkstra
   , lazyDijkstraK
   , shortestPaths
   , test
   ) where

import Control.DeepSeq
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Data.Hashable
import qualified Data.HashPSQ as PSQ
import qualified Data.HashMap.Strict as HM
import qualified Data.Sequence as Seq
import qualified Data.Heap as H
import qualified Data.Vector.Indexed as VI
import qualified Data.Vector.Indexed.Mutable as VIM
import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Deriving
import Data.Vector.Unboxed (Unbox)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.ST
import GHC.Stack

import DenseMapping
import Graph

data Distance e = Finite !e | Infinite
                deriving (Show, Eq, Ord, Functor)

instance NFData e => NFData (Distance e) where
    rnf (Finite x) = rnf x
    rnf Infinite   = ()

$(derivingUnbox "Distance"
     [t| forall a. (Num a, Unbox a) => Distance a -> (Bool, a) |]
     [| \dist -> case dist of Finite x -> (True, x)
                              Infinite -> (False, 0) |]
     [| \(finite,x) -> if finite then Finite x else Infinite |]
 )

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
            ( HasCallStack, Hashable n, Monoid e, Ord e, Ord n, Show e, Show n )
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
              forM_ (HM.toList $ getNeighbors graph u) $ \(v, len) -> do
                  distV <- lookupDistance v
                  let alt = distU <> Finite len
                  if -- sanity check
                     | alt <= distU ->
                         error $ "dijkstra: Non-increasing distance "++show (alt, distU, distV)++")"
                     | alt < distV ->
                         modify $ \s -> s { sAccum = HM.insert v (alt, [u]) (sAccum s)
                                          , sPSQ   = PSQ.insert v alt () (sPSQ s)
                                          }
                     | alt == distV ->
                         modify $ \s -> s { sAccum = HM.update (\(_, ns) -> Just (alt, u:ns)) v (sAccum s)
                                          }
                     | otherwise -> return ()

              go

--------------------------------------------------
-- Dense Dijkstra

-- | The monad 'denseDijkstra' operates within.
type DenseM s n e = StateT (PSQ.HashPSQ n (Distance e) ()) (ST s)

densePopS :: (Ord n, Hashable n, Ord e)
          => DenseM s n e (Maybe (n, Distance e))
densePopS = do
    s <- get
    case PSQ.minView s of
     Just (k,p,_,rest) -> do put $! rest
                             return $ Just (k,p)
     Nothing           -> return Nothing

denseDijkstra :: forall n e. (Hashable n, Eq n, Ord n, Show n, Ord e, Monoid e, Show e, Num e, Unbox e)
              => DenseMapping n
              -> Graph n e
              -> n
              -> VI.Vector VU.Vector (DenseId n) (Distance e)
denseDijkstra mapping graph =
    \src -> VI.create $ do
        accum <- VIM.replicate (denseRange mapping) Infinite
        let q0 = PSQ.singleton src (Finite mempty) ()
        evalStateT (go accum) q0
        return accum
  where
    go :: VIM.MVector VU.MVector s (DenseId n) (Distance e)
       -> DenseM s n e ()
    go !accum = do
        mNext <- densePopS
        case mNext of
          Nothing -> return ()
          Just (u, distU) -> do
              forM_ (HM.toList $ getNeighbors graph u) $ \(v, len) -> do
                  let !vi = toDense mapping v
                  distV <- lift $ VIM.read accum vi
                  let !alt = distU <> Finite len
                  if -- sanity check
                     | alt <= distU ->
                         error $ "denseDijkstra: Non-increasing distance "++show (alt, distU, distV)++")"
                     | alt < distV -> do
                         modify $ PSQ.insert v alt ()
                         lift $ VIM.write accum vi alt
                     | otherwise -> return ()

              go accum

----------------------------------------------------------
-- Lazy Dijkstra

data LS n e acc = LS { -- | Nodes which we already have a distance-from-source for
                       lsAccum :: !(HM.HashMap n (Distance e))
                       -- | Nodes which we will soon be able to emit distances for
                     , lsEmit  :: !(PSQ.HashPSQ n (Distance e) acc)
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
lazyDijkstra = lazyDijkstra' (\_dist n _ -> [n])

-- | Compute the @k@ predecessors with shortest paths for each node.
lazyDijkstraK
    :: forall n e.
       ( Hashable n, Monoid e, Ord e, Ord n )
    => Int -> Graph n e -> n -> [(n, Distance e, Seq.Seq (Distance e, n))]
lazyDijkstraK k = lazyDijkstra' f
  where
    f dist predNode acc =
        Seq.take k $ (dist, predNode) Seq.<| fromMaybe mempty acc

lazyDijkstra'
    :: forall n e acc.
       ( Hashable n, Monoid e, Ord e, Ord n )
    => (Distance e -> n -> Maybe acc -> acc)
       -- ^ @collect dist pred@ collects a new shortest distance @dist@
       -- from the source node. This will be used to update a node's accumulator
       -- whenever a shorter path between the node and the source is identified.
    -> Graph n e -> n -> [(n, Distance e, acc)]
lazyDijkstra' collect graph =
    \src -> let s0 = LS { lsAccum = HM.singleton src (Finite mempty)
                        , lsEmit  = PSQ.empty
                        , lsTodo  = PSQ.singleton src (Finite mempty) ()
                        }
            in go s0
  where
    go :: LS n e acc -> [(n, Distance e, acc)]
    go s0
      | Just (u, distU, _predsU, todo') <- PSQ.minView $ lsTodo s0 =
          let (emitted, lsEmit') = splitLessThan distU (lsEmit s0)
              s' = s0 { lsTodo = todo'
                      , lsEmit = lsEmit'
                      }
              s'' = foldl' (tryNeighbor (u, distU)) s' (HM.toList $ getNeighbors graph u)
          in emitted ++ go s''
      | otherwise = PSQ.toList (lsEmit s0)
      where
        lookupDist :: n -> Distance e
        lookupDist n = fromMaybe Infinite $ HM.lookup n (lsAccum s0)

        -- | Probe a neighbor @v@ of @u@
        tryNeighbor :: (n, Distance e) -> LS n e acc -> (n, e) -> LS n e acc
        tryNeighbor (u, distU) s (v, len)
          | alt <= distU
          = error "lazyDijkstra: Non-increasing distance"

          | alt < distV
          = s { lsAccum = HM.insert v alt (lsAccum s)
              , lsEmit  = lsEmit'
              , lsTodo  = PSQ.insert v alt () (lsTodo s)
              }

          | alt == distV
          = s { lsEmit  = lsEmit'
              }
          | otherwise
          = s
          where
            distV = lookupDist v
            alt = distU <> Finite len
            lsEmit' = snd $ PSQ.alter addEmission v (lsEmit s)
              where
                addEmission Nothing         = ((), Just (alt, collect alt u Nothing))
                addEmission (Just (_, acc)) = ((), Just (alt, collect alt u (Just acc)))

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

-- | Compute the shortest paths to @n@ (e.g. all of the minimal length).
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

kShortestPaths :: forall e n. (Eq n, Hashable n)
               => HM.HashMap n (Distance e, Seq.Seq (Distance e, n)) -> n
               -> [Path n]
kShortestPaths paths = go
  where
    go :: n -> [Path n]
    go n = [
           ]

-- | Given a list of sorted sequences, produce the interleaved sorted sequence
interleaveSorted :: forall a. Ord a => [[a]] -> [a]
interleaveSorted = go . foldl' (flip addCandidate) mempty
  where
    go :: H.Heap (H.Entry a [a]) -> [a]
    go heap
      | Just (H.Entry x xs, heap') <- H.viewMin heap
      = x : go (addCandidate xs heap')
      | otherwise
      = []

    addCandidate :: [a] -> H.Heap (H.Entry a [a]) -> H.Heap (H.Entry a [a])
    addCandidate [] = id
    addCandidate (x:xs) = H.insert (H.Entry x xs)


-------------------------------------------
-- Testing

test :: Graph Char (Sum Int)
test = Graph $ fmap HM.fromList $ HM.fromList
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
