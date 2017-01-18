{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}

module Dijkstra
   ( Graph(..)
   , Distance(..)
   , dijkstraST
   , dijkstra
   , shortestPaths
   , test
   ) where

import Control.Monad.ST
import Control.Monad.Trans.Class
import Data.Ix
import Data.Monoid
import Data.Maybe
import Data.Hashable
import qualified Data.Array as A
import qualified Data.Array.MArray as A
import qualified Data.Array.IArray as A
import qualified Data.Array.ST as A
import qualified Data.HashPSQ as PSQ
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict

data Graph n e = Graph (n, n) (HM.HashMap n [(n,e)])

getNeighbors :: (Eq n, Hashable n)
             => Graph n e -> n -> [(n,e)]
getNeighbors (Graph _ ns) n = fromMaybe [] $ HM.lookup n ns

data Distance e = Finite !e | Infinite
                deriving (Show, Eq, Ord)

instance Monoid e => Monoid (Distance e) where
    mempty = Finite mempty
    Finite a `mappend` Finite b = Finite (a <> b)
    _ `mappend` _               = Infinite

dijkstraST :: forall n e.
              ( Hashable n, Ix n, Monoid e, Ord e )
           => Graph n e -> n -> (A.Array n (Distance e), A.Array n n)
dijkstraST graph@(Graph nodes _) src = runST dijkstraM
  where
    dijkstraM :: forall s. ST s (A.Array n (Distance e), A.Array n n)
    dijkstraM = do
        dists <- A.newArray nodes Infinite :: ST s (A.STArray s n (Distance e))
        prevs <- A.newArray_ nodes :: ST s (A.STArray s n n)
        A.writeArray dists src mempty
        let vs0 :: PSQ.HashPSQ n (Distance e) ()
            vs0 = PSQ.singleton src (Finite mempty) ()

            go :: StateT (PSQ.HashPSQ n (Distance e) ()) (ST s) ()
            go = do
                mNext <- popPSQ
                case mNext of
                  Nothing -> return ()
                  Just (u, distU, _) ->
                      forM_ (getNeighbors graph u) $ \(v, len) -> do
                          let alt = distU <> Finite len
                          distV <- lift $ A.readArray dists v
                          when (alt < distV) $ do
                              lift $ A.writeArray dists v alt
                              lift $ A.writeArray prevs v u
                              modify $ PSQ.insert v alt ()
        runStateT go vs0
        (,) <$> A.freeze dists <*> A.freeze prevs

popPSQ :: (Monad m, Ord k, Hashable k, Ord p)
       => StateT (PSQ.HashPSQ k p v) m (Maybe (k, p, v))
popPSQ = do
    psq <- get
    case PSQ.minView psq of
      Just (k,p,v,rest) -> do put rest
                              return $ Just (k,p,v)
      Nothing           -> return Nothing


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
            ( Hashable n, Ix n, Monoid e, Ord e )
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


test :: Graph Char (Sum Int)
test = Graph (a, f) $ HM.fromList
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