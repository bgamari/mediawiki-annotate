{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Concurrently map over a structure
--
-- You will need @-XStaticPointers@.
module Control.Concurrent.ForkMap
    ( map
    , mapIO
      -- * Convenient reexports
    , Serializable
    , Dict(..)
    ) where

import Control.Monad
import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Binary
import Pipes.Safe (runSafeT)
import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Concurrent as PC
import Control.Concurrent.ForkMap.ForkPipe
import Control.Distributed.Closure
import Data.Typeable (Typeable)
import Prelude hiding (map)

instance Static (MonadIO IO) where closureDict = static Dict

-- | Map over a producer of elements with a pure function (not order-preserving).
map :: forall a b. (Serializable a, Serializable b)
    => Int -> Int
    -> Closure (Dicts () a () b IO ()) -- ^ just pass @static 'Dict'@
    -> Closure (a -> b) -> Producer a IO () -> Producer b IO ()
map queueDepth nMappers dicts f = mapIO queueDepth nMappers dicts (static (pure .) `cap` f)

-- | Map over a producer of elements with an effectful function (not order-preserving).
mapIO :: forall a b. (Serializable a, Serializable b)
      => Int -> Int
      -> Closure (Dicts () a () b IO ()) -- ^ just pass @static 'Dict'@
      -> Closure (a -> IO b) -> Producer a IO () -> Producer b IO ()
mapIO queueDepth nMappers dicts f xs =
    liftIO run >>= PC.fromInput
  where
    run :: IO (PC.Input b)
    run = do
        (workOut, workIn, workSeal) <- PC.spawn' $ PC.bounded queueDepth
        (resultOut, resultIn, resultSeal) <- PC.spawn' $ PC.bounded queueDepth

        -- Feeds work queue
        feeder <- async $ do
            runEffect $ xs >-> toOutput workOut
            atomically workSeal

        -- Feeds mappers
        mappers <- replicateM nMappers $ async $ runSafeT $ do
            pipe <- forkPipe dicts (static (RunAction id)) (static PP.mapM `cap` f)
            runEffect $ PC.fromInput workIn >-> pipe >-> PC.toOutput resultOut

        -- Seals result queue
        watcher <- async $ do
            mapM_ wait mappers
            atomically resultSeal

        link feeder
        link watcher
        return resultIn
