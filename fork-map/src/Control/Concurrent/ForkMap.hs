{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Concurrently map over a structure
module Control.Concurrent.ForkMap
    ( map
    , mapIO
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
import Prelude hiding (map)

map :: forall a b. (Binary a, Binary b)
    => Int -> Int
    -> (a -> b) -> Producer a IO () -> Producer b IO ()
map queueDepth nMappers f = mapIO queueDepth nMappers (pure . f)

mapIO :: forall a b. (Binary a, Binary b)
      => Int -> Int
      -> (a -> IO b) -> Producer a IO () -> Producer b IO ()
mapIO queueDepth nMappers f xs =
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
            pipe <- forkPipe id $ PP.mapM f
            runEffect $ PC.fromInput workIn >-> pipe >-> PC.toOutput resultOut

        -- Seals result queue
        watcher <- async $ do
            mapM_ wait mappers
            atomically resultSeal

        link feeder
        link watcher
        return resultIn
