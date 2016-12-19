{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}

module ForkPipe
    ( forkPipe
    , WorkerFailException(..)
    ) where

import Control.Exception
import Data.Kind (Constraint)
import GHC.Generics
import System.IO

import Pipes
import Pipes.Safe as Safe
import qualified Pipes.Internal as P
import Data.Binary
import System.Posix.Signals
import System.Posix.Process
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.ByteString.Char8 as BS

import FifoChannel

debug :: Bool
debug = False

-- | A request "inward" from the worker to the worker.
data InwardMessage a' a b' b r
    = MasterUpRespond a    -- | upstream node on master has yielded
    | MasterDownRequest b' -- | downstream node on master has awaited
    | MasterDone           -- | master has terminated
    deriving (Generic)
instance (All Binary a' a b' b r) => Binary (InwardMessage a' a b' b r)

showInward :: InwardMessage a' a b' b r -> String
showInward (MasterUpRespond _) = "MasterUpRespond{..}"
showInward (MasterDownRequest _) = "MasterDownRequest{..}"
showInward  MasterDone = "MasterDone"

-- | A request sent "outward" from the worker to the master
data OutwardMessage a' a b' b r
    = WorkerUpRequest a'   -- | worker is waiting for an element from upstream
    | WorkerDownRespond b  -- | worker has yielded an element downstream
    | WorkerDone r         -- | worker has terminated gracefully
    | WorkerFail String    -- | worker has terminated with an exception
    deriving (Generic)
instance (All Binary a' a b' b r) => Binary (OutwardMessage a' a b' b r)

showOutward :: OutwardMessage a' a b' b r -> String
showOutward (WorkerUpRequest _) = "WorkerUpRequest{..}"
showOutward (WorkerDownRespond _) = "WorkerDownRespond{..}"
showOutward (WorkerDone _) = "WorkerDone{..}"
showOutward (WorkerFail e) = "WorkerFail "++e

type All cls a a' b' b r = ((cls a, cls a', cls b', cls b, cls r) :: Constraint)

type Chans a = (SendChan a, ReceiveChan a)

data WorkerFailException = WorkerFailed String
                         | WorkerKilled ProcessStatus
                         deriving (Show, Eq, Ord)

instance Exception WorkerFailException

forkPipe :: forall a' a b' b m n r.
            ( MonadIO (Safe.Base n), Safe.MonadSafe n
            , All Binary a' a b' b r, MonadIO m)
         => (forall x. m x -> IO x)
         -> Proxy a' a b' b m r
         -> n (Proxy a' a b' b n r)
forkPipe runM pipe = do
    (sendA, recvA) <- liftIO newPipeChannel :: n (Chans (InwardMessage a' a b' b r))
    (sendB, recvB) <- liftIO newPipeChannel :: n (Chans (OutwardMessage a' a b' b r))
    pid <- liftIO $ forkProcess (worker recvA sendB)

    -- Ensure children are reaped
    exitCode <- liftIO newEmptyTMVarIO
    _ <- liftIO $ forkIO $ do
        dbg $ "waiting on "++show pid
        mx <- getProcessStatus True False pid
        dbg $ "pid "++show pid++" "++show mx
        case mx of
          Just code -> atomically $ putTMVar exitCode code
          Nothing   -> return ()

    rkey <- Safe.register $ liftIO $ do
        let term _ = signalProcess sigINT pid
        dbg $ "pid downing"
        Safe.handleAll term $ send sendA MasterDone
        mx <- getProcessStatus True False pid
        dbg $ "pid "++show pid++" "++show mx

    let checkExited exc = liftIO $ do
            dbg $ "checking exited "++show pid
            mcode <- atomically $ tryTakeTMVar exitCode
            case mcode of
              Just code -> throwM $ WorkerKilled code
              Nothing -> throwM exc
    return $ handleAll checkExited $ master rkey recvB sendA
  where
    master :: Safe.ReleaseKey
           -> ReceiveChan (OutwardMessage a' a b' b r)
           -> SendChan (InwardMessage a' a b' b r)
           -> Proxy a' a b' b n r
    master rkey recvC sendC = go
      where
        go = do
            req <- liftIO $ receive recvC
            liftIO $ dbg $ "master: "++showOutward req
            case req of
              WorkerUpRequest   a' -> P.Request a' goRequest
              WorkerDownRespond b  -> P.Respond b  goRespond
              WorkerDone        r  -> Safe.release rkey >> pure r
              WorkerFail        e  -> Safe.release rkey >> throwM (WorkerFailed e)
        goRequest a  = liftIO (send sendC (MasterUpRespond a)) >> go
        goRespond b' = liftIO (send sendC (MasterDownRequest b')) >> go

    worker :: ReceiveChan (InwardMessage a' a b' b r)
           -> SendChan (OutwardMessage a' a b' b r)
           -> IO ()
    worker recvC sendC = do
        setNumCapabilities 1
        Safe.handle onError $ runM $ go pipe
      where
        onError (SomeException e) = do
            liftIO $ dbg $ "Worker: "++show e
            send sendC (WorkerFail $ show e)

        go (P.Request a' cont) = do
            liftIO $ dbg "worker: Request"
            liftIO $ send sendC (WorkerUpRequest a')
            reply <- liftIO $ receive recvC
            liftIO $ dbg $ showInward reply
            case reply of
              MasterUpRespond   a -> go $ cont a
              MasterDownRequest _ -> error "Worker saw MasterDownRequest"
              MasterDone          -> return ()
        go (P.Respond b cont) = do
            liftIO $ dbg "worker: Respond"
            liftIO $ send sendC (WorkerDownRespond b)
            reply <- liftIO $ receive recvC
            liftIO $ dbg $ showInward reply
            case reply of
              MasterUpRespond   _  -> error "Worker saw MasterUpRespond"
              MasterDownRequest b' -> go $ cont b'
              MasterDone           -> return ()
        go (P.M m) = m >>= go
        go (P.Pure r) = liftIO $ send sendC (WorkerDone r)

dbg :: MonadIO m => String -> m ()
dbg x | debug = liftIO $ BS.hPutStrLn stderr $ BS.pack x
dbg _ = return ()
