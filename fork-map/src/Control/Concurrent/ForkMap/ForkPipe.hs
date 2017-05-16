{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StaticPointers #-}

module Control.Concurrent.ForkMap.ForkPipe
    ( forkPipe
    , Dicts
    , RunAction(..)
    , WorkerFailException(..)
    -- * Internal
    , wrapperMain
    ) where

import Control.Exception
import Data.Kind (Constraint)
import GHC.Generics
import System.IO
import System.Environment
import System.FilePath
import Data.Typeable (Typeable)

import Pipes
import Pipes.Safe as Safe
import qualified Pipes.Internal as P
import Data.Binary
import System.Posix.Signals
import System.Posix.DynamicLinker
import System.Process
import Control.Concurrent
import Control.Concurrent.STM
import System.Exit
import Control.Distributed.Closure

import qualified Data.ByteString.Char8 as BS

import Paths_fork_map
import Control.Concurrent.ForkMap.FifoChannel

debugEnabled :: Bool
debugEnabled = False

-- | A request "inward" from the master to the worker.
data InwardMessage a' a b' b r
    = MasterUpRespond a    -- ^ upstream node on master has yielded
    | MasterDownRequest b' -- ^ downstream node on master has awaited
    | MasterDone           -- ^ master has terminated
    deriving (Generic)
instance (All Binary a' a b' b r) => Binary (InwardMessage a' a b' b r)

showInward :: InwardMessage a' a b' b r -> String
showInward (MasterUpRespond _) = "MasterUpRespond{..}"
showInward (MasterDownRequest _) = "MasterDownRequest{..}"
showInward  MasterDone = "MasterDone"

-- | A request sent "outward" from the worker to the master
data OutwardMessage a' a b' b r
    = WorkerUpRequest a'   -- ^ worker is waiting for an element from upstream
    | WorkerDownRespond b  -- ^ worker has yielded an element downstream
    | WorkerDone r         -- ^ worker has terminated gracefully
    | WorkerFail String    -- ^ worker has terminated with an exception
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
                         | WorkerKilled ExitCode
                         deriving (Show, Eq, Ord)

instance Exception WorkerFailException

-- | Necessary due to impredicativity.
data RunAction m = RunAction (forall x. m x -> IO x)

type Dicts a' a b' b m r = (Dict (All Binary a' a b' b r, MonadIO m))

forkPipe :: forall a' a b' b m n r.
            ( MonadIO (Safe.Base n), Safe.MonadSafe n, MonadIO m
            , All Typeable a' a b' b r, Typeable m
            , All Binary a' a b' b r )
         => Closure (Dicts a' a b' b m r)
         -> Closure (RunAction m)
         -> Closure (Proxy a' a b' b m r)
         -> n (Proxy a' a b' b n r)
forkPipe dicts runM pipe = do
    binDir <- liftIO getBinDir
    (inSend, inRecv) <- liftIO newPipeChannel :: n (Chans (InwardMessage a' a b' b r))
    (outSend, outRecv) <- liftIO newPipeChannel :: n (Chans (OutwardMessage a' a b' b r))
    (configSend, configRecv) <- liftIO newPipeChannel :: n (Chans (Closure ConfigMessage))
    workerH <- liftIO $ do
        inRecvFd  <- receiveChanToFd inRecv
        outSendFd <- sendChanToFd outSend
        configRecvFd <- receiveChanToFd configRecv
        execPath <- getExecutablePath
        spawnProcess
                 (binDir </> "fork-map-wrapper")
                 [ execPath, show configRecvFd, show inRecvFd, show outSendFd ]

    liftIO $ send configSend $ static ConfigMessage `cap` runM `cap` pipe `cap` dicts
    liftIO $ closeSendChan configSend
    let pid = 0 --- TODO
    debug $ "started worker "++show pid

    -- Ensure children are reaped
    exitCode <- liftIO newEmptyTMVarIO
    debug "register"
    rkey <- Safe.register $ liftIO $ do
        debug $ "bringing down pid "++show pid
        let term _ = terminateProcess workerH
        Safe.handleAll term $ send inSend MasterDone
        debug $ "sent "++show pid
        mx <- getProcessExitCode workerH
        debug $ "pid "++show pid++" "++show mx
        case mx of
          Just code -> atomically $ putTMVar exitCode code
          Nothing   -> return ()

    let checkExited exc = liftIO $ do
            debug $ "checking exited "++show pid
            mcode <- atomically $ tryTakeTMVar exitCode
            case mcode of
              Just code -> throwM $ WorkerKilled code
              Nothing -> throwM exc

    debug "start master"
    return $ handleAll checkExited $ master rkey outRecv inSend

data ConfigMessage
    = forall a' a b' b m r.
      ConfigMessage { runAction :: RunAction m
                    , proxy :: Proxy a' a b' b m r
                    , dicts :: Dicts a' a b' b m r
                    }

wrapperMain :: IO ()
wrapperMain = do
    [execPath, configRecvFd, inRecvFd, outSendFd] <- getArgs
    dl <-  dlopen execPath [RTLD_NOW]
    configRecv <- fdToRecieveChan $ read configRecvFd :: IO (ReceiveChan (Closure ConfigMessage))
    ConfigMessage {dicts=Dict, ..} <- unclosure <$> receive configRecv
    inRecv <- fdToRecieveChan $ read inRecvFd :: IO (ReceiveChan (InwardMessage a' a b' b r))
    outSend <- fdToSendChan $ read outSendFd :: IO (SendChan (OutwardMessage a' a b' b r))
    closeReceiveChan configRecv
    worker runAction proxy inRecv outSend


master :: forall a' a b' b n r.
          ( MonadSafe n, All Binary a' a b' b r )
       => Safe.ReleaseKey
       -> ReceiveChan (OutwardMessage a' a b' b r)
       -> SendChan (InwardMessage a' a b' b r)
       -> Proxy a' a b' b n r
master rkey recvC sendC = go
  where
    go = do
        req <- liftIO $ receive recvC
        debug $ "master: "++showOutward req
        case req of
          WorkerUpRequest   a' -> P.Request a' goRequest
          WorkerDownRespond b  -> P.Respond b  goRespond
          WorkerDone        r  -> Safe.release rkey >> pure r
          WorkerFail        e  -> Safe.release rkey >> throwM (WorkerFailed e)
    goRequest a  = liftIO (send sendC (MasterUpRespond a)) >> go
    goRespond b' = liftIO (send sendC (MasterDownRequest b')) >> go

worker :: forall a' a b' b m r.
          ( All Binary a' a b' b r, MonadIO m )
       => RunAction m
       -> Proxy a' a b' b m r
       -> ReceiveChan (InwardMessage a' a b' b r)
       -> SendChan (OutwardMessage a' a b' b r)
       -> IO ()
worker runMClosure pipe recvC sendC = do
    setNumCapabilities 1
    dbg "started"
    Safe.handle onError $ runM $ go pipe
  where
    RunAction runM = runMClosure

    onError (SomeException e) = do
        dbg $ "exception: "++show e
        send sendC (WorkerFail $ show e)

    dbg s = liftIO $ do
        pid <- return 0 -- TODO
        debug $ "Worker("++show pid++"): "++s

    go (P.Request a' cont) = do
        dbg "Request"
        liftIO $ send sendC (WorkerUpRequest a')
        reply <- liftIO $ receive recvC
        liftIO $ dbg $ showInward reply
        case reply of
          MasterUpRespond   a -> go $ cont a
          MasterDownRequest _ -> fail "Worker saw MasterDownRequest"
          MasterDone          -> exitNow
    go (P.Respond b cont) = do
        dbg "Respond"
        liftIO $ send sendC (WorkerDownRespond b)
        reply <- liftIO $ receive recvC
        liftIO $ dbg $ showInward reply
        case reply of
          MasterUpRespond   _  -> fail "Worker saw MasterUpRespond"
          MasterDownRequest b' -> go $ cont b'
          MasterDone           -> exitNow
    go (P.M m) = dbg "Effect" >> m >>= go
    go (P.Pure r) = liftIO $ send sendC (WorkerDone r)

    exitNow = dbg "exiting"


debug :: MonadIO m => String -> m ()
debug x | debugEnabled = liftIO $ BS.hPutStrLn stderr $ BS.pack x
debug _ = return ()
