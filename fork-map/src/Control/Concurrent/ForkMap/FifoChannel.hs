module Control.Concurrent.ForkMap.FifoChannel
    ( ReceiveChan
    , SendChan
      -- * Creation
    , openSendChan
    , openReceiveChan
    , newPipeChannel
      -- * Closing
    , closeReceiveChan
    , closeSendChan
      -- * IO
    , send
    , receive
      -- * To/from fds
    , fdToSendChan
    , fdToRecieveChan
    , sendChanToFd
    , receiveChanToFd
    ) where

import System.Posix.IO.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get as B
import Data.Binary.Put as B
import System.IO
import System.Posix.Types (Fd)

newtype ReceiveChan a = ReceiveChan Handle
newtype SendChan a = SendChan Handle

openSendChan :: FilePath -> IO (SendChan a)
openSendChan path = SendChan <$> openFile path WriteMode

openReceiveChan :: FilePath -> IO (ReceiveChan a)
openReceiveChan path = ReceiveChan <$> openFile path ReadMode

fdToSendChan :: Fd -> IO (SendChan a)
fdToSendChan = fmap SendChan . fdToHandle

fdToRecieveChan :: Fd -> IO (ReceiveChan a)
fdToRecieveChan = fmap ReceiveChan . fdToHandle

sendChanToFd :: SendChan a -> IO Fd
sendChanToFd (SendChan h) = handleToFd h

receiveChanToFd :: ReceiveChan a -> IO Fd
receiveChanToFd (ReceiveChan h) = handleToFd h

send :: Binary a => SendChan a -> a -> IO ()
send (SendChan h) x = do
    let bs = encode x
    writeInt h $ fromIntegral (BSL.length bs)
    BSL.hPut h bs
    hFlush h

receive :: Binary a => ReceiveChan a -> IO a
receive (ReceiveChan h) = do
    len <- readInt h
    decode <$> BSL.hGet h len

newPipeChannel :: IO (SendChan a, ReceiveChan a)
newPipeChannel = do
    (readFd, writeFd) <- createPipe
    readH <- fdToHandle readFd
    writeH <- fdToHandle writeFd
    return (SendChan writeH, ReceiveChan readH)

closeReceiveChan :: ReceiveChan a -> IO ()
closeReceiveChan (ReceiveChan h) = hClose h

closeSendChan :: SendChan a -> IO ()
closeSendChan (SendChan h) = hClose h

readInt :: Handle -> IO Int
readInt h = fromIntegral . B.runGet B.getInt64host <$> BSL.hGet h 8

writeInt :: Handle -> Int -> IO ()
writeInt h = BSL.hPut h . B.runPut . B.putInt64host . fromIntegral
