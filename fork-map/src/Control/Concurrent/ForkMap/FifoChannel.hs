module Control.Concurrent.ForkMap.FifoChannel
    ( ReceiveChan
    , SendChan
    , newPipeChannel
    , send
    , receive
    ) where

import System.Posix.IO.ByteString
import qualified Data.ByteString.Lazy as BSL
import Data.Binary
import Data.Binary.Get as B
import Data.Binary.Put as B
import System.IO

newtype ReceiveChan a = ReceiveChan Handle
newtype SendChan a = SendChan Handle

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
    (read, write) <- createPipe
    readH <- fdToHandle read
    writeH <- fdToHandle write
    return (SendChan writeH, ReceiveChan readH)

readInt :: Handle -> IO Int
readInt h = fromIntegral . B.runGet B.getInt64host <$> BSL.hGet h 8

writeInt :: Handle -> Int -> IO ()
writeInt h = BSL.hPut h . B.runPut . B.putInt64host . fromIntegral
