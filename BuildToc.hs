{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

import Data.Foldable
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Serialise.CBOR as CBOR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import CAR.Types
import Data.Aeson

type Offset = Int

readValuesWithOffsets :: forall a. CBOR.Serialise a
                      => BSL.ByteString -> [(Offset, a)]
readValuesWithOffsets = start 0 . BSL.toChunks
  where
    start :: Offset  -- ^ current offset
          -> [BS.ByteString]
          -> [(Offset, a)]
    start _offset []  = []
    start offset  bss =
        go offset offset CBOR.deserialiseIncremental bss

    go :: Offset          -- ^ offset of beginning of current chunk
       -> Offset          -- ^ start offset of thing currently being decoded
       -> CBOR.Decoder a
       -> [BS.ByteString] -- ^ remaining chunks
       -> [(Offset, a)]
    go !currOff !startOff (CBOR.Partial f)   [] =
        go currOff startOff (f Nothing) []

    go currOff  startOff (CBOR.Partial f)   (bs:bss) =
        go currOff startOff (f (Just bs)) bss

    go currOff  startOff (CBOR.Done bs off x) bss =
        let !currOff' = currOff + fromIntegral off
            bss' | BS.null bs = bss
                 | otherwise  = bs : bss
        in (startOff, x) : start currOff' bss'

    go _currOff _startOff (CBOR.Fail _rest _ err) _ =
        error $ show err

main :: IO ()
main = do
    pages <- readValuesWithOffsets <$> BSL.getContents
    let addPage acc (offset, page) = M.insert (pageName page) offset acc
        toc = foldl' addPage mempty pages
    BSL.putStr $ encode toc
    --mapM_ print pages
