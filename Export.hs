{-# LANGUAGE TypeApplications #-}

import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Types
import Data.MediaWiki.Markup

main :: IO ()
main = do
    c <- readValues @Page <$> BSL.getContents
    mapM_ (mapM_ (putStrLn . prettySkeleton) . pageSkeleton) c

prettySkeleton :: PageSkeleton -> String
prettySkeleton = go 1
  where
    go :: Int -> PageSkeleton -> String
    go n (Section name children) =
        unlines
        $ [ replicate n '#' ++ " " ++ T.unpack name]
          <> map (go (n+1)) children
          <> [""]
    go _ (Para paras) =
        unwords (map goPara paras) ++ "\n"

    goPara (ParaText t) = T.unpack t
    goPara (ParaLink (PageName name) anchor) =
        "["<>T.unpack anchor<>"]("<>name<>")"
