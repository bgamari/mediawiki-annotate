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
    mapM_ putPage c

putPage :: Page -> IO ()
putPage p = do
    putStrLn $ "\n~~~~~~~~" ++ show (pageName p)
    mapM_ (putStrLn . prettySkeleton) (pageSkeleton p)

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
        concatMap goPara paras ++ "\n"

    goPara (ParaText t) = T.unpack t
    goPara (ParaLink (PageName name) anchor) =
        "["<>T.unpack anchor<>"]("<>T.unpack name<>")"
