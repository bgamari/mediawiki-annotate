{-# LANGUAGE TypeApplications #-}

import Data.Monoid
import qualified Data.ByteString.Lazy as BSL
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
