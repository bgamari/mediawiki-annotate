{-# LANGUAGE OverloadedStrings #-}

import TagMe
import System.Environment
import Data.Text as T
main :: IO ()
main = do
    tagMeToken <- Token . T.pack <$> getEnv "TAG_ME_TOKEN"
    env <- mkTagMeEnv

    let tagMeOptions = TagMeOptions { inclAbstract = False
                                    , inclCategories = False
                                    , isTweet = False
                                    , isLongText = False
                                    , language = langEn
                                    }
    result <- annotateWithEntityLinksConf env tagMeToken "Schumacher won the race in Indianapolis" tagMeOptions
    putStrLn $ show result
    return ()