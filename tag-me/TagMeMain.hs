{-# LANGUAGE OverloadedStrings #-}

import TagMe
import System.Environment
import Data.Text as T
main :: IO ()
main = do
    tagMeToken <- Token . T.pack <$> getEnv "TAG_ME_TOKEN"

    env <- mkTagMeEnv
    result <- annotateWithEntityLinksConf env tagMeToken "Where did Michele Obama go to school?" (TagMeOptions True True False True langEn)
    putStrLn $ show result
    return ()