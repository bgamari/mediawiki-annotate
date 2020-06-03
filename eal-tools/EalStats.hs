{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import EalData 
import Data.List
import Data.Tuple
import Data.Ord (Down)
import Options.Applicative
import Options.Applicative.Extra
import Control.Monad (join)
import Data.Hashable (Hashable)


opts :: Parser (IO ())
opts = subparser
    $  cmd "target-entities"    (
       dumpTargetEntities <$> readEalExamples'
    ) 
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    dumpTargetEntities :: IO [AspectLinkExampleOrError] -> IO()
    dumpTargetEntities readExamples = do
        examples <- readExamples
        let output = f examples
        T.putStrLn $ T.unlines output
      where
        f :: [AspectLinkExampleOrError] -> [T.Text]
        f eals =
            let histogram = 
                    unionsWith (+)
                    $ [ HM.singleton targetEntity 1
                      | Right AspectLinkExample{context=Context{target_entity=targetEntity}} <- eals
                      ]
                mostFreq = [ pair
                           | pair@(entity, count) <- HM.toList histogram
                           , count > 0
                           ]
                topFreq = fmap (\(x,y) -> x <> " "<> (T.pack $ show y)) $ sortOn ( ((-1) *) . snd ) mostFreq
            in  topFreq
            -- mapM_ (T.putStrLn . getPageName . pageName) pages


readEalExamples' :: Parser (IO [AspectLinkExampleOrError])
readEalExamples' =
    f <$> argument str (help "input file" <> metavar "FILE")
  where
    f :: FilePath -> IO [AspectLinkExampleOrError]
    f inputFile = do 
        readAspectExamples inputFile


main :: IO ()
-- main = join $ execParser' 1 (helper <*> opts) mempty
main = join $ execParser (info (helper <*> opts) mempty)



renderAnnotatedText :: PageId -> AnnotatedText -> T.Text
renderAnnotatedText targetEntityId (AnnotatedText {..})  = 
    let targetEntityMentions = 
            [ e
            | e@EntityMention{entity_id = entityId} <- entities
            , entityId == targetEntityId
            ]
        targetOffsets :: [[Int]]    
        targetOffsets = [[start, end]  | EntityMention{..} <- targetEntityMentions]    
        textSegments :: [T.Text]
        textSegments  = splitAts content $ concat targetOffsets
    in T.intercalate  " ]*[ " textSegments 




splitAts ::  T.Text -> [Int] -> [T.Text]
splitAts text offsets = 
  let lengths :: [Int]
      lengths = zipWith (-) offsets (0:offsets)
      (last, splits) = mapAccumL (\txt n -> swap $ T.splitAt n txt) text lengths
  in splits ++ [last]

unionsWith :: (Foldable g, Eq k, Hashable k)  => (v -> v -> v) -> g (HM.HashMap k v) -> HM.HashMap k v
unionsWith f = foldl' (HM.unionWith f) mempty
 