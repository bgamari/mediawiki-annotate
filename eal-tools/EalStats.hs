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
       dumpTargetEntities 
       <$> readEalExamples' 
       <*> option auto (short 'n' <> long "super-frequency" <> help "frequency to count as super-frequent entity"  <> metavar "INT" )
    )
    <>  cmd "verify-existing-true-aspect"    (
       verifyExistingTrueAspect
       <$> readEalExamples' 
    )
    <>  cmd "min-candidates"    (
       verifyMinCandidates
       <$> readEalExamples' 
       <*> option auto (short 'n' <> long "candidates" <> help "min number of candidates"  <> metavar "INT" )
    )  
  where
    cmd name action = command name (info (helper <*> action) fullDesc)

verifyMinCandidates :: IO [AspectLinkExampleOrError] -> Int -> IO ()
verifyMinCandidates readExamples minCand = do
    examples <- readExamples
    let output = f examples
    T.putStrLn $ T.unlines output
  where
    f :: [AspectLinkExampleOrError] -> [T.Text]
    f eals =
        catMaybes $ fmap verify eals

    verify :: AspectLinkExampleOrError -> Maybe T.Text
    verify (Left msg) = Just $ T.pack msg
    verify (Right AspectLinkExample{true_aspect=trueAspectId, candidate_aspects=aspects}) =
        let found = length aspects
        in if (found < minCand) 
              then Just $ ("" <> trueAspectId <> ": Less than " <> (T.pack  (show minCand))) --  <>" candidates.")
              else Nothing
         


dumpTargetEntities :: IO [AspectLinkExampleOrError] -> Int -> IO()
dumpTargetEntities readExamples cutoff = do
    examples <- readExamples
    let output = f examples
    T.putStrLn $ T.unlines output
    where
    f :: [AspectLinkExampleOrError] -> [T.Text]
    f eals =
        let histogram = 
                HM.fromListWith (+) 
                  $ [(targetEntity, 1 )
                    | Right AspectLinkExample{context=Context{target_entity=targetEntity}} <- eals
                    ]
            mostFreq = [ pair
                        | pair@(entity, count) <- HM.toList histogram
                        , count >= cutoff
                        ]
            topFreq = fmap (\(x,y) -> x <> " "<> (T.pack $ show y)) $ sortOn ( ((-1) *) . snd ) mostFreq
        in  topFreq

verifyExistingTrueAspect :: IO [AspectLinkExampleOrError] -> IO ()
verifyExistingTrueAspect readExamples = do
    examples <- readExamples
    let output = f examples
    T.putStrLn $ T.unlines output
  where
    f :: [AspectLinkExampleOrError] -> [T.Text]
    f eals =
        catMaybes $ fmap verify eals

    verify :: AspectLinkExampleOrError -> Maybe T.Text
    verify (Left msg) = Just $ T.pack msg
    verify (Right AspectLinkExample{true_aspect=trueAspectId, candidate_aspects=aspects}) =
        let found = filter (\Aspect{aspect_id=aspectId} -> trueAspectId == aspectId) aspects    
        in case (length found) of
             1 -> Nothing
             0 -> Just $ "True aspect "<>trueAspectId<> " not found in candidate aspects: " <> (T.unwords $ fmap aspect_id aspects)
             n -> Just $ "Found "<> (T.pack $ show n) <>" matching aspects: " <> (T.unlines $ fmap (T.pack . show) found)
         


readEalExamples' :: Parser (IO [AspectLinkExampleOrError])
readEalExamples' =
    f <$> argument str (help "input file" <> metavar "FILE")
  where
    f :: FilePath -> IO [AspectLinkExampleOrError]
    f inputFile = do 
        readAspectExamples inputFile


main :: IO ()
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
