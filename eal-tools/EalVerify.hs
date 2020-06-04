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
import Control.Monad (join)

data EalVerification = EalVerification { sourcePage :: PageId
                                       , targetEntity :: PageId
                                       , contextParagraph :: AnnotatedText
                                       , trueAspectId :: AspectId
                                       , trueAspect :: Maybe Aspect
                                       , candidateAspects :: [AspectName]
                                       , id :: EALId
                                       }
                deriving (Show, Eq)


opts :: Parser (IO ())
opts = subparser
    $  cmd "data"    (
       dumpVerify 
       <$> readEalExamples' 
       <*> option auto (short 'n' <> long "number" <> help "number of examples to output"  <> metavar "INT" )
       <*> optional (option auto (short 'k' <> long "per-entity" <> help "number of examples to output"  <> metavar "INT" ))
    )
  where
    cmd name action = command name (info (helper <*> action) fullDesc)


dumpVerify :: IO[AspectLinkExampleOrError ] -> Int -> Maybe Int -> IO()
dumpVerify readEalExample numExamples numExamplesPerEntityMaybe = do
    ealExamples <- readEalExample

    let ealsWithCounts :: [(HM.HashMap PageId Int, Maybe AspectLinkExample)]
        ealsWithCounts = scanl addToAccum (HM.empty, Nothing) ealExamples
                  where addToAccum :: (HM.HashMap PageId Int, Maybe AspectLinkExample) -> AspectLinkExampleOrError -> (HM.HashMap PageId Int, Maybe AspectLinkExample)
                        addToAccum (m, _) (Left _) = (m, Nothing)
                        addToAccum (m,_ ) (Right eal@AspectLinkExample{context = Context{target_entity=entity}}) =
                            (HM.insertWith (+) entity 1  m, Just eal)
        eals = catMaybes 
             $ fmap snd 
             $ filter lessThan ealsWithCounts
                   where lessThan :: (HM.HashMap PageId Int, Maybe AspectLinkExample) -> Bool
                         lessThan (m, Nothing) = False
                         lessThan (m, Just AspectLinkExample{context = Context{target_entity=entity}}) = 
                             (fromMaybe 0 $ entity `HM.lookup` m) <= (fromMaybe 1000000 numExamplesPerEntityMaybe)
                         

        ealVerifications :: [EalVerification]
        ealVerifications = take numExamples
                         $ [ convertToVerification eal  
                           | eal <- eals
                           ]                 
                           


    T.putStrLn $ T.unlines $ fmap renderEAL ealVerifications


renderEAL :: EalVerification -> T.Text
renderEAL EalVerification{trueAspect = Nothing, ..} =
    T.unlines [ "targetEntity " <> targetEntity <> " | sourcePage" <> sourcePage
            , "missing true aspect"
            ]
renderEAL EalVerification{trueAspect = Just trueAspect, id=exampleId, ..} =
    T.unlines 
        $ fmap (T.intercalate "\t") 
        $   [ [" sourcePage", sourcePage , " targetEntity ", targetEntity]
            , [" X ", exampleId, "correct? Y/N", ""]
            , [renderAnnotatedText targetEntity contextParagraph ]
            , ["  [ all Aspects: ", (T.intercalate ", " candidateAspects) <> " ]"]
            , ["  [ trueAspect " <> (aspect_name $ trueAspect) <> " ]"]
            , [renderAnnotatedText targetEntity (aspect_content trueAspect)]
            , [""]
            ]

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
    in wrapAt 200
        $ T.intercalate  " ]*[ " textSegments 


convertToVerification :: AspectLinkExample -> EalVerification
convertToVerification AspectLinkExample{ id=id
                                       , context=Context{ target_entity = targetEntity
                                                        , location = Location{ page_id=sourcePage} 
                                                        , paragraph = contextParagraph
                                                        }
                                       , true_aspect = trueAspectId
                                       , candidate_aspects = candidateAspectObjs                  
                                       } =
    let trueAspect = [ a 
                     | a@Aspect{aspect_id} <- candidateAspectObjs
                     , aspect_id == trueAspectId
                     ]
    in EalVerification { id = id
                    , sourcePage = sourcePage
                    , targetEntity = targetEntity
                    , contextParagraph = contextParagraph
                    , trueAspectId = trueAspectId
                    , trueAspect = listToMaybe trueAspect
                    , candidateAspects = [ aspectName | Aspect{ aspect_name = aspectName} <- candidateAspectObjs]
                    } 


readEalExamples' :: Parser (IO [AspectLinkExampleOrError])
readEalExamples' =
    f <$> argument str (help "input file" <> metavar "FILE")
  where
    f :: FilePath -> IO [AspectLinkExampleOrError]
    f inputFile = do 
        readAspectExamples inputFile


wrapAt :: Int -> T.Text -> T.Text
wrapAt charsPerLine text =
    T.intercalate "\n" 
    $ fmap ((T.intercalate "\n") . (T.chunksOf charsPerLine))
    $ T.splitOn "\n" text

splitAts ::  T.Text -> [Int] -> [T.Text]
splitAts text offsets = 
  let lengths :: [Int]
      lengths = zipWith (-) offsets (0:offsets)
      (last, splits) = mapAccumL (\txt n -> swap $ T.splitAt n txt) text lengths
  in splits ++ [last]


main :: IO ()
main = join $ execParser (info (helper <*> opts) mempty)
