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

data EalVerification = EalVerification { sourcePage :: PageId
                                       , targetEntity :: PageId
                                       , contextParagraph :: AnnotatedText
                                       , trueAspectId :: AspectId
                                       , trueAspect :: Maybe Aspect
                                       , candidateAspects :: [AspectName]
                                       , id :: EALId
                                       }
                deriving (Show, Eq)

main :: IO ()
main = do
    let fname = "/home/dietz/trec-car/code/mediawiki-annotate/eal-tools/small-eal-v2.4-en-01-01-2020.jsonl"
    ealExamples <- readAspectExamples fname
                :: IO[Either String AspectLinkExample]
    
    let ealVerifications :: [EalVerification]
        ealVerifications = [ convertToVerification eal  
                           | Right eal <- ealExamples
                           ]                 

    putStrLn $ unlines $ fmap renderEAL ealVerifications

renderEAL :: EalVerification -> String
renderEAL EalVerification{trueAspect = Nothing, ..} =
    unlines [ "targetEntity " <> show targetEntity <> " | sourcePage" <> show sourcePage
            , "missing true aspect"
            ]
renderEAL EalVerification{trueAspect = Just trueAspect, ..} =
    unlines [ " sourcePage" <> show sourcePage <> " >> targetEntity " <> show targetEntity
            , T.unpack $ renderAnnotatedText targetEntity contextParagraph
            , "  [ trueAspect " <> show (aspect_name $ trueAspect) <> " ]"
            , T.unpack $ renderAnnotatedText targetEntity (aspect_content trueAspect)
            , ""
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
    in T.intercalate  " ]*[ " textSegments 


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


splitAts ::  T.Text -> [Int] -> [T.Text]
splitAts text offsets = 
  let lengths :: [Int]
      lengths = zipWith (-) offsets (0:offsets)
      (last, splits) = mapAccumL (\txt n -> swap $ T.splitAt n txt) text lengths
  in splits ++ [last]