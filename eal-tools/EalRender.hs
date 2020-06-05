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
import Data.Either (partitionEithers)
import Data.List.Split 
import Network.URI (unEscapeString)


type PageName = T.Text

data EalRenders = EalRenders { sourcePageName :: PageName
                             , targetEntity :: PageId
                             , contextParagraph :: AnnotatedText
                             , contextSentence :: AnnotatedText
                             , trueAspectId :: AspectId
                             , trueAspect :: Maybe Aspect
                             , candidateAspects :: [Aspect]
                             , instanceId :: EALId
                             }
                deriving (Show, Eq)


opts :: Parser (IO ())
opts = subparser
    $  cmd "context"    (
       renderContext 
       <$> readEalExamples' 
       <*> option auto (short 'n' <> long "number" <> help "number of examples to output"  <> metavar "INT" )
    )
    <>  cmd "aspect"    (
       renderAspect 
       <$> readEalExamples' 
       <*> option auto (short 'n' <> long "number" <> help "number of examples to output"  <> metavar "INT" )
    )
  where
    cmd name action = command name (info (helper <*> action) fullDesc)


renderContext :: IO [AspectLinkExampleOrError] -> Int -> IO()
renderContext readEalExample numExamples  = do
    ealExamples <- readEalExample
    let 
        ealRenders :: [EalRenders]
        ealRenders = take numExamples
                   $ fmap convertToRender $ snd $ partitionEithers ealExamples


    T.putStrLn $ escapeLatex $ T.unlines $ fmap renderContextView ealRenders



renderAspect :: IO [AspectLinkExampleOrError] -> Int -> IO()
renderAspect readEalExample numExamples  = do
    ealExamples <- readEalExample
    let 
        ealRenders :: [EalRenders]
        ealRenders = take numExamples
                   $ fmap convertToRender $ snd $ partitionEithers ealExamples


    T.putStrLn $ escapeLatex $ T.unlines $ fmap renderAspectView ealRenders

escapeLatex :: T.Text -> T.Text
escapeLatex text =
    T.replace "&" "\\&"
    $ T.replace "#" "\\#"
    $ T.replace "%" "\\%"
    $ T.replace "$" "\\$"
    $ text

renderContextView :: EalRenders -> T.Text
renderContextView EalRenders{trueAspect = Just trueAspect, ..} =
        T.unlines 
        $ fmap (T.intercalate "") 
        $   [ [ "\\begin{description}" ]
            , [ " \\item[Source Page:] ", sourcePageName ]
            , [ " \\item[True Aspect:] ", (pageIdToTitle targetEntity),"/", (aspect_name trueAspect) ]
            , [ "\\end{description}" ]
            , [ "\\begin{quote}" ]
            , [ renderAnnotatedText targetEntity contextParagraph ]
            , [ "\\end{quote}" ]
            , [ "\\bigskip" ]
            , [ "" ]
            ]

renderAspectView :: EalRenders -> T.Text
renderAspectView EalRenders{trueAspect = Just trueAspect, candidateAspects = candidateAspects, ..} =
        T.unlines $ fmap (renderSingleAspectView targetEntity) candidateAspects

renderSingleAspectView :: PageId -> Aspect -> T.Text
renderSingleAspectView targetEntity Aspect{..} =
        T.unlines 
        $ fmap (T.intercalate "") 
        $   [ [ "\\paragraph{",  (pageIdToTitle targetEntity),"/", aspect_name, "}" ]
            , [ "\\begin{quote}" ]
            , [ renderAnnotatedText targetEntity aspect_content ]
            , [ "\\end{quote}" ]
            , [ "\\bigskip" ]
            , [ "" ]
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
        entityOffsets = [[start, end]  | EntityMention{..}  <- entities ] 
        textSegments :: [T.Text]
        textSegments  = splitAts content $ concat entityOffsets
    in T.replace "\n" "\n\n" $ interspace "\\entity{" "}" textSegments
    where interspace:: T.Text -> T.Text -> [T.Text] -> T.Text
          interspace beforeStr afterStr (headSegment:textSegments) =
            let textChunks :: [[T.Text]]
                textChunks = Data.List.Split.chunksOf 2 textSegments

            in T.intercalate "" 
               $ (headSegment :
                 fmap (\[t1,t2] -> beforeStr <> t1 <> afterStr <> t2) textChunks
               )
        



convertToRender :: AspectLinkExample -> EalRenders
convertToRender AspectLinkExample{ id=instanceId
                                       , context = Context{ target_entity = targetEntity
                                                          , location = Location{ page_title = sourcePageName} 
                                                          , paragraph = contextParagraph
                                                          , sentence = contextSentence
                                                          } 
                                       , true_aspect = trueAspectId
                                       , candidate_aspects = candidateAspectObjs                  
                                 } =
    let trueAspect = [ a 
                     | a@Aspect{aspect_id} <- candidateAspectObjs
                     , aspect_id == trueAspectId
                     ]
    in EalRenders { instanceId = instanceId
                    , sourcePageName = sourcePageName
                    , targetEntity = targetEntity
                    , contextSentence = contextSentence
                    , contextParagraph = contextParagraph
                    , trueAspectId = trueAspectId
                    , trueAspect = listToMaybe trueAspect
                    , candidateAspects = candidateAspectObjs
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
  let offsets' :: [Int]
      offsets' = sort offsets
      lengths :: [Int]
      lengths = zipWith (-) offsets' (0:offsets')
      (last, splits) = mapAccumL (\txt n -> swap $ T.splitAt n txt) text lengths
  in splits ++ [last]

{-# DEPRECATED pageIdToTitle "rather, expose page title of target entity" #-}
pageIdToTitle :: EalData.PageId -> EalData.PageTitle
pageIdToTitle pid =
    T.tail $ T.dropWhile (/=':')
    $ T.pack $ unEscapeString $ T.unpack pid


main :: IO ()
main = join $ execParser (info (helper <*> opts) mempty)
