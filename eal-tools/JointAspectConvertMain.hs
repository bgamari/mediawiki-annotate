{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Main where

import Development.GitRev
import Control.Monad
import Data.Semigroup hiding (All, Any, option)
import Options.Applicative
import qualified Options.Applicative.Help.Pretty as Pretty
import System.FilePath
import Control.Concurrent (setNumCapabilities)

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Maybe
import qualified Data.List.Split as Split
import Data.Ord
import Data.List
import System.Directory

import qualified SimplIR.Format.TrecRunFile as SimplirRun
import qualified SimplIR.Format.QRel as QRel
import SimplIR.LearningToRank


-- import RankLipsTypes
-- import EntFeatures
import SimplIR.Format.JsonRunQrels
-- import RankDataType


-- import Debug.Trace  as Debug
-- import qualified FeaturesAndSetup as RankLips

-- import qualified TrainAndSave as RankLips
-- import qualified Data.Aeson as Aeson
-- import qualified Data.List as L
-- import Data.Ord (comparing, Down(Down))

import JointAspectFeatureData
import RankDataType




getAspectConvertVersion :: String
getAspectConvertVersion = "Joint-aspect-convert version 1.1"

data ModelVersion = ModelVersionV10 | ModelVersionV11
    deriving (Eq, Read, Show)

opts :: Parser (IO ())
opts = subparser
    $  cmd "version"        doPrintVersion
    <> cmd "export-features"   doExportFeatures'
    <> cmd "conv-runs" doConvMethodRuns'
    <> cmd "export-assocs" doExportAssocs'
    <> cmd "keep-best-entity" keepBestEntity'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
     
    doPrintVersion =
        f <$> optional (option str (short 'v'))
      where 
          f :: Maybe String -> IO()
          f _v = putStrLn $ unlines [ getAspectConvertVersion
                                    -- , gitMsg
                                    ]
        
    doExportFeatures' =
        f <$> argument str (help "Features in Jordan's jsonl.gz format")
          <*> option str (long "out-dir" <> short 'O' <> help "directory to write runfiles to" <> metavar "FILE")     
          <*> option str (long "out" <> short 'o' <> help "output prefix " <> metavar "PREFIX" )
          <*> option ( RankDataField . T.pack <$> str) (long "from-aspect-field" <> metavar "FIELD" <> help "json field representing the first aspect in file" )
          <*> optional (option ( RankDataField . T.pack <$> str) (long "to-aspect-field" <> metavar "FIELD" <> help "json field representing the second aspect in file" ))
          <*> optional (option ( RankDataField . T.pack <$> str) (long "entity-field" <> metavar "FIELD" <> help "json field representing the entity in file" ))
      where
        f :: FilePath -> FilePath -> FilePath -> RankDataField -> Maybe RankDataField -> Maybe RankDataField -> IO()
        f inputJson outputDir outputFile fromAspectField toAspectField entityField = do
            inFeatures <- readJordanJointAspectFormat inputJson
            let entries = M.fromListWith (<>)
                          [ (feature, entries)
                          | jointAspectFeature <- inFeatures
                          , (feature, entries)  <- convertToRunEntries  fromAspectField toAspectField entityField  jointAspectFeature
                          ]


            
            mapM_ (\(f,entries) -> writeGzJsonLRunFile (outFile f) entries ) $ M.toList entries
            putStrLn "?"
          where outFile feature = (outputDir</>outputFile<> "-" <> (T.unpack feature) <.>"jsonl"<.>"gz") 

-- debugTr :: (x -> String) ->  x -> x
-- debugTr msg x = Debug.trace (msg x) $ x

            
    doConvMethodRuns' =
        f <$> option str (long "output" <> short 'o' <> help "location of new run file" <> metavar "FILE")     
          <*> option str (long "run" <> short 'r' <> help "trec_eval run file " <> metavar "RUN" )
          <*> option ( RankDataField . T.pack <$> str) (short 'p' <> long "run-field" <> metavar "FIELD" <> help "json field representing document in file" )
          <*> optional (option ( RankDataField . T.pack <$> str) (short 'm' <> long "method-field" <> metavar "FIELD" <> help "json field representing method in file" ))
      where
        f :: FilePath -> FilePath -> RankDataField -> Maybe RankDataField ->  IO()
        f outputFile runFile runField methodFieldOpt = do
            runData <- readTrecEvalRunFile' id convD runFile
            writeJsonLRunFile outputFile runData
          where  
            convD :: SimplirRun.DocumentName -> SimplirRun.MethodName -> RankData
            convD doc method =
                case methodFieldOpt of
                      Just methodField -> fromListRankData [(runField, (RankDataText doc)), (methodField,  (RankDataText method))]
                      Nothing -> fromListRankData [(runField, (RankDataText doc))]



    doExportAssocs' =
        f <$> argument str (help "Features in Jordan's jsonl.gz format")
          <*> option str (long "out" <> short 'o' <> help "output file " <> metavar "FILE" )
          <*> option ( RankDataField . T.pack <$> str) (long "from-aspect-field" <> metavar "FIELD" <> help "json field representing the first aspect in file" )
          <*> optional (option ( RankDataField . T.pack <$> str) (long "to-aspect-field" <> metavar "FIELD" <> help "json field representing the second aspect in file" ))
          <*> optional (option ( RankDataField . T.pack <$> str) (long "entity-field" <> metavar "FIELD" <> help "json field representing the entity in file" ))
          <*> optional (option ( RankDataField . T.pack <$> str) (long "self-entity-field" <> metavar "FIELD" <> help "json field representing the entity self-referential aspects in file" ))
      where
        f :: FilePath -> FilePath -> RankDataField -> Maybe RankDataField -> Maybe RankDataField -> Maybe RankDataField -> IO()
        f inputJson outFile fromAspectField toAspectField entityField selfEntityField = do
            inFeatures <- readJordanJointAspectFormat inputJson
            let entries = concat $ fmap (convertToAssocEntries  fromAspectField toAspectField entityField selfEntityField)  inFeatures

            writeGzJsonLRunFile outFile entries
            putStrLn $ "Written to "<> outFile


    keepBestEntity' =
        f <$> option str (long "output" <> short 'o' <> help "location of new run file" <> metavar "FILE")     
          <*> option str (long "run" <> short 'r' <> help "trec_eval run file " <> metavar "RUN" )
          <*> option ( RankDataField . T.pack <$> str) (short 'p' <> long "aspect-field" <> metavar "FIELD" <> help "json field representing aspect id" )
          <*> option ( RankDataField . T.pack <$> str) (short 'P' <> long "entity-field" <> metavar "FIELD" <> help "json field that will be representing entity id" )
      where
        f :: FilePath -> FilePath -> RankDataField -> RankDataField ->  IO()
        f outputFile runFile aspectField entityField = do
            runData <- readJsonLRunFile @T.Text runFile
            let runData' =  [ SimplirRun.RankingEntry{documentName=entityDoc ,..} 
                            | SimplirRun.RankingEntry{..} <- runData
                            , let entityDoc = augmentEntityDocument documentName
                            ]
                runData'' = M.fromListWith (keepBest)
                             [ ((queryId, entityId), [entry])
                             | entry@SimplirRun.RankingEntry{..} <- runData'   
                             , let entityId = rankDataLookup entityField documentName
                             ]
                runData''' = M.elems
                           $ M.map (head) runData''             
                             
            writeJsonLRunFile outputFile $ runData'''
          where
            augmentEntityDocument :: RankData -> RankData
            augmentEntityDocument aspectDoc = 
                let Just aspectId = aspectField `M.lookup` (unData aspectDoc)
                    entityData = singletonRankData entityField $ extractEntity aspectId
                in unionRankData aspectDoc entityData
  

        extractEntity :: RankDataValue -> RankDataValue
        extractEntity (RankDataText aspectId) = 
            RankDataText $ T.takeWhile (/= '/')$  aspectId 
        extractEntity (RankDataList aspectId) = 
            error $ "Cant support RankDataList for extractEntity"
            
        keepBest entries1 entries2 =
            [
              maximumOn (\SimplirRun.RankingEntry{..} -> documentRank) (entries1 <> entries2)
            ]
       
maximumOn f = Data.List.maximumBy (comparing f)

readTrecEvalRunFile' :: (SimplirRun.QueryId -> q ) -> (SimplirRun.DocumentName -> SimplirRun.MethodName -> d)
                  -> FilePath  
                  -> IO([SimplirRun.RankingEntry' q d] )
readTrecEvalRunFile' qConv dConv fname =  do
    runData <- SimplirRun.readRunFile fname
    return $ fmap (mapFromRunEntry' qConv dConv) runData



mapFromRunEntry' :: (SimplirRun.QueryId -> q) 
             -> (SimplirRun.DocumentName -> SimplirRun.MethodName -> d)
             -> SimplirRun.RankingEntry 
             -> SimplirRun.RankingEntry' q d
mapFromRunEntry' qConv dConv SimplirRun.RankingEntry{..} =
    SimplirRun.RankingEntry { queryId = (qConv queryId), documentName = (dConv documentName methodName), ..}


main :: IO ()
main = join $ execParser $ info (helper <*> opts) (progDescDoc (Just desc) <> fullDesc)
  where
    desc = Pretty.vcat $ Pretty.punctuate Pretty.linebreak
        [ para [ "Joint Aspect Convert " ]
        ]
    para = Pretty.fillSep . map Pretty.text . foldMap words
  
