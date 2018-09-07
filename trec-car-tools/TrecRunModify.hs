{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


import Data.Monoid
import Options.Applicative
import Control.Monad
import Control.DeepSeq
import Data.Maybe
import Data.Hashable
import Codec.Serialise as CBOR
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM


import CAR.Types
import CAR.TocFile as Toc
import CAR.ToolVersion
import CAR.RunFile
import CAR.RunFile as RF

import SimplIR.Format.TrecRunFile as TrecRunFile
import CAR.AnnotationsFile as AnnsFile

import Debug.Trace


opts :: Parser (IO ())
opts = subparser
    $  cmd "entity-drop-paragraphs"   entityDropParagraphs'
    <> cmd "entity-fix-id" entityFixId'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    pagesFile = argument str (help "Pages file" <> metavar "CBOR")
    inputFile = argument str (help "Input run file" <> metavar "RUNFILE")
    outputFile = option str (long "output" <> short 'o' <> help "Output run file" <> metavar "RUNFILE")
    entityDropParagraphs' =
        entityDropParagraphs <$> inputFile <*> outputFile

    entityFixId' =
        entityFixId <$> pagesFile <*> inputFile <*> outputFile



    entityDropParagraphs inputRunFile outputRunFile = do
        rankings <- readEntityParagraphRun inputRunFile :: IO [RF.PassageEntityRankingEntry]
        let filteredRankings = map dropParagraphFromEntityRankEntry rankings
        writeEntityRun outputRunFile filteredRankings
      where dropParagraphFromEntityRankEntry :: PassageEntityRankingEntry -> EntityRankingEntry
            dropParagraphFromEntityRankEntry  r =
                let entityName = carEntity r --(carDocument r)
                in r {carDocument = entityName}

    entityFixId :: FilePath -> FilePath -> FilePath -> IO ()
    entityFixId pagesFile inputRunFile outputRunFile = do
        unprocessedPages <- openAnnotations pagesFile
        let pageNameTranslate :: HM.HashMap PageName PageId
            pageNameTranslate = HM.fromList
                $ fmap (\page -> (pageName page, pageId page))
                $ AnnsFile.pages unprocessedPages

        rankings <- readStringRun inputRunFile :: IO [StringRankingEntry]
        let filteredRankings = map (entityFixIdFromRankEntry pageNameTranslate) rankings
        writeEntityRun outputRunFile filteredRankings
      where entityFixIdFromRankEntry :: (HM.HashMap PageName PageId) -> StringRankingEntry -> EntityRankingEntry
            entityFixIdFromRankEntry pageNameTranslate r =
                let entityPageName :: PageName
                    entityPageName = packPageName $ T.unpack
                                   $ T.replace "enwiki:" ""
                                   $ T.replace "%20" " "
                                   $ carDocument r -- pretends to be ID but is not
                    defaultEntityId :: PageId
                    defaultEntityId = packPageId $ T.unpack $ carDocument r
                    entityId :: PageId
                    entityId =
                             fromMaybe defaultEntityId
                             $ HM.lookup entityPageName pageNameTranslate

                in trace ( show (defaultEntityId == entityId) <> "\t" <> show entityPageName <> "\t" <> show entityId) $ r {carDocument = entityId}



type StringRankingEntry = RankingEntry' T.Text


readStringRun :: FilePath -> IO [StringRankingEntry]
readStringRun path =
    -- handle (throwIO . ReadRunError path)
    (map (toCarRankingEntry  id) <$> RF.readRunFile path)

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
