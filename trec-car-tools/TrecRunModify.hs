{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}


import Options.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM


import CAR.Types
import CAR.ToolVersion
import CAR.RunFile as RF
import qualified SimplIR.Format.TrecRunFile as Run

import CAR.AnnotationsFile as CAR

import Debug.Trace


opts :: Parser (IO ())
opts = subparser
    $  cmd "entity-drop-paragraphs"   entityDropParagraphs'
    <> cmd "entity-fix-id" entityFixId'
    <> cmd "drop-duplicates" dropDuplicates'
    <> cmd "drop-query-entities" dropQueryEntities'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    pagesFile = argument str (help "Pages file" <> metavar "CBOR")
    inputFile = argument str (help "Input run file" <> metavar "RUNFILE")
    outputFile = option str (long "output" <> short 'o' <> help "Output run file" <> metavar "RUNFILE")
    entityDropParagraphs' =
        entityDropParagraphs <$> inputFile <*> outputFile
    dropQueryEntities' =
        dropQueryEntities <$> inputFile <*> outputFile

    entityFixId' =
        entityFixId <$> pagesFile <*> inputFile <*> outputFile
    dropDuplicates' =
        dropDuplicates <$> inputFile <*> outputFile



    dropQueryEntities inputRunFile outputRunFile = do
        rankings <- readEntityRun inputRunFile :: IO [RF.EntityRankingEntry]
        let filteredRankings = filter dropQueryEntityFromEntityRankEntry rankings
        writeEntityRun outputRunFile filteredRankings
      where dropQueryEntityFromEntityRankEntry :: EntityRankingEntry -> Bool
            dropQueryEntityFromEntityRankEntry  r =
                let entityName = T.pack $ unpackPageId $ carDocument r --(carDocument r)
                    query = unQueryId $ carQueryId r
                in entityName `T.isPrefixOf` query


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
        pageBundle <- CAR.openPageBundle pagesFile
        let pageNameTranslate :: HM.HashMap PageName PageId
            pageNameTranslate = HM.fromList
                $ (\page -> (pageName page, pageId page))
               <$> CAR.bundleAllPages pageBundle

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

    dropDuplicates :: FilePath -> FilePath -> IO ()
    dropDuplicates inputRunFile outputRunFile = do
        rankings <- readStringRun inputRunFile :: IO [StringRankingEntry]
        let filteredRankings :: [StringRankingEntry]
            filteredRankings = HM.elems $ HM.fromListWith chooseHigher [ ((carQueryId entry, carDocument entry), entry) |  entry <- rankings]
        Run.writeRunFile  outputRunFile $ fmap (RF.fromCarRankingEntry id) filteredRankings
      where chooseHigher :: StringRankingEntry -> StringRankingEntry -> StringRankingEntry
            chooseHigher entry1 entry2 =
               if carScore entry1 >= carScore entry2 then
                    entry1
               else
                    entry2


type StringRankingEntry = RankingEntry' T.Text


readStringRun :: FilePath -> IO [StringRankingEntry]
readStringRun path =
    -- handle (throwIO . ReadRunError path)
    (map (toCarRankingEntry  id) <$> RF.readRunFile path)

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
