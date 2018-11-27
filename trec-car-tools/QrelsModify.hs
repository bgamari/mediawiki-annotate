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

import CAR.Types
import CAR.ToolVersion
import CAR.QRelFile as QF
import CAR.TocFile as TocFile

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS



import SimplIR.Format.TrecRunFile as TrecRunFile
import CAR.AnnotationsFile as AnnsFile

import Debug.Trace


{-
automatic: intersection ground truth and paraCorpus
manual: intersection of assessments and paracorpus
agreement-manual-auto: intersection of assessments and ground truth
rouge-auto: same as ground truth
rouge-manual: same as assessments
-}


opts :: Parser (IO ())
opts = subparser
    $  cmd "derive-automatic"   deriveAutomaticCmd
    <> cmd "derive-manual"   deriveManualCmd
    <> cmd "derive-agreement-manual-automatic"   deriveAgreementManualAutoCmd
    <> cmd "derive-rouge-manual"   deriveRougeManualCmd
    <> cmd "derive-rouge-automatic"   deriveRougeAutomaticCmd
   -- <> cmd "entity-fix-id" entityFixId'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    articleSwitch = switch (long "drop-article-level" <> help "Remove article-level ground truth")
    sectionSwitch = switch (long "drop-section-level" <> help "Remove section-level ground truth")

    pagesFile = argument str (help "Pages file" <> metavar "CBOR")
    indexedParagraphCorpus = option (TocFile.IndexedCborPath <$> str) (long "paragraph-corpus" <> help "Paragraph corpus available to participants")
    inputFile = loadQrels <$> argument str (help "Input qrels file" <> metavar "QRELSFILE")
                          <*> articleSwitch <*> sectionSwitch
    secondGroundTruth = loadQrels <$> option str (short 'q' <> long "ground-truth" <> help "Ground truth as qrels file" <> metavar "QRELSFILE")
                                  <*> articleSwitch <*> sectionSwitch
    outputFile = option str (long "output" <> short 'o' <> help "Output qrels file" <> metavar "QRELSFILE")

    loadQrels qrelsFile articleSwitch sectionSwitch = do
        content <- readParagraphQRel qrelsFile :: IO [QF.Annotation T.Text]
        let content' = filter dropAnnotations content
        return content'
      where dropAnnotations (Annotation (SectionPath _page []) _ _) = not articleSwitch
            dropAnnotations (Annotation (SectionPath _page _) _ _) = not sectionSwitch

    deriveAutomaticCmd =
        deriveManual <$> secondGroundTruth <*> outputFile <*> indexedParagraphCorpus
    deriveManualCmd =
        deriveManual <$> inputFile <*> outputFile <*> indexedParagraphCorpus
    deriveAgreementManualAutoCmd =
        deriveAgreementManualAuto <$> inputFile <*> outputFile <*> secondGroundTruth
    deriveRougeAutomaticCmd =
        noop <$> secondGroundTruth <*> outputFile
    deriveRougeManualCmd =
        noop <$> inputFile <*> outputFile



    deriveManual :: IO [Annotation T.Text] -> FilePath -> TocFile.IndexedCborPath ParagraphId Paragraph -> IO ()
    deriveManual annotations' outputQrelsFile indexedParagraphCorpus = do
        annotations <- annotations'
        indexedParas <- TocFile.open indexedParagraphCorpus
--         annotations <- readParagraphQRel inputQrelsFile :: IO [QF.Annotation T.Text]
        let filteredAnnotations = filter (dropX indexedParas) annotations
        writeParagraphQRel outputQrelsFile filteredAnnotations
      where dropX :: IndexedCbor ParagraphId Paragraph -> QF.Annotation T.Text -> Bool
            dropX indexedParas (Annotation qid doc rel) =
                 isJust (TocFile.lookup doc indexedParas)

    noop ::  IO [Annotation T.Text]  -> FilePath -> IO ()
    noop annotations' outputQrelsFile  = do
        annotations <- annotations' -- readParagraphQRel inputQrelsFile :: IO [QF.Annotation T.Text]
        writeParagraphQRel outputQrelsFile annotations


    deriveAgreementManualAuto ::  IO [Annotation T.Text]  -> FilePath ->  IO [Annotation T.Text]  -> IO ()
    deriveAgreementManualAuto annotations' outputQrelsFile inputGroundTruthQrels' = do
        annotations <- annotations' -- readParagraphQRel inputQrelsFile :: IO [QF.Annotation T.Text]
        groundTruthAnnotations <- inputGroundTruthQrels' -- readParagraphQRel inputGroundTruthQrels :: IO [QF.Annotation T.Text]

        let groundTruthHashed :: HM.HashMap SectionPath (HS.HashSet ParagraphId)
            groundTruthHashed = HM.fromListWith (<>)
                              $ [ (qid, HS.singleton doc)
                                | (Annotation qid doc rel) <- groundTruthAnnotations
                                , read (T.unpack rel) > 0
                                ]
            isInGroundTruth qid paraId =
                maybe False (paraId `HS.member`) (qid `HM.lookup` groundTruthHashed)

        let filteredAnnotations = filter (dropX isInGroundTruth) annotations
        writeParagraphQRel outputQrelsFile filteredAnnotations
      where dropX :: (SectionPath -> ParagraphId -> Bool) -> QF.Annotation T.Text -> Bool
            dropX isInGroundTruth (Annotation qid doc rel) =
                 isInGroundTruth qid doc

--     entityFixId :: FilePath -> FilePath -> FilePath -> IO ()
--     entityFixId pagesFile inputRunFile outputQrelsFile = do
--         unprocessedPages <- openAnnotations pagesFile
--         let pageNameTranslate :: HM.HashMap PageName PageId
--             pageNameTranslate = HM.fromList
--                 $ fmap (\page -> (pageName page, pageId page))
--                 $ AnnsFile.pages unprocessedPages
--
--         rankings <- readStringRun inputRunFile :: IO [StringRankingEntry]
--         let filteredRankings = map (entityFixIdFromRankEntry pageNameTranslate) rankings
--         writeEntityRun outputQrelsFile filteredRankings
--       where entityFixIdFromRankEntry :: (HM.HashMap PageName PageId) -> StringRankingEntry -> EntityRankingEntry
--             entityFixIdFromRankEntry pageNameTranslate r =
--                 let entityPageName :: PageName
--                     entityPageName = packPageName $ T.unpack
--                                    $ T.replace "enwiki:" ""
--                                    $ T.replace "%20" " "
--                                    $ carDocument r -- pretends to be ID but is not
--                     defaultEntityId :: PageId
--                     defaultEntityId = packPageId $ T.unpack $ carDocument r
--                     entityId :: PageId
--                     entityId =
--                              fromMaybe defaultEntityId
--                              $ HM.lookup entityPageName pageNameTranslate
--
--                 in trace ( show (defaultEntityId == entityId) <> "\t" <> show entityPageName <> "\t" <> show entityId) $ r {carDocument = entityId}
--


-- type StringRankingEntry = RankingEntry' T.Text
--
--
-- readStringRun :: FilePath -> IO [StringRankingEntry]
-- readStringRun path =
    -- handle (throwIO . ReadRunError path)
--     map (toCarRankingEntry  id) <$> QF.readRunFile path

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
