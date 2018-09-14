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
import CAR.ToolVersion
import CAR.QRelFile
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
    pagesFile = argument str (help "Pages file" <> metavar "CBOR")
    indexedParagraphCorpus = option (TocFile.IndexedCborPath <$> str) (long "paragraph-corpus" <> help "Paragraph corpus available to participants")
    inputFile = argument str (help "Input qrels file" <> metavar "QRELSFILE")
    secondGroundTruth = option str (short 'q' <> long "ground-truth" <> help "Ground truth as qrels file" <> metavar "QRELSFILE")
    outputFile = option str (long "output" <> short 'o' <> help "Output qrels file" <> metavar "QRELSFILE")
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



    deriveManual :: FilePath -> FilePath -> TocFile.IndexedCborPath ParagraphId Paragraph -> IO ()
    deriveManual inputQrelsFile outputQrelsFile indexedParagraphCorpus = do
        indexedParas <- TocFile.open indexedParagraphCorpus
        annotations <- readParagraphQRel inputQrelsFile :: IO [QF.Annotation T.Text]
        let filteredAnnotations = filter (dropX indexedParas) annotations
        writeParagraphQRel outputQrelsFile filteredAnnotations
      where dropX :: IndexedCbor ParagraphId Paragraph -> QF.Annotation T.Text -> Bool
            dropX indexedParas (Annotation qid doc rel) =
                 isJust (TocFile.lookup doc indexedParas)

    noop :: FilePath -> FilePath -> IO ()
    noop inputQrelsFile outputQrelsFile  = do
        annotations <- readParagraphQRel inputQrelsFile :: IO [QF.Annotation T.Text]
        writeParagraphQRel outputQrelsFile annotations


    deriveAgreementManualAuto :: FilePath -> FilePath -> FilePath -> IO ()
    deriveAgreementManualAuto inputQrelsFile outputQrelsFile inputGroundTruthQrels = do
        annotations <- readParagraphQRel inputQrelsFile :: IO [QF.Annotation T.Text]
        groundTruthAnnotations <- readParagraphQRel inputGroundTruthQrels :: IO [QF.Annotation T.Text]

        let groundTruthHashed :: HM.HashMap SectionPath (HS.HashSet ParagraphId)
            groundTruthHashed = HM.fromListWith (<>)
                              $ [ (qid, HS.singleton doc)
                                | (Annotation qid doc rel) <- groundTruthAnnotations
                                , rel /= "0"
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
