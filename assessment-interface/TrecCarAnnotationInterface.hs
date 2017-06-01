{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


-- | TrecCarAnnotationInterface: Commandline argument reading, trec run loading, merging, shuffling, nubbing
module Main where

import Options.Applicative

import Data.Monoid
import Data.Traversable
import Data.List
import Data.Maybe
import Data.Foldable
import Data.Hashable
import System.FilePath
import System.Directory
import System.FilePath.Glob

import System.Random
import System.Random.Shuffle
import Control.Monad.Random

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HM.Lazy
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Text.Blaze.Html.Renderer.Utf8 as H
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import           Text.Blaze.Html5 ((!))

import CAR.Types
import CAR.CarExports
import qualified CAR.TocFile as TocFile
import PassageViewHtml
import EntityViewHtml
import OutlineViewHtml
import qualified SimplIR.Format.TrecRunFile as TrecRun

import qualified SimplIR.Format.QRel as TrecQrel

import TrecCarRenderHtml
import FileNameLookup

import Debug.Trace

data Opts = Opts { outlinesFile :: FilePath
                 , paragraphFile :: FilePath
                 , entityFile :: FilePath
                 , optsDest :: FilePath
                 , optsShuffle :: Bool
                 , optsTopK :: Int
                 , optsOutlineId :: Maybe String
                 , optsQrelFile :: FilePath
                 , optsTrecPsgRunGlobs :: [FilePath]
                 , optsTrecEntityRunGlobs :: [FilePath]
                 }




trecResultUnionOfRankedItems :: forall item nubKey. (Eq nubKey, Hashable nubKey)
                             => (TrecRun.DocumentName -> item)
                             -> (RankingEntry item -> nubKey) -> Int -> Bool -> [FilePath]
                             -> IO (HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry item])
trecResultUnionOfRankedItems trecRunItemToEntryItem getNubKey optsTopK optsShuffle trecRunFiles = do

    let resultsToTopkMap ::  [TrecRun.RankingEntry] -> HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry item]
        resultsToTopkMap trecRankingContents =
            let resultMap :: HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry item]
                resultMap = HM.fromListWith (++) $
                  [ ( TrecRun.queryId entry
                    , [RankingEntry { entryItem = trecRunItemToEntryItem $ TrecRun.documentName entry
                                    , entryScore = TrecRun.documentScore entry
                                    }]
                    )
                  | entry <- trecRankingContents
                  ]
            in fmap (take optsTopK) resultMap

    files <- mapM TrecRun.readRunFile trecRunFiles
    let unionResultMap = evalRandIO $ mapM condShuffleStuffNub $ foldl' (HM.unionWith (++)) mempty $ fmap resultsToTopkMap files
          where
            condShuffleStuffNub :: [TrecCarRenderHtml.RankingEntry item] -> Rand StdGen [TrecCarRenderHtml.RankingEntry item]    -- todo generify
            condShuffleStuffNub rankElements
              | optsShuffle = shuffleM $ nubBy rankElements
              | otherwise   = return $ nubBy rankElements
            nubBy = HM.elems . HM.fromList . fmap (\rankElem -> (getNubKey $ rankElem, rankElem))
    unionResultMap


-- todo generify  trecQrelParagraphs
trecQrelItems :: forall item. (TrecRun.DocumentName -> Maybe item) -> FilePath
              -> IO (HM.Lazy.HashMap TrecQrel.QueryId [RankingEntry item] )
-- loadParagraphMaybe $ packParagraphId --> loadEntityMaybe $  unpackPageId (not sure)
-- Just paragraph <- pure $ loadParagraphMaybe $ packParagraphId --> Just entity <- pure $ ressurrect Enity(entityPageId, entityPageName)
-- QrelEntry --> EntityQrelEntry     (entryLabel --> entityEntryLabel)


trecQrelItems trecRunItemToEntryItemMaybe qrelfile  = do

    qrelEntries <- TrecQrel.readQRel qrelfile
    let qrelMap =   HM.fromListWith (++)
                    $  [ ( TrecQrel.queryId entry
                        , [QrelEntry { entryItem = item
                                     , entryLabel = fromBinaryRelevance $ TrecQrel.relevance entry
                                     }]
                        )
                      | entry <- qrelEntries
                      ,TrecQrel.relevance entry /= TrecQrel.NotRelevant
                      ,Just item <- pure $ trecRunItemToEntryItemMaybe $ TrecQrel.documentName entry
                      ]
    return qrelMap

opts :: Parser Opts
opts =
    Opts
    <$> argument str (help "outline collection file" <> metavar "OutlineCbor")
    <*> argument str (help "paragraph collection file" <> metavar "ParagraphCbor")
    <*> argument str (help "entity collection file" <> metavar "ArticleCbor")
    <*> option str (short 'd' <> long "destdir" <> help "destination directory for generated HTML" <> metavar "DIR")
    <*> switch (short 's' <> long "shuffle results")
    <*> option auto (short 'k' <> long "top" <> help "top k to take from each ranking" <> metavar "INT" <> value 10)
    <*> optional (option str (short 'O' <> long "outlineid" <> help "id of outline for which HTML should be generated" <> metavar "STR"))
    <*> option str (short 'q' <> long "qrels" <> help "trec compatible qrels file" <> metavar "QRELS")
    <*> many (option str (short 'p' <> long "psg-runs" <> help "trec compatible passage run file(s)" <> metavar "Trec-psg-run-FILE(s)"))
    <*> many (option str (short 'e' <> long "entity-runs" <> help "trec compatible entity run file(s)" <> metavar "Trec-entity-run-FILE(s)"))



main ::  IO ()
main = do
    Opts{..} <- execParser $ info (helper <*> opts) mempty
    trecPsgRunFiles <- concat <$> mapM glob optsTrecPsgRunGlobs
    trecEntityRunFiles <- concat <$> mapM glob optsTrecEntityRunGlobs
    -- ======== basic loading ===========

    -- load trec run files and merge with paragraph (in a lazy hashmap)
    putStrLn "deserializing paragraphs.."
    paragraphIndex <- TocFile.open $ TocFile.IndexedCborPath paragraphFile
    putStrLn "...done deserializing paragraphs"

    let loadParagraph :: ParagraphId -> Paragraph
        loadParagraph pid =
          fromMaybe (error $ "Can't find paragraph: "++ show pid ++ " in file "++ paragraphFile)
           $ loadParagraphMaybe pid

        loadParagraphMaybe :: ParagraphId -> Maybe Paragraph
        loadParagraphMaybe pid =
         TocFile.lookup pid paragraphIndex


    putStrLn "deserializing articles.."
    entityIndex <- TocFile.open (TocFile.IndexedCborPath entityFile :: TocFile.IndexedCborPath PageId Page)
    putStrLn "...done deserializing articles"

    let loadEntity :: PageId -> Entity
        loadEntity pid =
          fromMaybe (error $ "Can't find entity: "++ show pid ++ " in file "++ entityFile)
           $ loadEntityMaybe pid

        loadEntityMaybe :: PageId -> Maybe Entity
        loadEntityMaybe pid = do
            fmap toEntity $ TocFile.lookup pid entityIndex
          where toEntity page = Entity (pageName page) (pageId page)

    -- ========= view renderer Paragraphs ==============
    trecResultMap <-
        let trecRunItemToEntryItemPara ::  TrecRun.DocumentName -> Paragraph
            trecRunItemToEntryItemPara = loadParagraph . packParagraphId . T.unpack

            getNubKeyPara ::  RankingEntry Paragraph-> ParagraphId
            getNubKeyPara = paraId . entryItem
        in trecResultUnionOfRankedItems trecRunItemToEntryItemPara getNubKeyPara optsTopK optsShuffle trecPsgRunFiles
    trecQrelsMap <-
        let trecRunItemToEntryItemMaybePara = loadParagraphMaybe . packParagraphId . T.unpack
        in trecQrelItems  trecRunItemToEntryItemMaybePara optsQrelFile


    let lookupResult :: SectionPath -> Maybe [TrecCarRenderHtml.PassageRankingEntry]
        lookupResult sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecResultMap

    let lookupTruth :: SectionPath -> Maybe [TrecCarRenderHtml.PassageRankingEntry]
        lookupTruth sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecQrelsMap


    -- ========= view renderer Entity ==============

--

    trecResultMapEntity <-
        let trecRunItemToEntryItemEntity :: TrecRun.DocumentName -> Entity
            trecRunItemToEntryItemEntity = loadEntity . packPageId . T.unpack

            getNubKeyEntity :: EntityRankingEntry -> PageId
            getNubKeyEntity = entityPageId . entryItem

        in trecResultUnionOfRankedItems trecRunItemToEntryItemEntity getNubKeyEntity optsTopK optsShuffle trecEntityRunFiles
    trecQrelsMapEntity <-
        let trecRunItemToEntryItemMaybeEntity :: TrecQrel.DocumentName -> Maybe Entity
            trecRunItemToEntryItemMaybeEntity = loadEntityMaybe . packPageId .  T.unpack
        in trecQrelItems  trecRunItemToEntryItemMaybeEntity optsQrelFile


    putStrLn $ "trecResultMapEntity = " <> show trecResultMapEntity

    let lookupResultEntity :: SectionPath -> Maybe [TrecCarRenderHtml.EntityRankingEntry]
        lookupResultEntity sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecResultMapEntity

    let lookupTruthEntity :: SectionPath -> Maybe [TrecCarRenderHtml.EntityRankingEntry]
        lookupTruthEntity sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecQrelsMapEntity



    -- ========================================================

    let fileNameLookup = fileNameLookupFactory (isJust . lookupResult) (isJust . lookupResultEntity)


    let wrapDestDir :: FilePath -> IO FilePath
        wrapDestDir filePath = do
         let filePath' = optsDest </> filePath
         createDirectoryIfMissing True (takeDirectory filePath')
         pure filePath'


    -- ======= Main bits ================

    putStrLn "deserializing outlines.."
    outlinesAll <- decodeCborList <$> BSL.readFile outlinesFile
        :: IO [Stub]
    putStrLn ".. done deserializing outlines"

    let outlines = case optsOutlineId of
                     Just outlineId -> filter (\out -> (unpackPageId $ stubPageId out) == outlineId ) outlinesAll
                     Nothing -> outlinesAll

    let createPassageView :: FileNameLookup -> TrecCarRenderHtml.SectionPathWithName -> IO ()
        createPassageView FileNameLookup{..} sectionPathWithNames = do
           let sectionPath = sprQueryId $ sectionPathWithNames
               sectionResults = lookupResult sectionPath
               sectionTruthsMaybe = lookupTruth sectionPath    -- todo entity Lookups
               maybeFilePath = passageViewPathname sectionPath
           case (sectionResults, maybeFilePath) of
             (Just rankingEntries, Just filePath) -> do
             -- todo PassageViewHtml --> EntityViewHtml
                 let pageHtml = (trace $ "filePath"<> filePath) PassageViewHtml.passageRankingToHtml sectionPathWithNames rankingEntries sectionTruthsMaybe
                 passageFile <- wrapDestDir filePath
                 BSL.writeFile passageFile $ H.renderHtml pageHtml
             (Just rankingEntries, Nothing) -> do
                 error $ "Got rankEntries but Nothing as filepath. SectionPath = "<> show sectionPath
             _  -> do
                 putStrLn $ "no results for section path "++show sectionPath

    let createEntityView :: FileNameLookup -> TrecCarRenderHtml.SectionPathWithName -> IO ()
        createEntityView FileNameLookup{..} sectionPathWithNames = do
           let sectionPath = sprQueryId $ sectionPathWithNames
               sectionResults = lookupResultEntity sectionPath
               sectionTruthsMaybe = lookupTruthEntity sectionPath
               maybeFilePath = entityViewPathname sectionPath
           case (sectionResults, maybeFilePath) of
             (Just rankingEntries, Just filePath) -> do
                 let pageHtml = (trace $ "filePath"<> filePath) EntityViewHtml.entityRankingToHtml sectionPathWithNames rankingEntries sectionTruthsMaybe
                 passageFile <- wrapDestDir filePath
                 BSL.writeFile passageFile $ H.renderHtml pageHtml
             (Just rankingEntries, Nothing) -> do
                 error $ "Got rankEntries but Nothing as filepath. SectionPath = "<> show sectionPath
             _  -> do
                 putStrLn $ "no results for section path "++show sectionPath

    let outlineToFiles fileNameLookup@FileNameLookup{..} outline = do
            outlineFile <- wrapDestDir $ outlinePathname outline
            let pageHtml = OutlineViewHtml.outlineToHtml fileNameLookup outline
            BSL.writeFile outlineFile $ H.renderHtml pageHtml
            return outlineFile


    passageFiles <- mapM (outlineToFiles fileNameLookup) outlines
      :: IO [FilePath]
    forM_ outlines $ \outline -> do
        let sectionPathWithNamess = pageSkeletonToSectionPathsWithName outline
        forM_ sectionPathWithNamess (\spwn ->
                                    createPassageView fileNameLookup spwn >>
                                    createEntityView fileNameLookup spwn
                                    )

    -- ======== get sectionpaths out of a stub ===============


pageSkeletonToSectionPathsWithName :: Stub -> [TrecCarRenderHtml.SectionPathWithName]
pageSkeletonToSectionPathsWithName Stub{..}  = empty : foldMap (go empty) stubSkeleton
    where
      empty = TrecCarRenderHtml.SectionPathWithName
                  { sprQueryId     = SectionPath{sectionPathPageId=stubPageId, sectionPathHeadings=mempty}
                  , sprPageName    = stubName
                  , sprHeadingPath = mempty
                  }

      append :: TrecCarRenderHtml.SectionPathWithName -> HeadingId -> SectionHeading -> TrecCarRenderHtml.SectionPathWithName
      append spn headingId sectionHeading =
           spn
              { sprQueryId=(sprQueryId spn) {sectionPathHeadings = sectionPathHeadings (sprQueryId spn) ++ [headingId] }
              , sprHeadingPath = sprHeadingPath spn ++ [sectionHeading]
              }

      go :: TrecCarRenderHtml.SectionPathWithName -> PageSkeleton -> [TrecCarRenderHtml.SectionPathWithName]
      go sectionPathWithName (Section sectionHeading sectionId children) =
          sectionPathWithName' : foldMap (go sectionPathWithName') children
        where
          sectionPathWithName' = append sectionPathWithName sectionId sectionHeading
      go sectionPathWithName (Para _) = []
      go sectionPathWithName (Image _ _) = []

