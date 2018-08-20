{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | TrecCarAnnotationInterface: Commandline argument reading, trec run loading, merging, shuffling, nubbing
module Main where

import Options.Applicative

import Data.Bifunctor
import Data.Monoid
import Data.Maybe
import Data.Ord
import Data.List (sortBy)
import Data.Foldable
import Data.Hashable
import GHC.Generics
import System.FilePath
import System.Directory
import System.FilePath.Glob

import System.Random
import System.Random.Shuffle
import Control.Monad.Random

import qualified Data.Aeson as Aeson
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HM.Lazy
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Text.Blaze.Html.Renderer.Utf8 as H

import CAR.Types
import qualified CAR.RunFile as CarRun
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
                 , optsTopK :: Maybe Int
                 , optsTopKPerRun :: Maybe Int
                 , optsOutlineId :: Maybe String
                 , optsInterface :: Maybe FilePath
                 , optsTrueQrelFiles :: [FilePath]
                 , optsQrelFile :: FilePath
                 , optsTrecPsgRunGlobs :: [FilePath]
                 , optsTrecEntityRunGlobs :: [FilePath]
                 }




-- todo generify  trecQrelParagraphs
trecQrelItems :: forall item. (TrecRun.DocumentName -> Maybe item) -> FilePath
              -> IO (HM.Lazy.HashMap TrecQrel.QueryId [RankingEntry item] )
trecQrelItems trecRunItemToEntryItemMaybe qrelfile  = do
    qrelEntries <- TrecQrel.readQRel qrelfile
    let qrelMap =
            fmap DList.toList $
            HM.fromListWith (<>)
            [ ( TrecQrel.queryId entry
              , DList.singleton
                $ QrelEntry { entryItem = item
                            , entryLabel = fromBinaryRelevance $ TrecQrel.relevance entry
                            }
              )
            | entry <- qrelEntries
            , TrecQrel.relevance entry /= TrecQrel.NotRelevant
            , Just item <- pure $ trecRunItemToEntryItemMaybe $ TrecQrel.documentName entry
            ]
    return qrelMap


readTrecRanking :: Show item => (TrecRun.DocumentName -> Maybe item)
                -> FilePath
                -> IO (HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry item])
readTrecRanking trecRunItemToEntryItem path = do
    fmap sortIt . toMap <$> TrecRun.readRunFile path
  where
    sortIt = sortBy (flip $ comparing entryScore)      -- todo use Ranking
    toMap contents =
        fmap DList.toList $
        HM.fromListWith (<>)
        [ ( TrecRun.queryId entry
          , DList.singleton
            $ RankingEntry { entryItem = item
                           , entryScore = TrecRun.documentScore entry
                           , entryMethodNames = [TrecRun.methodName entry]
                           }
          )
        | entry <- contents
        , Just item <- pure $ trecRunItemToEntryItem $ TrecRun.documentName entry -- $ (\x -> traceShow (x, entry) x)
        ]

trecResultUnionOfRankedItems :: forall item nubKey. (Eq nubKey, Hashable nubKey)
                             => (RankingEntry item -> nubKey)
                             -> (RankingEntry item -> RankingEntry item -> RankingEntry item)
                             -> Int
                             -> [HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry item]]
                             -> HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry item]
trecResultUnionOfRankedItems getNubKey mergeNubbedElems optsTopK rankings =
    fmap nubIt
    $ foldl' (HM.unionWith (++)) mempty
    $ fmap (fmap $ take topKPerFile)
      rankings
  where
    topKPerFile = ceiling ((realToFrac optsTopK) / (realToFrac $ length rankings))
    nubIt = HM.elems . HM.fromListWith mergeNubbedElems . fmap (\rankElem -> (getNubKey rankElem, rankElem))
--     nubIt = HM.elems . HM.fromList . fmap (\rankElem -> (getNubKey rankElem, rankElem))

shuffleRankings :: Traversable t
                => t [TrecCarRenderHtml.RankingEntry item]
                -> IO (t [TrecCarRenderHtml.RankingEntry item])
shuffleRankings = evalRandIO . traverse shuffleM

-- todo urgent mix ground truth and result files


opts :: Parser Opts
opts =
    Opts
    <$> argument str (help "outline collection file" <> metavar "OutlineCbor")
    <*> argument str (help "paragraph collection file" <> metavar "ParagraphCbor")
    <*> argument str (help "entity collection file" <> metavar "ArticleCbor")
    <*> option str (short 'd' <> long "destdir" <> help "destination directory for generated HTML" <> metavar "DIR")
    <*> switch (short 's' <> long "shuffle results")
    <*> optional (option auto (short 'k' <> long "top" <> help "max k assessments per ranking" <> metavar "INT" ))
    <*> optional (option auto (short 'K' <> long "topPerRun" <> help "top k to take from each ranking" <> metavar "INT" ))
    <*> optional (option str (short 'O' <> long "outlineid" <> help "id of outline for which HTML should be generated" <> metavar "STR"))
    <*> optional (option str (short 'i' <> long "interface" <> help "regenerate old interface"))
    <*> many (option str (short 'Q' <> long "show-qrel" <> help "qrel file to show annotations from"))
    <*> option str (short 'q' <> long "qrels" <> help "trec compatible qrels file" <> metavar "QRELS")
    <*> many (option str (short 'p' <> long "psg-runs" <> help "trec compatible passage run file(s)" <> metavar "Trec-psg-run-FILE(s)"))
    <*> many (option str (short 'e' <> long "entity-runs" <> help "trec compatible entity run file(s)" <> metavar "Trec-entity-run-FILE(s)"))



main ::  IO ()
main = do
    Opts{..} <- execParser $ info (helper <*> opts) mempty
    trecPsgRunFiles <- concat <$> mapM glob optsTrecPsgRunGlobs
    trecEntityRunFiles <- concat <$> mapM glob optsTrecEntityRunGlobs
    let topKPara :: Int
        topKEntity :: Int
        (topKPara, topKEntity) = case (optsTopK, optsTopKPerRun) of
                                    (Just k, _) -> (k, k)
                                    (Nothing, Just k) -> ( k * (length $ trecPsgRunFiles)
                                                         , k * (length $ trecEntityRunFiles))
                                    _ -> (10, 10)

    -- ======== basic loading ===========
    interface <- flip traverse optsInterface $ \fname -> do
        Right i <- Aeson.eitherDecode <$> BSL.readFile fname
        return i
      :: IO (Maybe AnnotationInterface)

    -- load trec run files and merge with paragraph (in a lazy hashmap)
    putStrLn "deserializing paragraphs.."
    paragraphIndex <- TocFile.open $ TocFile.IndexedCborPath paragraphFile
    putStrLn "...done deserializing paragraphs"

    let loadParagraphMaybe :: ParagraphId -> Maybe Paragraph
        loadParagraphMaybe pid =
         TocFile.lookup pid paragraphIndex


    putStrLn "deserializing articles.."
    entityIndex <- TocFile.open (TocFile.IndexedCborPath entityFile :: TocFile.IndexedCborPath PageId Page)
    putStrLn "...done deserializing articles"

    let loadEntityMaybe :: PageId -> Maybe Entity
        loadEntityMaybe pid =
            fmap toEntity $ TocFile.lookup pid entityIndex
          where toEntity page = Entity (pageName page) (pageId page)

    -- ========= view renderer Paragraphs ==============
    trecResultMap <-
        let trecRunItemToEntryItemPara ::  TrecRun.DocumentName -> Maybe Paragraph
            trecRunItemToEntryItemPara = loadParagraphMaybe . packParagraphId . T.unpack

            getNubKeyPara ::  RankingEntry Paragraph-> ParagraphId
            getNubKeyPara = paraId . entryItem
            mergeNubbedPara :: RankingEntry Paragraph -> RankingEntry Paragraph -> RankingEntry Paragraph
            mergeNubbedPara entry1 entry2 =
                entry1 {entryMethodNames = entryMethodNames entry1 ++ entryMethodNames entry2 }
        in trecResultUnionOfRankedItems getNubKeyPara mergeNubbedPara topKPara
           <$> mapM (readTrecRanking trecRunItemToEntryItemPara) trecPsgRunFiles
    trecQrelsMap <-
        let trecRunItemToEntryItemMaybePara = loadParagraphMaybe . packParagraphId . T.unpack
        in trecQrelItems trecRunItemToEntryItemMaybePara optsQrelFile

    -- print trecResultMap

    let lookupResult :: SectionPath -> Maybe [TrecCarRenderHtml.PassageRankingEntry]
        lookupResult sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
              ranking = queryId  `HM.lookup` trecResultMap
          in ranking

    let lookupTruth :: SectionPath -> Maybe [TrecCarRenderHtml.PassageRankingEntry]
        lookupTruth sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecQrelsMap


    -- ========= view renderer Entity ==============
    trecResultMapEntity <-
        let trecRunItemToEntryItemEntity :: TrecRun.DocumentName -> Maybe (Entity, Paragraph)
            trecRunItemToEntryItemEntity docName =
                let CarRun.EntityAndPassage eid pid = CarRun.parsePassageEntity docName -- loadEntity . packPageId . T.unpack
                in case (loadEntityMaybe eid,loadParagraphMaybe pid) of
                   (Just e, Just p) -> Just (e, p)
--                    (Nothing, Just p) -> trace ("trecRunItemToEntryItemEntity can't load entity "<> show eid) $ Nothing
                   _ -> Nothing
--                 in do entity <- loadEntityMaybe eid
--                       para <- loadParagraphMaybe pid
--                       return (entity, para)

            getNubKeyEntity :: EntityParagraphRankingEntry -> (PageId, ParagraphId)
            getNubKeyEntity rankingEntry =
                let (entity, paragraph) = entryItem  rankingEntry
                in (entityPageId entity, paraId paragraph)
            mergeNubbedEntity :: EntityParagraphRankingEntry -> EntityParagraphRankingEntry -> EntityParagraphRankingEntry
            mergeNubbedEntity entry1 entry2 =
                entry1 {entryMethodNames = entryMethodNames entry1 ++ entryMethodNames entry2 }

        in trecResultUnionOfRankedItems getNubKeyEntity mergeNubbedEntity topKEntity
           <$> mapM (readTrecRanking trecRunItemToEntryItemEntity) trecEntityRunFiles
      :: IO (HM.Lazy.HashMap TrecRun.QueryId [RankingEntry (Entity, Paragraph)])

    let trecQrelsMapEntity = mempty
--     putStrLn $ "trecResultMapEntity = " <> show trecResultMapEntity

    let lookupResultEntity :: SectionPath -> Maybe [TrecCarRenderHtml.EntityParagraphRankingEntry]
        lookupResultEntity sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecResultMapEntity

    let lookupTruthEntity :: SectionPath -> Maybe [TrecCarRenderHtml.EntityParagraphRankingEntry]
        lookupTruthEntity sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecQrelsMapEntity


    -- ======= File names =====================================
    let fileNameLookup = fileNameLookupFactory (isJust . lookupResult) (isJust . lookupResultEntity)

    let wrapDestDir :: FilePath -> IO FilePath
        wrapDestDir filePath = do
         let filePath' = optsDest </> filePath
         createDirectoryIfMissing True (takeDirectory filePath')
         pure filePath'


    -- ======= Main bits ================

    putStrLn "deserializing outlines.."
    outlinesAll <- readOutlinesFile outlinesFile
    putStrLn ".. done deserializing outlines"

    let outlines = case optsOutlineId of
                     Just outlineId -> filter (\out -> (unpackPageId $ stubPageId out) == outlineId ) outlinesAll
                     Nothing -> outlinesAll

    let createPassageView :: FileNameLookup -> TrecCarRenderHtml.SectionPathWithName -> IO ()
        createPassageView FileNameLookup{..} sectionPathWithNames = do
           let sectionPath = sprQueryId sectionPathWithNames
               sectionResults = lookupResult sectionPath
               sectionTruthsMaybe = lookupTruth sectionPath    -- todo entity Lookups
               maybeFilePath = passageViewPathname sectionPath
           case (sectionResults, maybeFilePath) of
             (Just rankingEntries, Just filePath) -> do
             -- todo PassageViewHtml --> EntityViewHtml
                 let pageHtml = PassageViewHtml.passageMixedRankingToHtml sectionPathWithNames rankingEntries sectionTruthsMaybe
                 passageFile <- wrapDestDir filePath
                 BSL.writeFile passageFile $ H.renderHtml pageHtml
             (Just _rankingEntries, Nothing) ->
                 fail $ "Got passage rankEntries but Nothing as filepath. SectionPath = "<> show sectionPath
             _  ->
                 putStrLn $ "no passage results for section path "++show sectionPath

    let createEntityView :: FileNameLookup -> TrecCarRenderHtml.SectionPathWithName -> IO ()
        createEntityView FileNameLookup{..} sectionPathWithNames = do
           let sectionPath = sprQueryId sectionPathWithNames
               sectionResults = lookupResultEntity sectionPath
               sectionTruthsMaybe = lookupTruthEntity sectionPath
               maybeFilePath = entityViewPathname sectionPath
           case (sectionResults, maybeFilePath) of
             (Just rankingEntries, Just filePath) -> do
                 let pageHtml = EntityViewHtml.entityPassageRankingToHtml sectionPathWithNames rankingEntries sectionTruthsMaybe
                 passageFile <- wrapDestDir filePath
                 BSL.writeFile passageFile $ H.renderHtml pageHtml
             (Just _rankingEntries, Nothing) ->
                 fail $ "Got entity rankEntries but Nothing as filepath. SectionPath = "<> show sectionPath
             _  ->
                 putStrLn $ "no entity results for section path "++show sectionPath

    let outlineToFiles FileNameLookup{..} outline = do
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


    BSL.writeFile "interface.json" $ Aeson.encode
        $ AnnotationInterface { entityResults = fmap (map $ convertE) trecResultMapEntity
                              , entityQRels   = fmap (map $ convertE') trecQrelsMapEntity
                              , passageResults = fmap (map $ convertP) trecResultMap
                              , passageQRels   = fmap (map $ convertP') trecQrelsMap
                              , runEntityStats = fmap convert trecResultMapEntity
                              , runPassageStats = fmap convert trecResultMap
                              }
          where convertE :: RankingEntry (Entity, Paragraph) -> AnnotationRecordEntityPara
                convertE entry =
                   AnnotationRecordEntityPara { annotationEntityId = entityPageId $ fst $ entryItem entry
                                              , annotationEntityName = entityPageName $ fst $ entryItem entry
                                              , annnotationSupportParaId = paraId $ snd $ entryItem entry
                                              , annotationEntityMethodNames = entryMethodNames entry
                                              }
                convertE' :: RankingEntry (Entity, Paragraph) -> AnnotationRecordEntityPara
                convertE' entry =
                   AnnotationRecordEntityPara { annotationEntityId = entityPageId $ fst $ entryItem entry
                                              , annotationEntityName = entityPageName $ fst $ entryItem entry
                                              , annnotationSupportParaId = paraId $ snd $ entryItem entry
                                              , annotationEntityMethodNames = ["qrels"]
                                              }

                convertP :: RankingEntry Paragraph -> AnnotationRecordParagraph
                convertP entry =

                   AnnotationRecordParagraph { annotationParaId = paraId $ entryItem entry
                                             , annotationParaMethodNames = entryMethodNames entry
                                             }
                convertP' :: RankingEntry Paragraph -> AnnotationRecordParagraph
                convertP' entry =
                   AnnotationRecordParagraph { annotationParaId = paraId $ entryItem entry
                                        , annotationParaMethodNames = ["qrels"]
                                        }

                convert :: [RankingEntry x] -> [AnnotationRunStats]
                convert entries =
                     fmap conv
                     $ HM.toList
                     $ HM.fromListWith (+)
                     $ [ (method, 1) | entry <- entries, method <- entryMethodNames entry]
                       where conv (method, count) =
                                AnnotationRunStats { annotationStatsMethodName = method
                                                   , annotationStatsCount = count
                                                   }

--     BSL.writeFile "interface.json" $ Aeson.encode
--         $ AnnotationInterface { entityResults = fmap (map $ second paraId . entryItem) trecResultMapEntity
--                               , entityQRels   = fmap (map $ second paraId . entryItem) trecQrelsMapEntity
--                               , passageResults = fmap (map $ paraId . entryItem) trecResultMap
--                               , passageQRels   = fmap (map $ paraId . entryItem) trecQrelsMap
--                               }

data AnnotationRecordEntityPara =
    AnnotationRecordEntityPara { annotationEntityId :: PageId
                               , annotationEntityName :: PageName
                               , annnotationSupportParaId :: ParagraphId
                               , annotationEntityMethodNames :: [T.Text]
                               }
    deriving (Generic)
data AnnotationRecordParagraph =
    AnnotationRecordParagraph { annotationParaId :: ParagraphId
                              , annotationParaMethodNames :: [T.Text]}
    deriving (Generic)
data AnnotationRunStats =
    AnnotationRunStats { annotationStatsMethodName :: T.Text, annotationStatsCount :: Int}
    deriving (Generic)
data AnnotationInterface =
    AnnotationInterface { entityResults  :: HM.Lazy.HashMap TrecRun.QueryId [AnnotationRecordEntityPara]
                        , entityQRels    :: HM.Lazy.HashMap TrecRun.QueryId [AnnotationRecordEntityPara]
                        , passageResults :: HM.Lazy.HashMap TrecRun.QueryId [AnnotationRecordParagraph]
                        , passageQRels   :: HM.Lazy.HashMap TrecRun.QueryId [AnnotationRecordParagraph]
                        , runEntityStats :: HM.Lazy.HashMap TrecRun.QueryId [AnnotationRunStats]
                        , runPassageStats :: HM.Lazy.HashMap TrecRun.QueryId [AnnotationRunStats]
                        }
    deriving (Generic)
instance Aeson.FromJSON AnnotationInterface
instance Aeson.ToJSON AnnotationInterface
instance Aeson.FromJSON AnnotationRecordEntityPara
instance Aeson.ToJSON AnnotationRecordEntityPara
instance Aeson.FromJSON AnnotationRecordParagraph
instance Aeson.ToJSON AnnotationRecordParagraph
instance Aeson.FromJSON AnnotationRunStats
instance Aeson.ToJSON AnnotationRunStats

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
      go _sectionPathWithName (Para _)     = []
      go _sectionPathWithName (Image _ _)  = []
      go _sectionPathWithName (List {})    = []
      go _sectionPathWithName (Infobox {}) = []

