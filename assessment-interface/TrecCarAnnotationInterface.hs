{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | TrecCarAnnotationInterface: Commandline argument reading, trec run loading, merging, shuffling, nubbing
module Main where

import Options.Applicative

import Data.Monoid
import Data.Traversable
import Data.List
import Data.Maybe
import Data.Foldable
import System.FilePath
import System.Directory

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
import OutlineViewHtml
import qualified SimplIR.Format.TrecRunFile as TrecRun

import TrecCarRenderHtml
import FileNameLookup

import Debug.Trace

data Opts = Opts { outlinesFile :: FilePath
                 , paragraphFile :: FilePath
                 , optsDest :: FilePath
                 , optsShuffle :: Bool
                 , optsTopK :: Int
                 , optsOutlineId :: Maybe String
                 , optsTrecRunFiles :: [FilePath]
                 }



trecResultUnionOfRankedParagraphs :: (ParagraphId -> Paragraph) -> Int -> Bool -> [FilePath]
                                  -> IO (HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry])
trecResultUnionOfRankedParagraphs loadParagraph optsTopK optsShuffle trecRunFiles = do

    let resultsToTopkMap ::  [TrecRun.RankingEntry] -> HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry]
        resultsToTopkMap trecRankingContents =
            let resultMap :: HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry]
                resultMap = HM.fromListWith (++) $
                  [ ( TrecRun.queryId entry
                    , [RankingEntry { entryParagraph = loadParagraph $ packParagraphId $ T.unpack $ TrecRun.documentName entry
                                    , entryScore = TrecRun.documentScore entry
                                    }]
                    )
                  | entry <- trecRankingContents
                  ]
            in fmap (take optsTopK) resultMap

    files <- mapM TrecRun.readRunFile trecRunFiles
    let unionResultMap = evalRandIO $ mapM condShuffleStuffNub $ foldl' (HM.unionWith (++)) mempty $ fmap resultsToTopkMap files
          where
            condShuffleStuffNub :: [TrecCarRenderHtml.RankingEntry] -> Rand StdGen [TrecCarRenderHtml.RankingEntry]
            condShuffleStuffNub rankElements
              | optsShuffle = shuffleM $ nubByParaId rankElements
              | otherwise   = return $ nubByParaId rankElements
            nubByParaId = HM.elems . HM.fromList . fmap (\rankElem -> (paraId $ entryParagraph $ rankElem, rankElem))
    unionResultMap



opts :: Parser Opts
opts =
    Opts
    <$> argument str (help "outline collection file" <> metavar "CBOR FILE")
    <*> argument str (help "paragraph collection file" <> metavar "CBOR FILE")
    <*> option str (short 'd' <> long "destdir" <> help "destination directory for generated HTML" <> metavar "DIR")
    <*> switch (short 's' <> long "shuffle results")
    <*> option auto (short 'k' <> long "top" <> help "top k to take from each ranking" <> metavar "INT" <> value 10)
    <*> optional (option str (short 'O' <> long "outlineid" <> help "id of outline for which HTML should be generated" <> metavar "STR"))
    <*> some (argument str (help "trec compatible run file(s)" <> metavar "FILE"))



main ::  IO ()
main = do
    Opts{..} <- execParser $ info (helper <*> opts) mempty
    -- ======== basic loading ===========

    -- load trec run files and merge with paragraph (in a lazy hashmap)
    paragraphIndex <- TocFile.open $ TocFile.IndexedCborPath paragraphFile

    let loadParagraph :: ParagraphId -> Paragraph
        loadParagraph pid =
          fromMaybe (error $ "Can't find paragraph: "++ show pid ++ " in file "++ paragraphFile)
           $ TocFile.lookup pid paragraphIndex

    -- ========= view renderer ==============

    trecResultMap <- trecResultUnionOfRankedParagraphs loadParagraph optsTopK optsShuffle optsTrecRunFiles

    let lookupResult :: SectionPath -> Maybe [TrecCarRenderHtml.RankingEntry]
        lookupResult sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecResultMap

    let fileNameLookup = fileNameLookupFactory existResultsForSectionpath
         where
           existResultsForSectionpath :: SectionPath -> Bool
           existResultsForSectionpath =
              isJust . lookupResult

    let wrapDestDir :: FilePath -> IO FilePath
        wrapDestDir filePath = do
         let filePath' = optsDest </> filePath
         createDirectoryIfMissing True (takeDirectory filePath')
         pure filePath'


    -- ======= Main bits ================

    outlinesAll <- decodeCborList <$> BSL.readFile outlinesFile
        :: IO [Stub]

    let outlines = case optsOutlineId of
                     Just outlineId -> filter (\out -> (unpackPageId $ stubPageId out) == outlineId ) outlinesAll
                     Nothing -> outlinesAll

    let createPassageView :: FileNameLookup -> TrecCarRenderHtml.SectionPathWithName -> IO ()
        createPassageView FileNameLookup{..} sectionPathWithNames = do
           let sectionPath = sprQueryId $ sectionPathWithNames
               sectionResults = lookupResult sectionPath
               maybeFilePath = passageViewPathname sectionPath
           case (sectionResults, maybeFilePath) of
             (Just rankingEntries, Just filePath) -> do
                 let pageHtml = (trace $ "filePath"<> filePath) PassageViewHtml.passageRankingToHtml sectionPathWithNames rankingEntries
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
        forM_ sectionPathWithNamess (createPassageView fileNameLookup)

    -- ======== get sectionpaths out of a stub ===============


pageSkeletonToSectionPathsWithName :: Stub -> [TrecCarRenderHtml.SectionPathWithName]
pageSkeletonToSectionPathsWithName Stub{..}  = foldMap (go empty) stubSkeleton
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
