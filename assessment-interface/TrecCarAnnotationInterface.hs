{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


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
import qualified SimplIR.Format.TrecRunFile as TrecRun

import TrecCarRenderHtml

import System.Environment

data Opts = Opts { outlinesFile :: FilePath
                 , paragraphFile :: FilePath
                 , optsDest :: FilePath
                 , optsShuffle :: Bool
                 , optsTopK :: Int
                 , optsTrecRunFiles :: [FilePath]
                  }



trecResultUnionOfRankedParagraphs :: (ParagraphId -> Paragraph) -> Int -> Bool -> [FilePath]
                                  -> IO (HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry])
trecResultUnionOfRankedParagraphs loadParagraph optsTopK optsShuffle trecRunFiles = do

    let --resultsToTopkMap :: _ -> (HM.Lazy.HashMap TrecRun.QueryId [TrecCarRenderHtml.RankingEntry])
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
    <*> some (argument str (help "trec compatible run file(s)" <> metavar "FILE"))

main ::  IO ()
main = do
    Opts{..} <- execParser $ info (helper <*> opts) mempty

    let trecRunFile : _ = optsTrecRunFiles

    -- ======== basic loading ===========

    -- load trec run files and merge with paragraph (in a lazy hashmap)
    paragraphIndex <- TocFile.open $ TocFile.IndexedCborPath paragraphFile

    let loadParagraph :: ParagraphId -> Paragraph
        loadParagraph pid =
          fromMaybe (error $ "Can't find paragraph: "++ show pid ++ " in file "++ paragraphFile)
           $ TocFile.lookup pid paragraphIndex




    -- ========== low-level renderer ============




    -- ========= view renderer ==============

    trecResultMap <- trecResultUnionOfRankedParagraphs loadParagraph optsTopK optsShuffle optsTrecRunFiles

    let lookupResult :: SectionPath -> Maybe [TrecCarRenderHtml.RankingEntry]
        lookupResult sectionPath =
          let queryId = T.pack $ escapeSectionPath sectionPath
          in queryId  `HM.lookup` trecResultMap

    -- =======================

     -- done, only need resultMap after this point

    let toFilename :: SectionPath -> IO FilePath
        toFilename (SectionPath page headings) = do
            let dirPath = optsDest </> (unpackPageId page)
            createDirectoryIfMissing True dirPath
            return $ dirPath </> sectionfilename <.> "html"
          where
            sectionfilename =
              case headings of
                [] -> "index-article"
                _ ->  intercalate "-" $ map (map replaceChars . unpackHeadingId) headings
              where
                replaceChars '/' = '-'
                replaceChars c   = c


    outlines <- decodeCborList <$> BSL.readFile outlinesFile
        :: IO [Stub]

    files <- forM outlines $ \outline -> do
         let sectionPathWithNamess = pageSkeletonToSectionPathsWithName outline
         fmap catMaybes $ forM sectionPathWithNamess $ \sectionPathWithNames -> do
           let sectionPath = sprQueryId $ sectionPathWithNames
           case lookupResult sectionPath of
             Nothing -> do
                 putStrLn $ "no results for section path "++show sectionPath
                 return Nothing

             Just rankingEntries -> do
                -- createFilenameForSectionPath
                 let pageHtml =PassageViewHtml.passageRankingToHtml sectionPathWithNames rankingEntries

                 outFile <- toFilename sectionPath

        -- for every section create a file as well

                 BSL.writeFile outFile $ H.renderHtml pageHtml
                 return $ Just outFile
      :: IO [FilePath]

    print $ fold files

    return ()
--     let queryListHtml = undefined
--     BSL.writeFile (dest </> "index.html") queryListHtml


    -- makeAnnotationInterface "." queries

-- writeQuery :: FilePath -> Query -> IO ()
-- writeQuery dest query = do
--     BSL.writeFile (dest </> unpackHeadingId (queryId query) </> "index.html")
--         $ H.renderHtml $ queryToHtml query

-- pageSkeletonSections :: Stub -> [PassageViewHtml.SectionPathWithName]
-- pageSkeletonSections Stub{..} = foldMap (go []) stubSkeleton
--   where
--     go :: PassageViewHtml.SectionPathWithName -> Section -> [PassageViewHtml.SectionPathWithName]
--     go accum (Section sectionId sectionHeading children) =
--         let sectionPath = SectionPath stubPageId (accum')
--             sectionPathWithName = PassageViewHtml.SectionPathWithName sectionPath sectionHeadings stubName
--         in sectionPathWithName  : foldMap (go accum') children
--       where accum' = accum ++ [sectionId]
--     go accum (Para _) = []

pageSkeletonToSectionPathsWithName :: Stub -> [TrecCarRenderHtml.SectionPathWithName]
pageSkeletonToSectionPathsWithName Stub{..} = foldMap (go empty) stubSkeleton
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