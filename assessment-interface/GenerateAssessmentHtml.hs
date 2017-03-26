{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Monoid
import Data.Traversable
import Data.List
import Data.Maybe
import Data.Foldable
import System.FilePath
import System.Directory

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
import qualified SimplIR.Format.TrecRunFile as Run

import System.Environment

main ::  IO ()
main = do
    let dest = "annotations-html"
    outlinesFile : paragraphFile : trecRunFile : _ <- getArgs

    -- load trec run files and merge with paragraph (in a lazy hashmap)
    paragraphIndex <- TocFile.open $ TocFile.IndexedCborPath paragraphFile

    let loadParagraph :: ParagraphId -> Paragraph
        loadParagraph pid =
          fromMaybe (error $ "Can't find paragraph: "++ show pid ++ " in file "++ paragraphFile)
           $ TocFile.lookup pid paragraphIndex

    trecRankingContents <- Run.readRunFile trecRunFile
    let resultMap :: HM.Lazy.HashMap Run.QueryId [PassageViewHtml.RankingEntry]
        resultMap = HM.fromListWith (++) $
          [ ( Run.queryId entry
            , [RankingEntry { entryParagraph = loadParagraph $ packParagraphId $ T.unpack $ Run.documentName entry
                            , entryScore = Run.documentScore entry
                            }]
            )
          | entry <- trecRankingContents
          ]

        lookupResult :: SectionPath -> Maybe [PassageViewHtml.RankingEntry]
        lookupResult sectionPath =
            let queryId = T.pack $ escapeSectionPath sectionPath
            in queryId  `HM.lookup` resultMap


     -- done, only need resultMap after this point

    let toFilename :: SectionPath -> IO FilePath
        toFilename (SectionPath page headings) = do
            let dirPath = dest </> (unpackPageId page)
            createDirectoryIfMissing True dirPath
            return $ dirPath </> sectionfilename <.> "html"
          where
            sectionfilename = case headings of
                                [] -> "index-article"
                                _ ->  (intercalate "-" $ map unpackHeadingId headings)



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

pageSkeletonToSectionPathsWithName :: Stub -> [PassageViewHtml.SectionPathWithName]
pageSkeletonToSectionPathsWithName Stub{..} = foldMap (go empty) stubSkeleton
    where
      empty = PassageViewHtml.SectionPathWithName
                  { sprQueryId     = SectionPath{sectionPathPageId=stubPageId, sectionPathHeadings=mempty}
                  , sprPageName    = stubName
                  , sprHeadingPath = mempty
                  }

      append :: PassageViewHtml.SectionPathWithName -> HeadingId -> SectionHeading -> PassageViewHtml.SectionPathWithName
      append spn headingId sectionHeading =
           spn
              { sprQueryId=(sprQueryId spn) {sectionPathHeadings = sectionPathHeadings (sprQueryId spn) ++ [headingId] }
              , sprHeadingPath = sprHeadingPath spn ++ [sectionHeading]
              }

      go :: PassageViewHtml.SectionPathWithName -> PageSkeleton -> [PassageViewHtml.SectionPathWithName]
      go sectionPathWithName (Section sectionHeading sectionId children) =
          sectionPathWithName' : foldMap (go sectionPathWithName') children
        where
          sectionPathWithName' = append sectionPathWithName sectionId sectionHeading
      go sectionPathWithName (Para _) = []