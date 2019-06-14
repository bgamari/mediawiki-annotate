{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}

module Main where

import Data.Hashable
import Data.Semigroup ((<>))
import Data.Aeson as Aeson
import Data.Foldable
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Options.Applicative
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe



import CAR.Types
import CAR.Types.AST
import TQA
import TqaTopics

data Subset = TrainSubset | TestSubset
  deriving (Eq)
data ProcessContent = OnlyStubContent | OrigContent | NotesContent
  deriving (Eq)

options :: Parser (FilePath, FilePath, FilePath, Subset, ProcessContent)
options =
    (,,,,) <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output CBOR file")
        <*> argument str (metavar "FILE" <> help "Input TqaStatus JSON file")
        <*> argument str (metavar "CBOR FILE" <> help "Input Tqa CBOR file")
        <*> (trainParser <|> testParser)
        <*> (onlyStubContentParser <|> origContentParser <|> keywordContentParser)
  where trainParser = flag' TrainSubset (short 't' <> long "train" <> help "Only export training subset")
        testParser = flag' TestSubset (short 'T' <> long "test" <> help "Only export test subset")
        onlyStubContentParser = flag' OnlyStubContent (short 's' <> long "only-stub-content" <> help "Only export stub content")
        origContentParser = flag' OrigContent (short 'r' <> long "orig-content" <> help "Export all original content")
        keywordContentParser = flag' NotesContent (short 'n' <> long "notes-content" <> help "Export all original content and keywords")


main :: IO ()
main = do
    (outputPath, inputJsonPath, inputCborPath, subset, processContent) <- execParser $ info (helper <*> options) mempty
    status <- either error id . Aeson.eitherDecode <$> BSL.readFile inputJsonPath
              :: IO TqaStatus
    (prov, pages) <- readPagesFileWithProvenance inputCborPath
    let prov' = prov {dataReleaseName = "benchmarkY3" <>
                       (case subset of
                              TestSubset -> "test"
                              TrainSubset -> "train"
                       )
                     }
    let pages' = convertPages processContent status pages
    case processContent of
      OnlyStubContent ->
          writeCarFile outputPath prov' $ fmap toStubSkeleton pages'
      OrigContent ->
          writeCarFile outputPath prov' pages'
      NotesContent ->
          writeCarFile outputPath prov' pages'

convertPages :: ProcessContent -> TqaStatus ->  [Page] -> [Page]
convertPages processContent status@TqaStatus{..}  pages =
    fmap rewritePage selectedPages
  where selectedPages = filter (\p -> pageId p `S.member` includePages) pages

        rewritePage :: Page -> Page
        rewritePage page@Page{..} =
            page { pageName = titleText page
                 , pageSkeleton = mapMaybe (rewriteSkeleton [SectionPathPage pageId]) pageSkeleton
                 }

        rewriteSkeleton :: SectionPathId -> PageSkeleton -> Maybe PageSkeleton
        rewriteSkeleton sp (Section heading headingId skel) =
            let skel' = mapMaybe (rewriteSkeleton sp') skel
                keySkel = createNotesParagraphs sp'
            in  if sp' `S.member` includeSections
                then Just
                     $ Section heading' headingId
                     $ if processContent == NotesContent then keySkel else skel'
                else Nothing
          where sp' = sp <>  [SectionPathHeading headingId]
                heading' = headingText sp' heading

        rewriteSkeleton _ x = if processContent == OrigContent
                              then (Just x)
                              else Nothing

        createNotesParagraphs :: SectionPathId -> [PageSkeleton]
        createNotesParagraphs sp =
            fmap wrapInParagraph $ notesText sp
          where wrapInParagraph txt =
                    let paraBodies = [ParaText txt]
                        paraId = paraBodiesToId paraBodies
                    in Para (Paragraph paraId paraBodies )



        titleText :: Page -> PageName
        titleText Page{..} =
                    fromMaybe (pageName)
                    $ fmap (packPageName . T.unpack) $ pageId `M.lookup` titles


        headingText :: SectionPathId -> SectionHeading -> SectionHeading
        headingText sp sectionHeading =
            fromMaybe (sectionHeading)
            $ fmap SectionHeading $ sp `M.lookup` headings

        notesText :: SectionPathId -> [T.Text]
        notesText sp =
            filter (not . T.null)
            $ T.splitOn ", "
            $ fromMaybe "" $ sp `M.lookup` notes

-- Todo: this came from trec-car-tools CAR.CarExports
-- sadly we cannot depend on it directly, because
--    CarExports depends on simplir, and
--    miso-assessment depends on this module
--    and it seems impossible to nixify simplir
toStubSkeleton :: Page -> Stub
toStubSkeleton (Page name pageId ty meta skeleton) =
    Stub name pageId ty meta (mapMaybe go skeleton)
  where
    go :: PageSkeleton -> Maybe PageSkeleton
    go (Section heading headingId children) =
        Just $ Section heading headingId (mapMaybe go children)
    go (Para _)    = Nothing
    go (Image{})   = Nothing
    go (List{})    = Nothing
    go (Infobox{}) = Nothing
