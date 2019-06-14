{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric#-}
{-# LANGUAGE DuplicateRecordFields#-}

module Main where

import GHC.Generics
import Control.Monad

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

options :: Parser (FilePath, FilePath, FilePath, Subset, ProcessContent, Maybe FilePath)
options =
    (,,,,,) <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output CBOR file")
        <*> argument str (metavar "FILE" <> help "Input TqaStatus JSON file")
        <*> argument str (metavar "CBOR FILE" <> help "Input Tqa CBOR file")
        <*> (trainParser <|> testParser)
        <*> (onlyStubContentParser <|> origContentParser <|> keywordContentParser)
        <*> optional (option str (short 'c' <> long "compat-out" <> metavar "JSON" <> help "File for Y2/Y3 compatibility info"))
  where trainParser = flag' TrainSubset (short 't' <> long "train" <> help "Only export training subset")
        testParser = flag' TestSubset (short 'T' <> long "test" <> help "Only export test subset")
        onlyStubContentParser = flag' OnlyStubContent (short 's' <> long "only-stub-content" <> help "Only export stub content")
        origContentParser = flag' OrigContent (short 'r' <> long "orig-content" <> help "Export all original content")
        keywordContentParser = flag' NotesContent (short 'n' <> long "notes-content" <> help "Export all original content and keywords")


main :: IO ()
main = do
    (outputPath, inputJsonPath, inputCborPath, subset, processContent, compatFile) <- execParser $ info (helper <*> options) mempty
    status <- either error id . Aeson.eitherDecode <$> BSL.readFile inputJsonPath
              :: IO TqaStatus
    (prov, pages) <- readPagesFileWithProvenance inputCborPath
    let subsetStr = (case subset of
                          TestSubset -> "test"
                          TrainSubset -> "train"
                    )

        prov' = prov {dataReleaseName = "benchmarkY3" <> subsetStr
                     , comments = ["TREC-CAR Dataset by Laura Dietz, Ben Gamari is licensed under a Creative Commons Attribution-ShareAlike 3.0 Unported License. Based on a work at http://data.allenai.org/tqa/ ."
                                  , ("Used as "<> subsetStr <> " data for year 3 of the TREC Complex Answer Retrieval track.")
                                  ]
                     }
    let pages' = convertPages subset processContent status pages
    case processContent of
      OnlyStubContent ->
          writeCarFile outputPath prov' $ fmap toStubSkeleton pages'
      OrigContent ->
          writeCarFile outputPath prov' pages'
      NotesContent ->
          writeCarFile outputPath prov' pages'

    let compats = convertCompatFile subset status pages

    if (compatFile /= Nothing)
    then BSL.writeFile (fromJust compatFile) $ Aeson.encode compats
    else return ()

selectPages :: Subset -> TqaStatus ->  [Page] -> [Page]
selectPages subset status@TqaStatus{..}  pages =
    filter includePage pages
  where includePage Page{..} =
            (pageId `S.member` includePages) &&
            case subset of
              TrainSubset -> pageId `S.member` trainTitles
              TestSubset -> not $ pageId `S.member` trainTitles


data TqaY3Compat = TqaY3Compat { y2SectionId :: T.Text
                               , y2PageTitle :: PageName
                               , y2Heading :: SectionHeading
                               , sectionId :: T.Text
                               , pageTitle :: PageName
                               , headings :: SectionHeading
                               , keywords :: Maybe [T.Text]
                               }
    deriving (ToJSON, Generic)

convertCompatFile :: Subset -> TqaStatus -> [Page] -> [TqaY3Compat]
convertCompatFile subset status@TqaStatus{..} pages =
    [ TqaY3Compat { sectionId = unpackSectionPathId sectionPathId
                  , y2SectionId = T.pack $ oldHeadingId pageName sectionHeading
                  , y2Heading = sectionHeading
                  , y2PageTitle = pageName
                  , pageTitle = titleText page
                  , headings = headingText sectionPathId sectionHeading
                  , keywords = notesText sectionPathId
                  }
    | page@Page{..} <- selectedPages
    , skel <- pageSkeleton
    , (sectionPathId, sectionHeading) <- getHeadings [SectionPathPage pageId] skel
    ,  sectionPathId `S.member` includeSections
    ]
  where selectedPages = selectPages subset status pages
        getHeadings :: SectionPathId -> PageSkeleton -> [(SectionPathId, SectionHeading)]
        getHeadings sp (Section sectionHeading headingid skeleton) =
            let sp' = sp <>  [SectionPathHeading headingid]
                subtree = foldMap (getHeadings sp') skeleton
            in [(sp', sectionHeading)] <> subtree
        getHeadings sp _ = []

        notesText :: SectionPathId -> Maybe [T.Text]
        notesText sp =
            let keywordList =
                    filter (not . T.null)
                    $ fmap T.strip
                    $ T.splitOn ", "
                    $ fromMaybe "" $ sp `M.lookup` notes
            in case keywordList of
                    [] -> Nothing
                    x -> Just x

        headingText :: SectionPathId -> SectionHeading -> SectionHeading
        headingText sp sectionHeading =
            fromMaybe (sectionHeading)
            $ fmap SectionHeading $ sp `M.lookup` headings

        titleText :: Page -> PageName
        titleText Page{..} =
                    fromMaybe (pageName)
                    $ fmap (packPageName . T.unpack) $ pageId `M.lookup` titles



        oldHeadingId pageName sectionHeading  =
            let h = unpackHeadingId $ sectionHeadingToId sectionHeading
                t = unpackPageId $ pageNameToId "tqa" pageName
            in (t <> "/" <> h)




convertPages :: Subset -> ProcessContent -> TqaStatus ->  [Page] -> [Page]
convertPages subset processContent status@TqaStatus{..}  pages =
    fmap rewritePage selectedPages
  where selectedPages = selectPages subset status pages
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
