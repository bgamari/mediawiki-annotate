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
import TQA
import TqaTopics

options :: Parser (FilePath, FilePath, FilePath)
options =
    (,,) <$> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output TqaStatus JSON file")
        <*> argument str (metavar "FILE" <> help "Input TqaStatus JSON file")
        <*> argument str (metavar "CBOR FILE" <> help "Input Tqa CBOR file")

main :: IO ()
main = do
    (outputPath, inputJsonPath, inputCborPath) <- execParser $ info (helper <*> options) mempty
    status <- either error id . Aeson.eitherDecode <$> BSL.readFile inputJsonPath
    pages <- readPagesFile inputCborPath
    let status' = changeText T.toLower pages status
    BSL.writeFile outputPath $ Aeson.encode status'


changeText :: (T.Text -> T.Text) -> [Page] -> TqaStatus -> TqaStatus
changeText textTransform pages status@TqaStatus{..} =
    status{ titles = M.fromList titles', headings = M.fromList headings'}
  where selectedPages = filter (\p -> pageId p `S.member` includePages) pages

        titles' = fmap changeTitle selectedPages
        headings' = foldMap changeHeading  selectedPages

        changeTitle :: Page -> (PageId, T.Text)
        changeTitle p = (pageId p, textTransform $ titleText p)

        changeHeading :: Page -> [(SectionPathId, T.Text)]
        changeHeading Page{..}= [ (sp, textTransform $ headingText sp sectionHeading)
                                | skel <- pageSkeleton
                                , (sp, sectionHeading) <- getHeadings [SectionPathPage pageId] skel
                                ]

        titleText Page{..} =
                    fromMaybe (T.pack $ unpackPageName pageName)
                    $ pageId `M.lookup` titles

        headingText :: SectionPathId -> SectionHeading -> T.Text
        headingText sp sectionHeading =
            fromMaybe (getSectionHeading sectionHeading)
            $ sp `M.lookup` headings

        getHeadings :: SectionPathId -> PageSkeleton -> [(SectionPathId, SectionHeading)]
        getHeadings sp (Section sectionHeading headingid skeleton) =
            let sp' = sp <>  [SectionPathHeading headingid]
                subtree = foldMap (getHeadings sp') skeleton
            in [(sp', sectionHeading)] <> subtree
        getHeadings sp _ = []
