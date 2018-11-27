{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

import Options.Applicative
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe

import CAR.Utils
import CAR.Types
import SimplIR.Galago as Galago
import CAR.AnnotationsFile as CAR

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")



data SectionPath' = SectionPath' { sectionPath'PageName :: PageName
                                 , sectionPath'Headings :: [SectionHeading]
                                 }
               deriving (Show, Eq, Ord
               )


data LinkDoc = LinkDoc { linkDocParagraphId    :: ParagraphId
                       , linkDocArticleId      :: PageId
                       , linkDocSourceEntity   :: PageName
                       , linkDocSourceEntityId :: PageId
                       , linkDocSectionPath    :: SectionPath'
                       , linkDocCategories     :: [T.Text]
                       , linkDocParagraph      :: Paragraph
                       , linkDocOutlinks       ::  [Link]
                       }

-- todo handle links to Disambiguation pages and redirects

transformContent :: PageBundle -> Page -> [LinkDoc]
transformContent pageBundle page@(Page {pageName, pageId, pageSkeleton = pageSkeleta}) =
    foldMap (go mempty) pageSkeleta
  where
    go :: DList.DList SectionHeading -> PageSkeleton -> [LinkDoc]
    go parentHeadings (Section heading _ children) =
      let parentHeadings' = parentHeadings `DList.snoc` heading
       in concatMap (go parentHeadings') children
    go parentHeadings (Para paragraph) =
      let sectionPath = SectionPath' pageName (DList.toList parentHeadings)
      in [convertPara paragraph sectionPath]
    go parentHeadings (Image {}) =
      mempty
    go parentHeadings (Infobox {}) =
      mempty
    go parentHeadings (List {}) =
      mempty

    convertPara :: Paragraph -> SectionPath' -> LinkDoc
    convertPara paragraph sectionPath =
      let
        linkDocParagraphId    = paraId $ paragraph
        linkDocArticleId      = pageId
        linkDocSourceEntity   = pageName
        linkDocSourceEntityId = getOne $ fromMaybe (error $ "Can't find pageName " ++ (show pageName)) $ CAR.bundleLookupPageName pageBundle pageName
        linkDocSectionPath    = sectionPath
        -- FIXME
        --linkDocCategories     = pageCategories $ pageMetadata page
        linkDocParagraph      = paragraph
        linkDocOutlinks       = paraLinks $ paragraph
        linkDocCategories     = []
      in LinkDoc {..}

    getOne :: S.Set a -> a
    getOne s = head $ S.toList s


toGalagoDoc :: LinkDoc -> Galago.Document
toGalagoDoc linkDoc =
    let galagoDocId = T.pack $ (unpackPageId $ linkDocArticleId $ linkDoc) ++ "/" ++ (unpackParagraphId $ paraId $ linkDocParagraph $ linkDoc)
        sectionPath = fmap getSectionHeading
                      $ sectionPath'Headings
                      $ linkDocSectionPath
                      $ linkDoc
        meta = M.fromList [ ("paragraphId", T.pack $ unpackParagraphId $ linkDocParagraphId $ linkDoc)
                          , ("articleId", T.pack $ unpackPageId $ linkDocArticleId $ linkDoc)
                          , ("sourceEntity", T.pack $ unpackPageId $ linkDocSourceEntityId $ linkDoc)
                          , ("sectionpath", T.intercalate " / " $ sectionPath)
                          , ("categories", T.intercalate " " $ linkDocCategories $ linkDoc)
                          , ("targetEntities", T.intercalate " " $ map (T.pack . unpackPageId . linkTargetId) $ linkDocOutlinks $ linkDoc)
                          , ("targetEntityAnchors", T.intercalate " " $ fmap linkAnchor $ linkDocOutlinks $ linkDoc )
                          ]
        phrase x = (TL.fromStrict x, Galago.DoNotTokenize)
        naturalText x = (TL.fromStrict x, Galago.DoTokenize)
        naturalLazyText x = (x, Galago.DoTokenize)
    in Galago.Document { docId       = Galago.DocumentId $ galagoDocId
                       , docMetadata = fmap (BSL.fromStrict . T.encodeUtf8) meta
                       , docFields   = M.fromList
                          [ ("sourceentity",         [naturalText $ getPageName $ linkDocSourceEntity linkDoc])
                          , ("sourceentity-exact",   [phrase $ getPageName $ linkDocSourceEntity linkDoc])
                          , ("category",             map phrase $ linkDocCategories linkDoc)
                          , ("section",              [naturalText $ T.unwords $ sectionPath])
                          , ("section-exact",        [phrase $ T.unwords $ sectionPath])
                          , ("paragraph",            [naturalLazyText $ paraToText $ linkDocParagraph linkDoc])
                          , ("targetentity",         fmap (naturalText . getPageName . linkTarget) $ linkDocOutlinks $ linkDoc)
                          , ("targetentity-exact",   fmap (phrase . getPageName . linkTarget) $ linkDocOutlinks $ linkDoc)
                          , ("anchor",               fmap (naturalText . T.unwords . T.lines . linkAnchor) $ linkDocOutlinks $ linkDoc)
                          , ("anchor-exact",         fmap (phrase . T.unwords . T.lines . linkAnchor) $ linkDocOutlinks $ linkDoc)
                          ]
                       }

-- Todo pagenames with '#' , and also normalize first letter.


main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pageBundle <- CAR.openPageBundle inputFile
    BSL.writeFile outputFile
        $ Galago.toWarc
        $ map toGalagoDoc
        $ foldMap (nubLinkDocs . dropLinkDocsNoLinks . transformContent (pageBundle))
        $ CAR.bundleAllPages pageBundle
  where
    nubLinkDocs :: [LinkDoc] -> [LinkDoc]
    nubLinkDocs linkDocs =
        HM.elems $ HM.fromList $ fmap (\linkDoc -> (key linkDoc, linkDoc)) $ linkDocs
      where
        key linkDoc = linkDocParagraphId $ linkDoc
    dropLinkDocsNoLinks :: [LinkDoc] -> [LinkDoc]
    dropLinkDocsNoLinks =
        filter (\linkDoc -> not ( null (linkDocOutlinks $ linkDoc)))
