{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Monoid hiding (All, Any)
import System.IO

import Options.Applicative
import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Bifunctor

import CAR.Utils
import CAR.Types
import SimplIR.Galago as Galago

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")

-- data Entity = Entity { entityId :: PageId
--                      , entityCanonicalName :: PageName
--                      }


data SectionPath' = SectionPath' { sectionPath'PageName :: PageName
                                 , sectionPath'Headings :: [SectionHeading]
                                 }
               deriving (Show, Eq, Ord)


data LinkDoc = LinkDoc { linkDocParagraphId :: ParagraphId
                   , linkDocArticleId :: PageId
                   , linkDocSourceEntity :: PageName
                   , linkDocSourceEntityId :: PageId
                   , linkDocSectionPath :: SectionPath'
                   , linkDocCategories :: [T.Text]
                   , linkDocParagraph :: Paragraph
                   , linkDocOutlinks ::  [(PageName, T.Text)]
                   , linkDocOutlinkIds ::  [PageId]
                   }

-- pageNameToEntity :: PageName -> Entity
-- pageNameToEntity pageName =
--     Entity {entityId = pageNameToId pageName, entityCanonicalName = pageName }

-- todo handle links to Disambiguation pages and redirects

transformContent :: Page -> [LinkDoc]
transformContent (Page pageName pageId pageSkeleta) =
    foldMap (go mempty) pageSkeleta
  where
    go :: DList.DList SectionHeading -> PageSkeleton -> [LinkDoc]
    go parentHeadings (Section heading _ children) =
      let parentHeadings' = parentHeadings `DList.snoc` heading
       in concatMap (go parentHeadings') children
    go parentHeadings (Para paragraph) =
      let sectionPath = SectionPath' pageName (DList.toList parentHeadings)
      in [convertPara paragraph sectionPath]

    convertPara :: Paragraph -> SectionPath' -> LinkDoc
    convertPara paragraph sectionPath =
      let
        linkDocParagraphId    = paraId $ paragraph
        linkDocArticleId      = pageId
        linkDocSourceEntity   = pageName
        linkDocSourceEntityId = pageNameToId pageName
        linkDocSectionPath    = sectionPath
        linkDocCategories     = pageCategories (Page pageName pageId pageSkeleta)
        linkDocParagraph      = paragraph
        linkDocOutlinks       = paraLinks $ paragraph
        linkDocOutlinkIds     = fmap (pageNameToId . fst) $ linkDocOutlinks
      in LinkDoc {..}


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
                          , ("targetEntities", T.intercalate " " $ map (T.pack . unpackPageId) $ linkDocOutlinkIds $ linkDoc)
                          , ("targetEntityAnchors", T.intercalate " " $ fmap snd $ linkDocOutlinks $ linkDoc )
                          ]
        phrase x = (TL.fromStrict x, Galago.DoNotTokenize)
        naturalText x = (TL.fromStrict x, Galago.DoTokenize)
    in Galago.Document { docId       = Galago.DocumentId $ galagoDocId
                       , docMetadata = fmap (BSL.fromStrict . T.encodeUtf8) meta
                       , docFields   = M.fromList
                          [ ("sourceentity",         [naturalText $ getPageName $ linkDocSourceEntity linkDoc])
                          , ("sourceentity-exact",   [phrase $ getPageName $ linkDocSourceEntity linkDoc])
                          , ("category",             map phrase $ linkDocCategories linkDoc)
                          , ("section",              [naturalText $ T.unwords $ sectionPath])
                          , ("section-exact",        [phrase $ T.unwords $ sectionPath])
                          , ("paragraph",            [naturalText $ paraToText $ linkDocParagraph linkDoc])
                          , ("targetentity",         fmap (naturalText . getPageName . fst) $ linkDocOutlinks $ linkDoc)
                          , ("targetentity-exact",   fmap (phrase . getPageName . fst) $ linkDocOutlinks $ linkDoc)
                          , ("anchor",               fmap (naturalText . T.unwords . T.lines . snd) $ linkDocOutlinks $ linkDoc)
                          , ("anchor-exact",         fmap (phrase . T.unwords . T.lines . snd) $ linkDocOutlinks $ linkDoc)
                          ]
                       }

-- Todo pagenames with '#' , and also normalize first letter.

--         linkDocParagraphId    = paraId $ paragraph
--         linkDocArticleId      = pageId
--         linkDocSourceEntity   = pageName
--         linkDocSourceEntity   = pageNameToId pageName
--         linkDocSectionPath    = sectionpath
--         linkDocCategories     = pageCategories (Page pageName pageId pageSkeleta)
--         linkDocParagraph      = paraToText paragraph
--         linkDocOutlinks       = pageSkeletonLinks paragraph
--         linkDocOutlinkIds     = fmap (\(ParaLink pagename anchor) -> pageNameToId pagename) $ linkDocOutlinks


main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSL.hPutStr h $ Galago.toWarc
            $ map toGalagoDoc
            $ foldMap (nubLinkDocs . dropLinkDocsNoLinks . transformContent)
            $ pages
  where
    nubLinkDocs :: [LinkDoc] -> [LinkDoc]
    nubLinkDocs linkDocs =
        HM.elems $ HM.fromList $ fmap (\linkDoc -> (key linkDoc, linkDoc)) $ linkDocs
      where
        key linkDoc = linkDocParagraphId $ linkDoc
--         key linkDoc = (linkDocParagraphId $ linkDoc, linkDocArticleId $ linkDoc)   -- if you nub across multiple articles
    dropLinkDocsNoLinks :: [LinkDoc] -> [LinkDoc]
    dropLinkDocsNoLinks =
        filter (\linkDoc -> not ( null (linkDocOutlinkIds $linkDoc)))