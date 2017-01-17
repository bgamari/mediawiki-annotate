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


data KbDoc = KbDoc { kbDocParagraphId :: ParagraphId
                   , kbDocArticleId :: PageId
                   , kbDocSourceEntity :: PageName
                   , kbDocSourceEntityId :: PageId
                   , kbDocSectionPath :: SectionPath'
                   , kbDocCategories :: [T.Text]
                   , kbDocParagraph :: Pa:ragraph
                   , kbDocOutlinks ::  [(PageName, T.Text)]
                   , kbDocOutlinkIds ::  [PageId]
                   }

-- pageNameToEntity :: PageName -> Entity
-- pageNameToEntity pageName =
--     Entity {entityId = pageNameToId pageName, entityCanonicalName = pageName }

-- todo handle links to Disambiguation pages and redirects

transformContent :: Page -> [KbDoc]
transformContent (Page pageName' pageId pageSkeleta) =
    foldMap (go mempty) pageSkeleta
  where
    pageName = normTargetPageName pageName'
    go :: DList.DList SectionHeading -> PageSkeleton -> [KbDoc]
    go parentHeadings (Section heading _ children) =
      let parentHeadings' = parentHeadings `DList.snoc` heading
       in concatMap (go parentHeadings') children
    go parentHeadings (Para paragraph) =
      let sectionPath = SectionPath' pageName (DList.toList parentHeadings)
      in [convertPara paragraph sectionPath]

    convertPara :: Paragraph -> SectionPath' -> KbDoc
    convertPara paragraph sectionPath =
      let
        kbDocParagraphId    = paraId $ paragraph
        kbDocArticleId      = pageId
        kbDocSourceEntity   = pageName
        kbDocSourceEntityId = pageNameToId pageName
        kbDocSectionPath    = sectionPath
        kbDocCategories     = pageCategories (Page pageName pageId pageSkeleta)
        kbDocParagraph      = paragraph
        kbDocOutlinks       = fmap (first normTargetPageName) $ paraLinks $ paragraph
        kbDocOutlinkIds     = fmap (pageNameToId . fst) $ kbDocOutlinks
      in KbDoc {..}


toGalagoDoc :: KbDoc -> Galago.Document
toGalagoDoc kbDoc =
    let galagoDocId = T.pack $ (unpackPageId $ kbDocArticleId $ kbDoc) ++ "/" ++ (unpackParagraphId $ paraId $ kbDocParagraph $ kbDoc)
        sectionPath = fmap getSectionHeading
                      $ sectionPath'Headings
                      $ kbDocSectionPath
                      $ kbDoc
        meta = M.fromList [ ("paragraphId", T.pack $ unpackParagraphId $ kbDocParagraphId $ kbDoc)
                          , ("articleId", T.pack $ unpackPageId $ kbDocArticleId $ kbDoc)
                          , ("sourceEntity", T.pack $ unpackPageId $ kbDocSourceEntityId $ kbDoc)
                          , ("sectionpath", T.intercalate " / " $ sectionPath)
                          , ("categories", T.intercalate " " $ kbDocCategories $ kbDoc)
                          , ("targetEntities", T.intercalate " " $ map (T.pack . unpackPageId) $ kbDocOutlinkIds $ kbDoc)
                          , ("targetEntityAnchors", T.intercalate " " $ fmap snd $ kbDocOutlinks $ kbDoc )
                          ]
        phrase x = (TL.fromStrict x, Galago.DoNotTokenize)
        naturalText x = (TL.fromStrict x, Galago.DoTokenize)
    in Galago.Document { docId       = Galago.DocumentId $ galagoDocId
                       , docMetadata = fmap (BSL.fromStrict . T.encodeUtf8) meta
                       , docFields   = M.fromList
                          [ ("sourceentity",         [naturalText $ getPageName $ kbDocSourceEntity kbDoc])
                          , ("sourceentity-exact",   [phrase $ getPageName $ kbDocSourceEntity kbDoc])
                          , ("category",             map phrase $ kbDocCategories kbDoc)
                          , ("section",              [naturalText $ T.unwords $ sectionPath])
                          , ("section-exact",        [phrase $ T.unwords $ sectionPath])
                          , ("paragraph",            [naturalText $ paraToText $ kbDocParagraph kbDoc])
                          , ("targetentity",         fmap (naturalText . getPageName . fst) $ kbDocOutlinks $ kbDoc)
                          , ("targetentity-exact",   fmap (phrase . getPageName . fst) $ kbDocOutlinks $ kbDoc)
                          , ("anchor",               fmap (naturalText . T.unwords . T.lines . snd) $ kbDocOutlinks $ kbDoc)
                          , ("anchor-exact",         fmap (phrase . T.unwords . T.lines . snd) $ kbDocOutlinks $ kbDoc)
                          ]
                       }

-- Todo pagenames with '#' , and also normalize first letter.

--         kbDocParagraphId    = paraId $ paragraph
--         kbDocArticleId      = pageId
--         kbDocSourceEntity   = pageName
--         kbDocSourceEntity   = pageNameToId pageName
--         kbDocSectionPath    = sectionpath
--         kbDocCategories     = pageCategories (Page pageName pageId pageSkeleta)
--         kbDocParagraph      = paraToText paragraph
--         kbDocOutlinks       = pageSkeletonLinks paragraph
--         kbDocOutlinkIds     = fmap (\(ParaLink pagename anchor) -> pageNameToId pagename) $ kbDocOutlinks


main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
    withFile outputFile WriteMode $ \h ->
        BSL.hPutStr h $ Galago.toWarc
            $ map toGalagoDoc
            $ foldMap (nubKbDocs . transformContent)
            $ pages
  where
    nubKbDocs :: [KbDoc] -> [KbDoc]
    nubKbDocs kbDocs =
      HM.elems $ HM.fromList $ fmap (\kbDoc -> (key kbDoc, kbDoc)) $ kbDocs
      where
        key kbDoc = kbDocParagraphId $ kbDoc
--         key kbDoc = (kbDocParagraphId $ kbDoc, kbDocArticleId $ kbDoc)   -- if you nub across multiple articles