{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Foldable (foldl')
import Data.Monoid hiding (All, Any)
import System.IO

import Options.Applicative
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe

import CAR.Utils
import CAR.Types
import SimplIR.Galago as Galago

opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")

isLead :: PageSkeleton -> Bool
isLead (Para _) = True
isLead (Section _ _ _) = False

data KbDoc = KbDoc { kbDocPageId :: PageId
                   , kbDocCanonicalName :: PageName
                   , kbDocLeadText :: [T.Text]
                   , kbDocCategories :: [T.Text]
                   , kbDocOutLinks :: [PageName]
                   }

data InlinkInfo = InlinkInfo { anchorName :: [ T.Text ]
                             , disambiguationName :: [ PageName ]
                             , redirectName :: [ PageName ]
                             }

instance Monoid InlinkInfo where
    mempty = InlinkInfo [] [] []
    InlinkInfo a b c `mappend` InlinkInfo a' b' c' =
      InlinkInfo (a <> a') (b <> b') (c <> c')

data InlinkCounts = InlinkCounts { anchorCount ::  !(HM.HashMap T.Text Int)
                                 , disambiguationCount :: !(HM.HashMap PageName Int)
                                 , redirectCount :: !(HM.HashMap PageName Int)
                                 }

instance Monoid InlinkCounts where
    mempty = InlinkCounts mempty mempty mempty
    InlinkCounts a b c `mappend` InlinkCounts a' b' c' =
        InlinkCounts (HM.unionWith (+) a a')
                     (HM.unionWith (+) b b')
                     (HM.unionWith (+) c c')


---  sourcepage targetpage anchortext => attach anchortext to targetpage
---  redirect sourcepage targetpage   => attach sourcepage to targetpage
--   similar for disambiguation

collectInlinkInfo :: [Page] -> HM.HashMap PageName InlinkInfo
collectInlinkInfo = foldl' (\acc -> HM.unionWith mappend acc . pageInlinkInfo) mempty
  where
    pageInlinkInfo :: Page -> HM.HashMap PageName InlinkInfo
    pageInlinkInfo page
      | Just linkTarget <- pageRedirect page  =
            HM.singleton linkTarget (mempty { redirectName = [ pageName page ] })

      | pageIsDisambiguation page  =
            let toInlinkInfo (linkTarget, anchorText) =
                    HM.singleton linkTarget (mempty { disambiguationName = [ pageName page ] })
            in unions $ map toInlinkInfo (pageLinks page)

      | otherwise  =
            let toInlinkInfo (linkTarget, anchorText) =
                   HM.singleton linkTarget (mempty { anchorName = [ anchorText ] })
            in unions $ map toInlinkInfo (pageLinks page)
      where
        unions = foldl' (HM.unionWith mappend) mempty


linkCount :: InlinkInfo -> InlinkCounts
linkCount inlinkInfo =
    InlinkCounts { anchorCount = histo $ anchorName $ inlinkInfo
                 , disambiguationCount = histo $ disambiguationName $ inlinkInfo
                 , redirectCount = histo $ redirectName $ inlinkInfo
                 }
  where
    histo :: (Hashable a, Eq a) => [a] -> HM.HashMap a Int
    histo list = HM.fromListWith (+) $ map (\name -> (name, 1)) $ list

linkStatistics :: HM.HashMap PageName InlinkInfo -> InlinkCounts
linkStatistics inlinkInfoMap =
     foldMap linkCount $ inlinkInfoMap

-- #(anchor, target) / #(anchor, *)
transformContent :: HM.HashMap PageName InlinkInfo -> Page -> Maybe KbDoc
transformContent inlinkInfoMap (Page pageName pageId pageSkeleta) =
  let leadParas = filter isLead $ pageSkeleta
      kbDocPageId = pageId
      kbDocLeadText = foldMap pageSkeletonText $ leadParas
      kbDocOutLinks = fmap fst $ foldMap pageSkeletonLinks $ leadParas
      kbDocCategories = pageCategories (Page pageName pageId pageSkeleta)
      kbDocCanonicalName = pageName

      inlinkInfo = fromMaybe mempty $ HM.lookup pageName inlinkInfoMap

      kbDocAnchorNames = anchorName inlinkInfo -- todo consolidate multiple occurrences of the same name
      disambiguationNames = disambiguationName inlinkInfo
      redirectNames = redirectName inlinkInfo
  in Just KbDoc {..}

toGalagoDoc :: HM.HashMap PageName InlinkInfo -> InlinkCounts -> KbDoc -> Galago.Document
toGalagoDoc inlinkInfoMap linkStats kbDoc =
    let meta = M.fromList []
        phrase x = (TL.fromStrict x, Galago.DoNotTokenize)
        naturalText x = (TL.fromStrict x, Galago.DoTokenize)
        inlinkInfo = fromMaybe mempty $ HM.lookup (kbDocCanonicalName kbDoc) inlinkInfoMap
    in Galago.Document { docId       = Galago.DocumentId $ T.pack $ unpackPageId $ kbDocPageId kbDoc
                       , docMetadata = meta
                       , docFields   = M.fromList
                          [ ("title",               [phrase $ getPageName $ kbDocCanonicalName kbDoc])
                          , ("links",               map (phrase . getPageName) $ kbDocOutLinks kbDoc)
                          , ("categories",          map phrase $ kbDocCategories kbDoc)
                          , ("lead",                [naturalText $ T.unlines $ kbDocLeadText kbDoc])
                          , ("anchornames",         map phrase $ anchorName $ inlinkInfo)
                          , ("disambiguationnames", map (phrase . getPageName) $ disambiguationName $ inlinkInfo)
                          , ("redirectnames",       map (phrase . getPageName) $ redirectName $ inlinkInfo )
                          ]
                       }

main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
    let inlinkInfo = collectInlinkInfo pages
        linkStats = linkStatistics inlinkInfo
    withFile outputFile WriteMode $ \h ->
        BSL.hPutStr h $ Galago.toWarc
            $ map (toGalagoDoc inlinkInfo linkStats)
            $ mapMaybe (transformContent inlinkInfo) $ pages
