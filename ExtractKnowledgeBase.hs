{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Exception (assert)
import Data.Monoid hiding (All, Any)
import System.IO

import Options.Applicative
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
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

data InlinkInfo = InlinkInfo { documentInlinks :: !(HM.HashMap PageName InlinkCounts)
                             , redirectPages   :: !(HS.HashSet PageName)
                             }

instance Monoid InlinkInfo where
    mempty = InlinkInfo mempty mempty
    InlinkInfo a b `mappend` InlinkInfo a' b' =
        InlinkInfo (HM.unionWith mappend a a') (b<>b')

data InlinkCounts = InlinkCounts { -- | How many time each anchor is used to link to this document.
                                   anchorCount ::  !(HM.HashMap T.Text Int)
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
resolveRedirects :: InlinkInfo -> HM.HashMap PageName InlinkCounts
resolveRedirects (InlinkInfo {..}) =
    fmap resolve $ HM.filterWithKey isRedirect documentInlinks
  where
    isRedirect k _ = not $ k `HS.member` redirectPages

    resolve :: InlinkCounts -> InlinkCounts
    resolve counts =
        counts <> mconcat [ assert (n==1) $ fromMaybe mempty $ HM.lookup rpage documentInlinks
                          | (rpage, n) <- HM.toList $ redirectCount counts ]


-- | Given a set of documents, build a map from target document to its 'InlinkCounts'
collectInlinkInfo :: [Page] -> InlinkInfo
collectInlinkInfo = foldMap pageInlinkInfo
  where
    one :: Hashable a => a -> HM.HashMap a Int
    one x = HM.singleton x 1

    pageInlinkInfo :: Page -> InlinkInfo
    pageInlinkInfo page
      | Just linkTarget <- pageRedirect page  =
            mempty { documentInlinks = HM.singleton linkTarget
                                       $ mempty { redirectCount = one $ pageName page }
                   , redirectPages = HS.singleton (pageName page)
                   }

      | pageIsDisambiguation page  =
            let toInlinkInfo (linkTarget, anchorText) =
                    mempty { documentInlinks = HM.singleton linkTarget
                                               $ mempty { disambiguationCount = one $ pageName page }
                           }
            in foldMap toInlinkInfo (pageLinks page)

      | otherwise  =
            let toInlinkInfo (linkTarget, anchorText) =
                   mempty { documentInlinks = HM.singleton linkTarget
                                              $ mempty { anchorCount = one $ anchorText }
                          }
            in foldMap toInlinkInfo (pageLinks page)

-- #(anchor, target) / #(anchor, *)
transformContent :: HM.HashMap PageName InlinkCounts -> Page -> Maybe KbDoc
transformContent inlinkInfoMap (Page pageName pageId pageSkeleta) =
  let leadParas = filter isLead $ pageSkeleta
      kbDocPageId = pageId
      kbDocLeadText = foldMap pageSkeletonText $ leadParas
      kbDocOutLinks = fmap fst $ foldMap pageSkeletonLinks $ leadParas
      kbDocCategories = pageCategories (Page pageName pageId pageSkeleta)
      kbDocCanonicalName = pageName

      inlinkInfo = fromMaybe mempty $ HM.lookup pageName inlinkInfoMap

      kbDocAnchorNames = anchorCount inlinkInfo -- todo consolidate multiple occurrences of the same name
      disambiguationNames = disambiguationCount inlinkInfo
      redirectNames = redirectCount inlinkInfo
  in Just KbDoc {..}

toGalagoDoc :: HM.HashMap PageName InlinkCounts -> InlinkCounts -> KbDoc -> Galago.Document
toGalagoDoc inlinkInfoMap linkStats kbDoc =
    let meta = M.fromList []
        phrase x = (TL.fromStrict x, Galago.DoNotTokenize)
        naturalText x = (TL.fromStrict x, Galago.DoTokenize)
        inlinkInfo = fromMaybe mempty $ HM.lookup (kbDocCanonicalName kbDoc) inlinkInfoMap
        repeated :: HM.HashMap a Int -> [a]
        repeated = foldMap f . HM.toList
          where f (x,n) = replicate n x
    in Galago.Document { docId       = Galago.DocumentId $ T.pack $ unpackPageId $ kbDocPageId kbDoc
                       , docMetadata = meta
                       , docFields   = M.fromList
                          [ ("title",               [phrase $ getPageName $ kbDocCanonicalName kbDoc])
                          , ("links",               map (phrase . getPageName) $ kbDocOutLinks kbDoc)
                          , ("categories",          map phrase $ kbDocCategories kbDoc)
                          , ("lead",                [naturalText $ T.unlines $ kbDocLeadText kbDoc])
                          , ("anchornames",         map phrase $ repeated $ anchorCount $ inlinkInfo)
                          , ("disambiguationnames", map (phrase . getPageName) $ repeated $ disambiguationCount $ inlinkInfo)
                          , ("redirectnames",       map (phrase . getPageName) $ repeated $ redirectCount $ inlinkInfo )
                          ]
                       }

main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
    let inlinkInfo = resolveRedirects $ collectInlinkInfo pages
        linkStats = mconcat $ HM.elems inlinkInfo
    withFile outputFile WriteMode $ \h ->
        BSL.hPutStr h $ Galago.toWarc
            $ map (toGalagoDoc inlinkInfo linkStats)
            $ mapMaybe (transformContent inlinkInfo) $ pages

