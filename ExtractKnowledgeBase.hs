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
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TB
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
                   , kbDocLeadText :: [T.Text]    -- ^ Page's lead paragraph text
                   , kbDocCategories :: [T.Text]  -- ^ Page's categories
                   , kbDocOutLinks :: [PageName]  -- ^ Targets of page's outgoing links
                   , kbDocOutMentions :: [T.Text] -- ^ Anchor text of page's outgoing links
                   , kbDocLeadPara :: [PageSkeleton]   -- ^ 'PageSkeleton's of lead paragraph text
                   }

wikiAnchorStopwords = [ "more....", "wikipedia article", "source: wikipedia", "here", "wiki", "wikipedia"]
wikiAnchorStopphrases = [ "wikipedia the free encyclopedia", "en.wikipedia.org", "full article at wikipedia.org"
                        , "wikipedia the free encyclopedia", "wikipedia, the free encyclopedia"
                        , "http://en.wikipedia.org/wiki/", "wikipedia"]


skeletonToXml :: [PageSkeleton] -> TB.Builder
skeletonToXml list =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<articles loadtime=\"0 sec\" rendertime=\"0.06 sec\" totaltime=\"0.06 sec\"><article> "
    <> foldMap skelToXml list
    <>" </article></articles>"
  where

    skelToXml :: PageSkeleton -> TB.Builder
    skelToXml (Para (Paragraph paraId parabodies ) )  =
        " <paragraph><sentence> "  -- id = 1
        <> foldMap bodyToXml parabodies
        <> " </sentence></paragraph> "
    skelToXml (Section (SectionHeading heading) _ parabodies )  =
        " <heading level=\"1\"> "<> TB.fromText (T.replace "\n" " " heading) <>" </heading> "  -- id = 1


    bodyToXml :: ParaBody -> TB.Builder
    bodyToXml (ParaText text) = " " <> TB.fromText (T.replace "\n" " " text) <> " "
    bodyToXml (ParaLink (PageName pagename) anchor) =
        " <link><target> "<>TB.fromText pagename<>" </target></link> "


data InlinkInfo = InlinkInfo { documentInlinks :: !(HM.HashMap PageName InlinkCounts)
                             , redirectPages   :: !(HS.HashSet PageName)
                             }

instance Monoid InlinkInfo where
    mempty = InlinkInfo mempty mempty
    InlinkInfo a b `mappend` InlinkInfo a' b' =
        InlinkInfo (HM.unionWith mappend a a') (b<>b')

data InlinkCounts = InlinkCounts { -- | How many time each anchor is used to link to this document.
                                   inLinkCounts        :: !(HM.HashMap PageName Int)
                                 , anchorCount         :: !(HM.HashMap T.Text Int)
                                 , disambiguationCount :: !(HM.HashMap PageName Int)
                                 , redirectCount       :: !(HM.HashMap PageName Int)
                                 }

instance Monoid InlinkCounts where
    mempty = InlinkCounts mempty mempty mempty mempty
    InlinkCounts a b c d `mappend` InlinkCounts a' b' c' d' =
        InlinkCounts (HM.unionWith (+) a a')
                     (HM.unionWith (+) b b')
                     (HM.unionWith (+) c c')
                     (HM.unionWith (+) d d')

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
                                              $ mempty { anchorCount = one $ anchorText, inLinkCounts = one $ linkTarget }
                          }
            in foldMap toInlinkInfo (pageLinks page)

-- #(anchor, target) / #(anchor, *)
transformContent :: HM.HashMap PageName InlinkCounts -> Page -> Maybe KbDoc
transformContent inlinkInfoMap (Page pageName pageId pageSkeleta) =
  let leadParas = filter isLead $ pageSkeleta
      kbDocPageId = pageId
      kbDocLeadText = foldMap pageSkeletonText $ leadParas
      kbDocOutLinks = fmap fst $ foldMap pageSkeletonLinks $ leadParas
      kbDocOutMentions = fmap snd $ foldMap pageSkeletonLinks $ leadParas
      kbDocLeadPara = leadParas
      kbDocCategories = pageCategories (Page pageName pageId pageSkeleta)
      kbDocCanonicalName = pageName

      inlinkInfo = fromMaybe mempty $ HM.lookup pageName inlinkInfoMap

      kbDocAnchorNames = anchorCount inlinkInfo -- todo consolidate multiple occurrences of the same name
      disambiguationNames = disambiguationCount inlinkInfo
      redirectNames = redirectCount inlinkInfo
  in Just KbDoc {..}

toGalagoDoc :: HM.HashMap PageName InlinkCounts -> InlinkCounts -> KbDoc -> Galago.Document
toGalagoDoc inlinkInfoMap linkStats kbDoc =
    let inlinkInfo = fromMaybe mempty $ HM.lookup (kbDocCanonicalName kbDoc) inlinkInfoMap
        xml = skeletonToXml (kbDocLeadPara kbDoc)
        numAnchors = sum $ anchorCount inlinkInfo
        numOutlinks = length $ kbDocOutLinks $ kbDoc
        meta = M.fromList [
                            ("fbname", TL.fromStrict $ getPageName $ kbDocCanonicalName kbDoc)
                          , ("contextLinks", "") -- pagename 1\npage2name 2\n"
                          , ("xml", TB.toLazyText xml)
                          , ("kbId","")
                          , ("lastModified", "2012-01-20 08:14:38")
                          , ("inlink", TL.pack $ show numAnchors)
                          , ("externalLinkCount", TL.pack $ show numOutlinks)
                          , ("category",  TL.intercalate "," $ map TL.fromStrict $ kbDocCategories kbDoc) -- "Category:Healthcare occupations,Category:Occupational therapy")
                          , ("kbName","")
                          , ("title", TL.fromStrict $ getPageName $ kbDocCanonicalName kbDoc)
                          , ("fbtype", "") -- /people/profession,/base/tagit/concept
                          , ("kbType", "")
                          , ("srcInlinks", TL.intercalate " " $ map (TL.fromStrict . getPageName) $ HM.keys $ inLinkCounts inlinkInfo) --"Medical_credentials United_States_Public_Health_Service_Commissioned_Corps University_College_of_Northern_Denmark Home_care"
                          ]
        phrase x = (TL.fromStrict x, Galago.DoNotTokenize)
        naturalText x = (TL.fromStrict x, Galago.DoTokenize)
        repeated :: HM.HashMap a Int -> [a]
        repeated = foldMap f . HM.toList
          where f (x,n) = replicate n x
    in Galago.Document { docId       = Galago.DocumentId $ T.pack $ unpackPageId $ kbDocPageId kbDoc
                       , docMetadata = fmap TL.encodeUtf8 meta
                       , docFields   = M.fromList
                          [ ("title",                [naturalText $ getPageName $ kbDocCanonicalName kbDoc])
                          , ("title-exact",          [phrase $ getPageName $ kbDocCanonicalName kbDoc])
                          , ("category",             map phrase $ kbDocCategories kbDoc)
                          , ("lead",                 [naturalText $ T.unlines $ kbDocLeadText kbDoc])
                          , ("anchor",               map naturalText $ repeated $ anchorCount $ inlinkInfo)
                          , ("anchor-exact",         map phrase $ repeated $ anchorCount $ inlinkInfo)
                          , ("disambiguation",       map (naturalText . getPageName) $ repeated $ disambiguationCount $ inlinkInfo)
                          , ("disambiguation-exact", map (phrase . getPageName) $ repeated $ disambiguationCount $ inlinkInfo)
                          , ("redirect",             map (naturalText . getPageName) $ repeated $ redirectCount $ inlinkInfo )
                          , ("redirect-exact",       map (phrase . getPageName) $ repeated $ redirectCount $ inlinkInfo )
                          , ("mentions",             map phrase $ kbDocOutMentions kbDoc) -- TODO
                          , ("kblinks",              map (phrase . getPageName) $ kbDocOutLinks kbDoc)
                          , ("wikititle",            map (phrase . T.replace "%20" "_" . getPageName) $ kbDocOutLinks kbDoc)
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

