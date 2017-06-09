{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Monoid hiding (All, Any)
import System.IO

import Options.Applicative
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe

import CAR.Types
import SimplIR.Galago as Galago

import CAR.KnowledgeBase


opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")


wikiAnchorStopwords = [ "more....", "wikipedia article", "source: wikipedia", "here", "wiki", "wikipedia"]
wikiAnchorStopphrases = [ "wikipedia the free encyclopedia", "en.wikipedia.org", "full article at wikipedia.org"
                        , "wikipedia the free encyclopedia", "wikipedia, the free encyclopedia"
                        , "http://en.wikipedia.org/wiki/", "wikipedia"]


skeletonToXml :: [PageSkeleton] -> TB.Builder
skeletonToXml list =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><articles loadtime=\"0 sec\" rendertime=\"0.06 sec\" totaltime=\"0.06 sec\"><article> "
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
    skelToXml (Image {} )  = mempty

    bodyToXml :: ParaBody -> TB.Builder
    bodyToXml (ParaText text) = " " <> TB.fromText (T.replace "\n" " " text) <> " "
    bodyToXml (ParaLink link) =
        " <link><target> "<>TB.fromText (getPageName $ linkTarget link)<>" </target></link> "



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
    let inlinkInfo   = collectInlinkInfo pages
        inlinkCounts = resolveRedirects inlinkInfo
        inlinkTotals = mconcat $ HM.elems inlinkCounts

    withFile outputFile WriteMode $ \h ->
        BSL.hPutStr h $ Galago.toWarc
            $ map (toGalagoDoc inlinkCounts inlinkTotals)
            $ map (pageToKbDoc inlinkCounts)
            $ filter (\p -> not $ pageName p `HS.member` redirectPages inlinkInfo)
            $ pages

