{-# LANGUAGE OverloadedStrings #-}

module CAR.FillMetadata.Tests where

import qualified Data.Text as T
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Semigroup
import Data.Tagged

import Test.Tasty
import Test.Tasty.Silver
import Test.Tasty.HUnit
import Test.Tasty.Options


import CAR.Types
import CAR.Types.AST.Pretty
import CAR.FillMetadata
import CAR.Utils


datapath :: FilePath
datapath = "./tests/output/test.cbor"


newtype LargeTestDataPath = LargeTestDataPath FilePath

instance IsOption LargeTestDataPath where
    defaultValue = LargeTestDataPath datapath
    parseValue = Just . LargeTestDataPath
    optionName = Tagged "large-test-data-path"
    optionHelp = Tagged "filepath to pages CBOR used for testing"


tests :: TestTree
tests =
    testGroup "FillMetadata"
    [
     goldenVsAction
        "verify test data generation"
        "tests/output/fill-metadata/verify-test-data.golden"
        (readPagesFileWithProvenance datapath)
        prettyPrintGenPages
     , goldenVsAction
        "resolve redirect stage"
        "tests/output/fill-metadata/resolve-redirect.golden"
        (stageResolveRedirect datapath)
        prettyPrintGenPages
     , askOption $ \(LargeTestDataPath datapath) ->
       testCase
        "check no redirects in link targets"
        (checkRedirects datapath)
    ]
    where prettyPrintGenPages = T.pack . unlines . map (prettyPage withLink) . snd



checkRedirects :: FilePath -> IO ()
checkRedirects inputPath = do
    (_, pages) <- readPagesFileWithProvenance inputPath
    let redirectPageNames :: HS.HashSet PageName
        redirectPageNames = HS.fromList
                          $ fmap pageName
                          $ filter isRedirect
                          $ pages
        articleLinkTargets :: HM.HashMap PageName PageName
        articleLinkTargets =  HM.fromList
                           $ [ (link , pageName page)
                             | page <- pages
                             , isArticle page
                             , link <- pageLinkTargets page]


        linkTargetsToRedirects :: HM.HashMap PageName PageName
        linkTargetsToRedirects = articleLinkTargets `HM.intersection` (HS.toMap redirectPageNames)


    assertBool ("The following link targets point to redirect pages: " <> show linkTargetsToRedirects)
              $ (HM.null linkTargetsToRedirects)

  where
    isRedirect :: Page -> Bool
    isRedirect = isJust . pageRedirect
    isArticle =  pageIsArticle


siteId = "enwiki"

testPages :: [Page]
testPages = [page1, page2, pageR]
  where
     pagename1 = "page1"
     pageid1 = pageNameToId siteId pagename1
     pagenameR = "redirect2"
     pageidR = pageNameToId siteId pagenameR
     pagename2 = "page2"
     pageid2 = pageNameToId siteId pagename2

     page1 =  Page { pageName = pagename1
                   , pageId = pageid1
                   , pageMetadata = emptyPageMetadata
                   , pageSkeleton = [
                         Para (Paragraph (ParagraphId "para1") [
                                ParaLink (Link { linkTarget = pagenameR
                                               , linkSection = Nothing
                                               , linkTargetId = pageidR
                                               , linkAnchor = "anchor text to page R"})
                                ] )
                     ]
                   }

     pageR =  Page { pageName = pagenameR
                   , pageId = pageidR
                   , pageType = RedirectPage $ Link pagename2 Nothing pageid2 "hello"
                   , pageMetadata = emptyPageMetadata
                   , pageSkeleton = [
                         Para (Paragraph (ParagraphId "paraR") [
                                  ParaText "#REDIRECT "
                                , ParaLink (Link { linkTarget = pagename2
                                                 , linkSection = Nothing
                                                 , linkTargetId = pageid2
                                                 , linkAnchor = "anchor text to page 2"})
                                ] )
                     ]
                   }

     page2 =  Page { pageName = pagename2
                   , pageId = pageid2
                   , pageType = ArticlePage
                   , pageMetadata = emptyPageMetadata
                   , pageSkeleton = [
                         Para (Paragraph (ParagraphId "para2") [
                                  ParaText "This is page 2"
                                ] )
                     ]
                   }



generateInputData :: IO ()
generateInputData =
    writeCarFile datapath prov testPages
  where
    prov =  Provenance { siteProvenances  =
                           [ SiteProvenance { provSiteId = siteId
                                            , language = "en-US"
                                            , sourceName = "gen"
                                            , siteComments = []
                                            }
                           ]
                       , dataReleaseName  = "gen"
                       , comments         = []
                       , transforms       = []
                       }
