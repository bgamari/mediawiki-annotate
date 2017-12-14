{-# LANGUAGE OverloadedStrings #-}

module CAR.FillMetadata.Tests where

import qualified Data.Text as T

import Test.Tasty
import Test.Tasty.Silver

import CAR.Types
import CAR.Types.AST.Pretty
import CAR.FillMetadata

datapath :: FilePath
datapath = "./tests/output/test.cbor"


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
    ]
    where prettyPrintGenPages = T.pack . unlines . map (prettyPage withLink) . snd




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
                   , pageMetadata = emptyPageMetadata  {pagemetaType = RedirectPage ( pageid2 )}
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
    prov =  Provenance { wikiDumpDate    = "gen"
                       , wikiSite         = siteId
                       , dataReleaseName  = "gen"
                       , comments         = ""
                       , toolsCommit      = "gen"
                       }