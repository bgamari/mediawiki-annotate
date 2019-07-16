{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Maybe

import qualified Data.HashSet as HS
import qualified Data.Text as T
import Options.Applicative hiding (action)

import qualified CAR.TocFile as TocFile
import CAR.Types
import CAR.ToolVersion
import qualified SimplIR.Format.QRel as QRel
import SimplIR.Format.QRel


opts :: Parser (IO ())
opts = subparser
    $  cmd "paragraph-ids-in-corpus"      paragraphIdsInCorpus
  where
    cmd name action = command name (info (helper <*> action) fullDesc)

    paragraphIdsInCorpus =
        f <$> argument str (help "input paragraph file" <> metavar "FILE")
          <*> option str (short 'q' <> long "qrels" <> metavar "FILE")
      where
        f :: FilePath -> FilePath -> IO ()
        f inputFile qrelFile = do
            qrels <- QRel.readQRel @QRel.IsRelevant qrelFile
            let paraIds :: [ParagraphId]
                paraIds = HS.toList $ HS.fromList $ fmap (packParagraphId . T.unpack . documentName) qrels

            paragraphs <- do paras <- TocFile.open $ TocFile.IndexedCborPath inputFile
                                      :: IO (TocFile.IndexedCbor ParagraphId Paragraph )
                             return $ mapMaybe (`TocFile.lookup` paras) paraIds
            let numParaIds = length paraIds
                numParasFound = length paragraphs

            putStrLn $ "numParaIds = "<> show numParaIds <> "\n numParasFound = " <> show numParasFound



-- readFilteredPages :: S.Set PageName    -- ^ set of page names to read
--                   -> S.Set PageId    -- ^ set of page names to read
--                   -> FilePath          -- ^ pages or outlines file
--                   -> IO [Page]
-- readFilteredPages pageNames pageIds inputFile
--   | S.null pageNames && S.null pageIds  =
--      readPagesOrOutlinesAsPages inputFile
--   | otherwise = do
--      siteId <- wikiSite . fst <$> readPagesOrOutlinesAsPagesWithProvenance inputFile
--      anns <- CAR.openAnnotations inputFile
--      let pageIds' = pageIds <> (S.map (pageNameToId siteId) $ pageNames)
--      return $ mapMaybe (`CAR.lookupPage` anns) ( S.toList  pageIds')
--
-- pagesFromFile :: Parser (IO [Page])
-- pagesFromFile =
--     f <$> argument str (help "input file" <> metavar "FILE")
--       <*> fmap S.fromList (many (option  (packPageName  <$> str) (short 'p' <> long "page" <> metavar "PAGE NAME" <> help "Page name to dump or nothing to dump all")))
--       <*> fmap S.fromList (many (option  (packPageId  <$> str) (short 'P' <> long "pageid" <> metavar "PAGE ID " <> help "Page id to dump or nothing to dump all")))
--       <*> (many (option (flip pageNameToId <$> (packPageName <$> str)) (long "target" <> short 't' <> help "dump only pages with links to this target page name (and the page itself)")))
--       <*> ( HS.fromList <$> many (option (packPageId <$> str) (long "targetids" <> short 'T'  <> help "dump only pages with links to this target page id (and the page itself)")))
--       <*> ( HS.fromList <$> many (option (packPageName <$> str)  (long "redirect" <> short 'r' <> help "dump only pages with redirects from this page name")))
--   where
--     f :: FilePath -> S.Set PageName -> S.Set PageId -> [SiteId -> PageId] -> HS.HashSet PageId -> HS.HashSet PageName -> IO [Page]
--     f inputFile pageNames pageIds targetPageIds1 targetPageIds2 redirectPageNames = do
--         siteId <- wikiSite . fst <$> readPagesOrOutlinesAsPagesWithProvenance inputFile
--         pages <- readFilteredPages pageNames pageIds inputFile
--         return $ filter (redirectTargets redirectPageNames)
--                $ filter (searchTargets (targetPageIds siteId)) pages
--       where targetPageIds siteId =
--                 HS.fromList (map ($ siteId) targetPageIds1)
--                 `HS.union` targetPageIds2
--             searchTargets targets page =
--                 if | HS.null targets -> True
--                    | (pageId page) `HS.member` targets -> True
--                    | otherwise     -> let pageTargets = HS.fromList (pageLinkTargetIds page)
--                                       in any (  `HS.member` pageTargets) targets
--             redirectTargets :: HS.HashSet PageName -> Page -> Bool
--             redirectTargets redirects page =
--                 if | HS.null redirects -> True
--                    | Just pageRedirects <- getMetadata _RedirectNames (pageMetadata page)
--                         -> let pageRedirectSet = HS.fromList pageRedirects
--                            in any (  `HS.member` pageRedirectSet) redirects
--                    | otherwise -> False
--

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) mempty
