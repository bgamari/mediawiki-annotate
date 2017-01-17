{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Data.Monoid hiding (All, Any)
import System.IO

import Options.Applicative
-- import qualified Data.DList as DList
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Bifunctor

import CAR.Utils
import CAR.Types



opts :: Parser (FilePath, FilePath)
opts =
    (,)
    <$> argument str (help "input file" <> metavar "ANNOTATIONS FILE")
    <*> option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")



data KbDoc = KbDoc { kbDocParagraphId :: ParagraphId
                   , kbDocArticleId :: PageId
                   , kbDocSourceEntityId :: PageId
                   , kbDocOutlinkIds ::  [PageId]
                   }
           deriving Show

type GraphEdges = HM.HashMap PageId [KbDoc]
type Graph = HM.HashMap PageId (HS.HashSet PageId)

transformContent :: Page -> [KbDoc]
transformContent (Page pageName' pageId pageSkeleta) =
    foldMap go pageSkeleta
  where
    pageName = normTargetPageName pageName'
    go :: PageSkeleton -> [KbDoc]
    go (Section heading _ children) =
       concatMap go  children
    go (Para paragraph) =
      [convertPara paragraph ]

    convertPara :: Paragraph -> KbDoc
    convertPara paragraph =
      let
        kbDocParagraphId    = paraId $ paragraph
        kbDocArticleId      = pageId
        kbDocSourceEntityId = pageNameToId pageName
        kbDocOutlinks       = fmap (first normTargetPageName) $ paraLinks $ paragraph
        kbDocOutlinkIds     = fmap (pageNameToId . fst) $ kbDocOutlinks
      in KbDoc {..}

main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
--     withFile outputFile WriteMode $ \h ->
--         BSL.hPutStr h $ Galago.toWarc
--             $ map toGalagoDoc
    let graphEdges :: GraphEdges
        graphEdges = HM.fromListWith (++) $ foldMap hashEdgeNodes
              $ foldMap ( dropKbDocsNoLinks . transformContent)
              $ pages
    let graph :: Graph
        graph = fmap (foldMap (\kbDoc -> HS.fromList $ [kbDocSourceEntityId $ kbDoc] ++ (kbDocOutlinkIds $kbDoc))) $ graphEdges

    let seeds :: HS.HashSet PageId
        seeds = HS.fromList [ "Thorium", "Plutonium",  "Burnup"]
    print $ expandNodesK graph seeds 3
  where
    expandNodes :: Graph -> HS.HashSet PageId -> HS.HashSet PageId
    expandNodes graph seeds =
      seeds <> foldMap (fromMaybe mempty . (`HM.lookup` graph)) seeds

    expandNodesK graph seeds k =
      iterate (expandNodes graph) seeds !! k

    dropKbDocsNoLinks :: [KbDoc] -> [KbDoc]
    dropKbDocsNoLinks =
        filter (\kbDoc -> not ( null (kbDocOutlinkIds $kbDoc)))

    hashEdgeNodes :: KbDoc -> [(PageId, [KbDoc])]
    hashEdgeNodes kbDoc =
      [(kbDocSourceEntityId $kbDoc, [kbDoc])] ++ [(target, [kbDoc])  | target <- kbDocOutlinkIds $kbDoc]