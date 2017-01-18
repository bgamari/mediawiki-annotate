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
import Data.Foldable
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

--
-- newtype Graph' = Graph' HashSet Edge'
--                | Graph' HashSet Edges'
-- newtype Edge' =  Edge' SourceNode  TargetNode Weight
--
-- newtype Edges' = Edges' SourceNode (Map TargetNode Weight)


newtype OutwardEdges = OutwardEdges (HM.HashMap PageId Int)     -- ^ a set of outward edges and their weights
        deriving Show
data Edges = Edges PageId OutwardEdges  -- ^ sourceNode and its outward edges
        deriving Show
newtype Subgraph = Subgraph [Edges]
        deriving Show


singleGraphEdge :: PageId -> OutwardEdges
singleGraphEdge target = OutwardEdges $ HM.singleton target 1

type Graph = HM.HashMap PageId OutwardEdges

instance Monoid OutwardEdges where
    mempty = OutwardEdges mempty
    OutwardEdges a `mappend` OutwardEdges b = OutwardEdges (HM.unionWith (+) a b)


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



countEdges :: [KbDoc] -> OutwardEdges
countEdges kbDocs =
      foldMap (singleGraphEdge . kbDocSourceEntityId) kbDocs
   <> foldMap (foldMap singleGraphEdge . kbDocOutlinkIds) kbDocs


main :: IO ()
main = do
    (inputFile, outputFile) <- execParser $ info (helper <*> opts) mempty
    pages <- decodeCborList <$> BSL.readFile inputFile
--     withFile outputFile WriteMode $ \h ->
--         BSL.hPutStr h $ Galago.toWarc
--             $ map toGalagoDoc
    let outwardEdges :: HM.HashMap PageId [KbDoc]
        outwardEdges = HM.fromListWith (++)
                   $ foldMap hashEdgeNodes
                   $ foldMap (dropKbDocsNoLinks . transformContent)
                   $ pages
    let graph :: Graph
        graph = fmap  countEdges $ outwardEdges

    let seeds :: [PageId]
        seeds =  [ "Thorium", "Plutonium",  "Burnup"]
    print $ expandNodes graph seeds
  where
    lookupNeighbors :: Graph -> PageId -> OutwardEdges
    lookupNeighbors graph source =
        fromMaybe mempty $ HM.lookup source graph


    expandNodes :: Graph -> [PageId] -> Subgraph
    expandNodes graph seeds =
       Subgraph $ fmap (\seed -> Edges seed (lookupNeighbors graph seed)) seeds

--     expandNodes graph seeds =
--       seeds <> foldMap (fromMaybe mempty . (`HM.lookup` graph)) seeds
--       seeds `UnionWith +` foldMap
--
--     expandNodesK graph seeds k =
--       iterate (expandNodes graph) seeds !! k

    dropKbDocsNoLinks :: [KbDoc] -> [KbDoc]
    dropKbDocsNoLinks =
        filter (\kbDoc -> not ( null (kbDocOutlinkIds $kbDoc)))

    hashEdgeNodes :: KbDoc -> [(PageId, [KbDoc])]
    hashEdgeNodes kbDoc =
      [(kbDocSourceEntityId $kbDoc, [kbDoc])] ++ [(target, [kbDoc])  | target <- kbDocOutlinkIds $kbDoc]