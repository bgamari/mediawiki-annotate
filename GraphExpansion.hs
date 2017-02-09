{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}

module GraphExpansion where

import Control.DeepSeq
import Data.Monoid hiding (All, Any)
import Data.Foldable
import Data.Maybe
import Data.Bifunctor
import Data.Function
import GHC.Generics

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Dijkstra
import PageRank
import CAR.Utils
import CAR.Types

data EdgeDoc = EdgeDoc { edgeDocParagraphId     :: ParagraphId
                       , edgeDocArticleId       :: PageId
                       , edgeDocNeighbors       :: [PageId]
                       , edgeDocContent         :: T.Text
                       }
           deriving (Show, Generic)

instance NFData EdgeDoc

instance Eq EdgeDoc where
    (==) = (==) `on` edgeDocParagraphId

instance Hashable EdgeDoc where
    hashWithSalt salt = hashWithSalt salt . edgeDocParagraphId

transformContent :: Page -> [EdgeDoc]
transformContent (Page pageName pageId pageSkeleta) =
    foldMap (go mempty) pageSkeleta
  where
    go :: [SectionHeading] -> PageSkeleton -> [EdgeDoc]
    go headings (Section heading _ children) =
        concatMap (go (heading : headings)) $ children
    go headings (Para paragraph) =
      [convertPara paragraph headings]

    convertPara :: Paragraph -> [SectionHeading] -> EdgeDoc
    convertPara paragraph headings=
      let
        edgeDocParagraphId    = paraId $ paragraph
        edgeDocArticleId      = pageId
        edgeDocNeighbors      = [pageId] ++ (fmap linkTargetId $ paraLinks $ paragraph)
        edgeDocContent        = paragraphContent paragraph headings
      in EdgeDoc {..}
      where paragraphContent :: Paragraph -> [SectionHeading] -> T.Text
            paragraphContent paragraph headings =
              (paraToText $ paragraph)
              <> (T.intercalate " " $ fmap getSectionHeading $ headings)
              <> (getPageName pageName)





dropEdgeDocsNoLinks :: [EdgeDoc] -> [EdgeDoc]
dropEdgeDocsNoLinks =
    filter (\edgeDoc -> not ( lengthOne (edgeDocNeighbors $ edgeDoc)))
  where lengthOne [_] = True   -- list with one element
        lengthOne _   = False

type UniverseGraph = HM.HashMap PageId [EdgeDoc]

edgeDocsToUniverseGraph :: [EdgeDoc] -> UniverseGraph
edgeDocsToUniverseGraph edgeDocs =
    HM.fromListWith (++)
    $ foldMap symmetrizeEdge edgeDocs
  where
    symmetrizeEdge :: EdgeDoc -> [(PageId, [EdgeDoc])]
    symmetrizeEdge edgeDoc =
           [ (target, [edgeDoc])
           | target <- edgeDocNeighbors edgeDoc]


emitEdgeDocs :: [Page] -> [EdgeDoc]
emitEdgeDocs pages =
    foldMap (dropEdgeDocsNoLinks . transformContent)
    $ pages
  where
    symmetrizeEdge :: EdgeDoc -> [(PageId, [EdgeDoc])]
    symmetrizeEdge edgeDoc =
           [ (target, [edgeDoc])
           | target <- edgeDocNeighbors edgeDoc]





-- ------------------------------------------------
type BinarySymmetricGraph = HM.HashMap PageId (HS.HashSet PageId)

universeToBinaryGraph :: UniverseGraph -> BinarySymmetricGraph
universeToBinaryGraph universeGraph =
  fmap (foldMap (\edgeDoc -> HS.fromList $ (edgeDocNeighbors $edgeDoc))) $ universeGraph


expandNodes :: BinarySymmetricGraph -> HS.HashSet PageId -> HS.HashSet PageId
expandNodes binarySymmetricGraph seeds =
  seeds <> foldMap (fromMaybe mempty . (`HM.lookup` binarySymmetricGraph)) seeds

expandNodesK binarySymmetricGraph seeds k =
  iterate (expandNodes binarySymmetricGraph) seeds !! k



-- ------------------------------------------------



lookupNeighbors :: Monoid v =>  HM.HashMap PageId v -> PageId -> v
lookupNeighbors graph node =
    fromMaybe mempty $ HM.lookup node graph


subsetOfUniverseGraph :: UniverseGraph -> HS.HashSet PageId -> UniverseGraph
subsetOfUniverseGraph universe nodeset =
    foldMap (\node -> HM.singleton node (map pruneEdges $ universe `lookupNeighbors` node) ) $ nodeset
  where
    -- Throw out neighbors not in our subgraph
    pruneEdges :: EdgeDoc -> EdgeDoc
    pruneEdges edoc = edoc { edgeDocNeighbors = filter (`HS.member` nodeset) (edgeDocNeighbors edoc) }

rankByPageRank :: Graph PageId Double -> Double -> Int -> [(PageId, Double)]
rankByPageRank graph teleport iterations =
  let pr = (!! iterations)  $ PageRank.pageRank teleport graph
      prRanking  =  PageRank.toEntries $ pr
  in prRanking

rankByPersonalizedPageRank :: Graph PageId Double -> Double -> HS.HashSet PageId -> Int -> [(PageId, Double)]
rankByPersonalizedPageRank graph teleport seeds iterations =
  let pr = (!! iterations)  $ PageRank.pageRankWithSeeds 0 teleport seeds graph
      prRanking  =  PageRank.toEntries $ pr
  in prRanking


filterEdges :: (PageId -> PageId -> Bool) -> Dijkstra.Graph PageId w -> Dijkstra.Graph PageId w
filterEdges pred (Dijkstra.Graph graph ) =
    Dijkstra.Graph $ HM.mapWithKey f $ graph
  where f :: PageId -> [(PageId, w)] -> [(PageId, w) ]
        f source =
          filter (\(target, _) -> pred source target)

rankByShortestPaths :: Dijkstra.Graph PageId (Sum Double) -> [PageId] -> [(PageId, Double)]
rankByShortestPaths graph seeds =
    let seeds' = HS.fromList seeds
        graph' = filterEdges (\s t -> not (s `HS.member` seeds' && t `HS.member` seeds'))  $ graph
        shortestPaths =  [ (n1, n2, Dijkstra.shortestPaths paths n2)
                         | n1 <- toList seeds
                         , let paths = Dijkstra.dijkstra (graph') n1
                         , n2 <- toList seeds
                         ]

        pathRanking = shortestPathsToNodeScores shortestPaths
    in pathRanking

takeMiddle :: Seq.Seq a -> Seq.Seq a
takeMiddle s =
    case Seq.viewl s of
      Seq.EmptyL  -> mempty
      _ Seq.:< s' ->
       case Seq.viewr s' of
         Seq.EmptyR   -> mempty
         s'' Seq.:> _ -> s''

shortestPathsToNodeScores :: [(PageId, PageId, [Dijkstra.Path PageId])] -> [(PageId, Double)]
shortestPathsToNodeScores paths =
    let innerPaths :: [Seq.Seq PageId]
        innerPaths = fmap takeMiddle [ Seq.fromList path
                                     | (_, _, pathList) <- paths
                                     , path <- pathList
                                     ]

        numPaths = realToFrac $ length innerPaths
    in HM.toList
     $ HM.fromListWith (+) [ (elem, 1 / numPaths)
                           | path <- innerPaths
                           , elem <- toList path
                           ]



toGraph :: HM.HashMap PageId (HM.HashMap PageId Double) -> Graph PageId Double
toGraph graph =
    Graph $ fmap HM.toList graph
