{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}


module GraphExpansionExperiments where

import Control.DeepSeq
import Data.Foldable
import Data.Monoid
import Data.List (intercalate)
import GHC.Generics
import Data.Tuple

import qualified Control.Foldl as Foldl
import Data.Bifunctor
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Aeson
import Numeric.Log (Log)

import CAR.Types
import qualified CAR.KnowledgeBase as KB

import CAR.Retrieve
import SimplIR.TopK
import EdgeDocCorpus


mapKeys :: (Hashable k1, Eq k1, Hashable k2, Eq k2) => (k1 -> k2) -> HM.HashMap k1 v -> HM.HashMap k2 v
mapKeys f = HM.fromList . map (first f) . HM.toList

unionsWith :: (Hashable k, Eq k) => (v -> v -> v) -> [HM.HashMap k v] -> HM.HashMap k v
unionsWith f = foldl' (HM.unionWith f) mempty

data QueryDoc = QueryDoc { queryDocQueryId :: PageId
                         , queryDocQueryText :: T.Text
                         , queryDocLeadEntities ::  HS.HashSet PageId
                         }
           deriving (Show, Generic)
instance FromJSON QueryDoc
instance ToJSON QueryDoc

data QueryDocList = QueryDocList { queryDocListContent :: [QueryDoc]}
           deriving Generic

instance FromJSON QueryDocList
instance ToJSON QueryDocList

pagesToLeadEntities :: (PageId -> PageId) -> [Page] ->  [QueryDoc]
pagesToLeadEntities resolveRedirect pages  =
        map (\page -> let kbDoc = KB.pageToKbDoc inlinkCounts page
                      in QueryDoc { queryDocQueryId        = KB.kbDocPageId kbDoc
                                  , queryDocQueryText      = getPageName $ pageName page
                                  , queryDocLeadEntities   = HS.fromList $ fmap (resolveRedirect . pageNameToId) $ KB.kbDocOutLinks kbDoc
                                  }
            )
        $ pages
      where
        inlinkInfo   = KB.collectInlinkInfo pages
        inlinkCounts = KB.resolveRedirects inlinkInfo

queryDocRawTerms :: QueryDoc -> [Term]
queryDocRawTerms = textToTokens' . queryDocQueryText

data GraphStats = GraphStats { nNodes, nEdges :: !Int }
                deriving (Show)



-- ----------------------------------------------------------------------

data EdgeDocWithScores = EdgeDocWithScores { withScoreEdgeDoc   :: EdgeDoc
                                           , withScoreCount     :: Int
                                           , withScoreScore     :: Log Double
                                           , withScoreRank      :: Int
                                           }
           deriving (Show, Generic)
instance NFData EdgeDocWithScores


rankNormDocs :: RetrievalResult EdgeDoc -> Int -> Int ->  [EdgeDocWithScores]
rankNormDocs retrievalResults normRank cutoffRank =
    let fromEntry (Entry b a) = (a, b)
        rankedEdgeDocs :: [(EdgeDoc, Log Double)]
        rankedEdgeDocs = map fromEntry
                         $ Foldl.fold (topK cutoffRank)
                         $ map (uncurry $ flip Entry)
                         $ retrievalResults
        (_, normScore)
          | length rankedEdgeDocs > normRank  = rankedEdgeDocs !! normRank
          | not (null rankedEdgeDocs)         = last rankedEdgeDocs
          | otherwise                         = error "rankNormDocs: ranking empty"

        cutRankedEdgeDocs =  fmap (\(rank, (edgeDoc, score))  -> (EdgeDocWithScores edgeDoc 1 (exp (score - normScore)) (rank)))
                           $ zip [1::Int ..]
                           $ rankedEdgeDocs
    in cutRankedEdgeDocs

--
-- filterGraphByTopNGraphEdges :: RetrievalFunction EdgeDoc
--                             -> Int
--                             -> [Term]
--                             -> HM.HashMap PageId [EdgeDocWithScores]
-- filterGraphByTopNGraphEdges retrieveDocs topN query =
--         let edges :: [EdgeDocWithScores]
--             edges  = rankNormDocs retrieveDocs topN topN query
--         in HM.fromListWith (++) $ foldMap groupByEntity $ edges
--   where groupByEntity :: EdgeDocWithScores -> [(PageId, [EdgeDocWithScores])]
--         groupByEntity ele@(EdgeDocWithScores edgeDoc _ _ _) =
--                   [ (entity, [ele])
--                   | entity <- edgeDocNeighbors $ edgeDoc]


filterGraphByTopNGraphEdges :: RetrievalResult EdgeDoc
                            -> Int
                            -> HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTopNGraphEdges retrievalResult topN =
        HM.fromListWith (++) $ foldMap groupByEntity $ edges
      where edges :: [EdgeDocWithScores]
            edges = rankNormDocs retrievalResult topN topN
            groupByEntity :: EdgeDocWithScores -> [(PageId, [EdgeDocWithScores])]
            groupByEntity ele@(EdgeDocWithScores edgeDoc _ _ _) =
                      [ (entity, [ele])
                      | entity <- edgeDocNeighbors $ edgeDoc]


instance NFData GraphNames
instance NFData WeightingNames
instance NFData GraphRankingNames

{-
filterGraphByTop5NodeEdges :: RetrievalFunction -> [Term] -> [EdgeDoc] ->  HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTop5NodeEdges retrieveDocs query edgeDocs =
  let perNodeEdges :: HM.HashMap PageId [EdgeDoc]
      perNodeEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs
      perNodeFilteredEdges :: HM.HashMap PageId [EdgeDocWithScores]
      perNodeFilteredEdges =  fmap filterNode perNodeEdges
   in perNodeFilteredEdges


  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]
        filterNode :: [EdgeDoc] -> [EdgeDocWithScores]
        filterNode edgeDocs' =
            rankNormDocs retrieveDocs 5 5 query edgeDocs'
    -}


noFilterTwice :: [EdgeDoc] ->  HM.HashMap PageId (HM.HashMap PageId [EdgeDoc])
noFilterTwice edgeDocs =
  let perSourceEdges :: HM.HashMap PageId [EdgeDoc]
      perSourceEdges = HM.fromListWith (++) $ foldMap groupByIncidentEntity edgeDocs
      perTargetEdges = fmap (\edgeDocs' -> HM.fromListWith (++) $ foldMap groupByIncidentEntity edgeDocs' ) $ perSourceEdges
  in perTargetEdges
  where groupByIncidentEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByIncidentEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]



-- mapKeys f == HM.fromList . map (first f) . HM.toList
-- unionsWith f = foldl' (HM.unionWith f) mempty

onlySymmetricEdges :: [EdgeDoc] -> [EdgeDoc]
onlySymmetricEdges edgeDocs =
  let fromTo :: HM.HashMap (PageId, PageId) [EdgeDoc]
      fromTo = unionsWith (++)
          [ HM.singleton (edgeDocArticleId edoc, neigh) [edoc]
          | edoc <- edgeDocs
          , neigh <- edgeDocNeighbors edoc
          ]
      fromToFiltered :: HM.HashMap (PageId, PageId) [EdgeDoc]
      fromToFiltered = HM.intersectionWith (++) fromTo (mapKeys swap fromTo)

      filteredIncidentNodes :: HM.HashMap EdgeDoc (HS.HashSet PageId)
      filteredIncidentNodes = unionsWith mappend
          [ HM.singleton edoc (HS.fromList [a,b])
          | ((a,b), edocs) <- HM.toList fromToFiltered
          , edoc <- edocs
          ]

      edgeDocs' :: [EdgeDoc]
      edgeDocs' =
          [ edoc { edgeDocNeighbors = HS.toList v }
          | (edoc, v) <- HM.toList filteredIncidentNodes
          ]

  in edgeDocs'


randomFilter :: Int -> [EdgeDoc] ->  HM.HashMap PageId (HM.HashMap PageId [EdgeDoc])
randomFilter topN edgeDocs =
  let edgeDocs' = take topN $ HS.toList $ HS.fromList edgeDocs -- rely on HashSet randomizing the list
      perSourceEdges :: HM.HashMap PageId [EdgeDoc]
      perSourceEdges = HM.fromListWith (++) $ foldMap groupByEntity edgeDocs'
      perTargetEdges = fmap (HM.fromListWith (++) . foldMap groupByEntity) $ perSourceEdges
  in perTargetEdges
  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]


accumulateEdgeWeights :: forall w. Num w
                      => HM.HashMap PageId [EdgeDocWithScores]
                      -> (EdgeDocWithScores -> w)
                      -> HS.HashSet PageId
                      -> HM.HashMap PageId (HM.HashMap PageId w)
accumulateEdgeWeights sourceToEdgeDocsWithScores by seeds=
     (HM.mapWithKey countEdgeDocs sourceToEdgeDocsWithScores) <> fmap (const mempty) (HS.toMap seeds)
  where countEdgeDocs :: PageId -> [EdgeDocWithScores] -> HM.HashMap PageId w
        countEdgeDocs sourceNode edgeDocsWithScores =
            HM.fromListWith (+)
              [ (targetNode, by edgeDocsWithScore)
              | edgeDocsWithScore <- edgeDocsWithScores
              , targetNode <- edgeDocNeighbors $ withScoreEdgeDoc $ edgeDocsWithScore
              , targetNode /= sourceNode  -- we only compute the destinations, this will be added to the source's outedges
              ]



-- Marginalized over second argument in edges, e.g. Map source (Map target weight_{st}) -> Map source weight_{s*}
marginalizeEdges :: HM.HashMap PageId (HM.HashMap PageId Double) -> [(PageId, Double)]
marginalizeEdges graph =
    HM.toList $ fmap marginalizeMap $ graph
  where marginalizeMap :: HM.HashMap PageId Double -> Double
        marginalizeMap = sum . fmap snd . HM.toList


-- ----------------------------------------------------------------------

data GraphNames = Top5PerNode | Top100PerGraph | SimpleGraph | RandomGraph | Random2000Graph  | Top10PerGraph | Top50PerGraph | Top200PerGraph | Top2000PerGraph | Top20000PerGraph
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data WeightingNames = Count | Binary | Score | RecipRank | LinearRank| BucketRank
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data GraphRankingNames = PageRank | PersPageRank | AttriRank | ShortPath | MargEdges
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data EdgeFilteringNames = Unfiltered | BidiFiltered
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data RetrievalFun = Bm25 | Ql | NoIr
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data Method = Method GraphNames EdgeFilteringNames WeightingNames GraphRankingNames RetrievalFun
            | CandidateSet
    deriving ( Ord, Eq, Generic)
instance Show Method where
    show = showMethodName
showMethodName:: Method -> String
showMethodName (Method a b c d e) = intercalate "-" [show a, show b, show c, show d, show e]
showMethodName (CandidateSet ) = "CandidateSet"

allMethods :: [Method]
allMethods = [ Method gName eName wName rName irName
             | gName <- [minBound :: GraphNames .. maxBound ]
             , eName <- [minBound :: EdgeFilteringNames .. maxBound]
             , wName <- [minBound :: WeightingNames .. maxBound]
             , rName <- [minBound :: GraphRankingNames .. maxBound]
             , irName <- [minBound :: RetrievalFun .. maxBound]
             ] ++ [CandidateSet]

baseMethods :: [Method]
baseMethods = [ Method gName eName wName rName irName
             | gName <- [SimpleGraph]
             , eName <- [Unfiltered]
             , wName <- [Count]
             , rName <- [PersPageRank, MargEdges, PageRank, ShortPath] -- AttriRank,
             , irName <- [NoIr]
             ]

coreMethods :: [Method]
coreMethods = [ Method gName eName wName rName irName
             | gName <- [Top100PerGraph, Top2000PerGraph, Top20000PerGraph, RandomGraph ]
             , eName <- [Unfiltered]
             , wName <- [Count, Score]
             , rName <- [PersPageRank, MargEdges, PageRank, ShortPath] -- AttriRank,
             , irName <- [minBound :: RetrievalFun .. maxBound]
             ]

prio0Methods :: [Method]
prio0Methods = [Method gName eName wName rName irName
               | gName <- [Top2000PerGraph]
               , eName <- [Unfiltered]
               , wName <- [Score]
               , rName <- [MargEdges, PageRank]
               , irName <- [Ql]
               ]

prio1Methods :: [Method]
prio1Methods = [ Method gName eName wName rName irName
                | gName <- [Top100PerGraph, Top2000PerGraph, RandomGraph ]
                , eName <- [Unfiltered]
                , wName <- [Count, Score]
                , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                , irName <- [Ql]
                ]

prio2Methods :: [Method]
prio2Methods = [ Method gName eName wName rName irName
                | gName <- [SimpleGraph, Top5PerNode ]
                , eName <- [Unfiltered]
                , wName <- [Count, Score]
                , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                , irName <- [Ql]
                ]
prio3Methods :: [Method]
prio3Methods = [ Method gName eName wName rName irName
                | gName <- [Top10PerGraph, Top50PerGraph, Top200PerGraph ]
                , eName <- [Unfiltered]
                , wName <- [Count, Score]
                , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                , irName <- [Ql]
                ]

prio4Methods :: [Method]
prio4Methods = [ Method gName eName wName rName irName
                | gName <- [Top100PerGraph, Top2000PerGraph, Random2000Graph, Top10PerGraph, Top50PerGraph, Top200PerGraph]
                , eName <- [Unfiltered]
                , wName <- [Count, Score, RecipRank, LinearRank, BucketRank]
                , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                , irName <- [Ql]
                ]

fix1Methods :: [Method]
fix1Methods = [ Method gName eName wName rName irName
              | gName <- [Top100PerGraph, Top2000PerGraph ]
              , eName <- [Unfiltered]
              , wName <- [Count, Score]
              , rName <- [AttriRank]
              , irName <- [Ql]
              ]

fix2Methods :: [Method]
fix2Methods = [ Method gName eName wName rName irName
              | gName <- [Random2000Graph ]
              , eName <- [Unfiltered]
              , wName <- [Count, Score]
              , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
              , irName <- [Ql]
              ]


testMethods :: [Method]
testMethods = [ Method gName eName wName rName irName
              | gName <- [minBound :: GraphNames .. maxBound]
              , eName <- [Unfiltered]
              , wName <- [Binary, Score]
              , rName <- [PersPageRank, MargEdges, AttriRank, ShortPath]
              , irName <- [Ql]
              ]

topNPerGraphMethods :: [Method]
topNPerGraphMethods = [ Method gName eName wName rName irName
                       | gName <- [Top100PerGraph, Top10PerGraph, Top50PerGraph, Top200PerGraph, Top2000PerGraph]
                       , eName <- [minBound :: EdgeFilteringNames .. maxBound]
                       , wName <- [Count, Score, RecipRank]
                       , rName <- [PersPageRank, PageRank, ShortPath, MargEdges]
                       , irName <- [Ql]
                       ]


-- instance NFData GraphNames
-- instance NFData WeightingNames
-- instance NFData GraphRankingNames


type RankingFunction = forall elem. [Term] -> [(elem, T.Text)] -> [(elem, Double)]
type RetrievalFunction elem = [Term] -> RetrievalResult elem
type RetrievalResult elem = [(elem, Log Double)]
