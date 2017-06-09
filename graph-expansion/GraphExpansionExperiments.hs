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
import Data.List (intercalate)
import GHC.Generics
import Data.Tuple

import Data.Bifunctor
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Aeson

import CAR.Types
import qualified CAR.KnowledgeBase as KB

import CAR.Retrieve
import GraphExpansion


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

pagesToLeadEntities :: [Page] -> [QueryDoc]
pagesToLeadEntities pages =
        map (\page -> let kbDoc = KB.pageToKbDoc inlinkCounts page
                      in QueryDoc { queryDocQueryId        = KB.kbDocPageId kbDoc
                                  , queryDocQueryText      = getPageName $ pageName page
                                  , queryDocLeadEntities   = HS.fromList $ fmap pageNameToId $ KB.kbDocOutLinks kbDoc
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
                                           , withScoreScore     :: Double
                                           , withScoreRank      :: Int
                                           }
           deriving (Show, Generic)
instance NFData EdgeDocWithScores


rankNormDocs :: RankingFunction -> Int -> Int -> [Term] -> [EdgeDoc] -> [EdgeDocWithScores]
rankNormDocs rankDocs normRank cutoffRank query edgeDocs =
    let rankedEdgeDocs :: [(EdgeDoc, Double)]
        rankedEdgeDocs = take cutoffRank
                         $ rankDocs query
                         $ fmap (\edgeDoc -> (edgeDoc, edgeDocContent $ edgeDoc))
                         $ edgeDocs
        (_, normScore)
          | length rankedEdgeDocs > normRank  = rankedEdgeDocs !! normRank
          | not (null rankedEdgeDocs)         = last rankedEdgeDocs
          | otherwise                         = error "rankNormDocs: ranking empty"

        cutRankedEdgeDocs =  fmap (\(rank, (edgeDoc, score))  -> (EdgeDocWithScores edgeDoc 1 (exp (score - normScore)) (rank)))
                           $ zip [1::Int ..]
                           $ rankedEdgeDocs
    in cutRankedEdgeDocs


filterGraphByTopNGraphEdges :: RankingFunction -> Int ->  [Term] -> [EdgeDoc] ->   HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTopNGraphEdges rankDocs topN query edgeDocs  =
        let edges :: [EdgeDocWithScores]
            edges  = rankNormDocs rankDocs topN topN query edgeDocs
        in HM.fromListWith (++) $ foldMap groupByEntity $ edges
  where groupByEntity :: EdgeDocWithScores -> [(PageId, [EdgeDocWithScores])]
        groupByEntity elem@(EdgeDocWithScores edgeDoc _ _ _) =
                  [ (entity, [elem])
                  | entity <- edgeDocNeighbors $ edgeDoc]


instance NFData GraphNames
instance NFData WeightingNames
instance NFData GraphRankingNames


filterGraphByTop5NodeEdges :: RankingFunction -> [Term] -> [EdgeDoc] ->  HM.HashMap PageId [EdgeDocWithScores]
filterGraphByTop5NodeEdges rankDocs query edgeDocs =
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
            rankNormDocs rankDocs 5 5 query edgeDocs'


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
      perTargetEdges = fmap (\edgeDocs' -> HM.fromListWith (++) $ foldMap groupByEntity edgeDocs' ) $ perSourceEdges
  in perTargetEdges
  where groupByEntity :: EdgeDoc -> [(PageId, [EdgeDoc])]
        groupByEntity edgeDoc =
                  [ (entity, [edgeDoc])
                  | entity <- edgeDocNeighbors $ edgeDoc]


accumulateEdgeWeights :: forall w. Num w => HM.HashMap PageId [EdgeDocWithScores] -> (EdgeDocWithScores -> w) ->   HM.HashMap PageId (HM.HashMap PageId w)
accumulateEdgeWeights sourceToEdgeDocsWithScores by=
    HM.mapWithKey countEdgeDocs sourceToEdgeDocsWithScores
  where countEdgeDocs :: PageId -> [EdgeDocWithScores] -> HM.HashMap PageId w
        countEdgeDocs sourceNode edgeDocsWithScores =
            HM.fromListWith (+)
              [ (targetNode, by edgeDocsWithScore)
              | edgeDocsWithScore <- edgeDocsWithScores
              , targetNode <- edgeDocNeighbors $ withScoreEdgeDoc $ edgeDocsWithScore
              , targetNode /= sourceNode
              ]



-- Marginalized over second argument in edges, e.g. Map source (Map target weight_{st}) -> Map source weight_{s*}
marginalizeEdges :: HM.HashMap PageId (HM.HashMap PageId Double) -> [(PageId, Double)]
marginalizeEdges graph =
    HM.toList $ fmap marginalizeMap $ graph
  where marginalizeMap :: HM.HashMap PageId Double -> Double
        marginalizeMap = sum . fmap snd . HM.toList


-- ----------------------------------------------------------------------

data GraphNames = Top5PerNode | Top100PerGraph | SimpleGraph | RandomGraph | Random2000Graph  | Top10PerGraph | Top50PerGraph | Top200PerGraph | Top2000PerGraph
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data WeightingNames = Count | Binary | Score | RecipRank | LinearRank| BucketRank
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data GraphRankingNames = PageRank | PersPageRank | AttriRank | ShortPath | MargEdges
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data EdgeFilteringNames = Unfiltered | BidiFiltered
    deriving (Show, Enum, Bounded, Ord, Eq, Generic)
data Method = Method GraphNames EdgeFilteringNames WeightingNames GraphRankingNames
            | CandidateSet
    deriving ( Ord, Eq, Generic)
instance Show Method where
    show = showMethodName
showMethodName:: Method -> String
showMethodName (Method a b c d) = intercalate "-" [show a, show b, show c, show d]
showMethodName (CandidateSet ) = "CandidateSet"

allMethods :: [Method]
allMethods = [ Method gName eName wName rName
             | gName <- [minBound :: GraphNames .. maxBound ]
             , eName <- [minBound :: EdgeFilteringNames .. maxBound]
             , wName <- [minBound :: WeightingNames .. maxBound]
             , rName <- [minBound :: GraphRankingNames .. maxBound]
             ] ++ [CandidateSet]

coreMethods :: [Method]
coreMethods = [ Method gName eName wName rName
             | gName <- [Top100PerGraph, Top2000PerGraph, RandomGraph ]
             , eName <- [minBound :: EdgeFilteringNames .. maxBound]
             , wName <- [Count, Score]
             , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
             ]

prio1Methods :: [Method]
prio1Methods = [ Method gName eName wName rName
                          | gName <- [Top100PerGraph, Top2000PerGraph, RandomGraph ]
                          , eName <- [Unfiltered]
                          , wName <- [Count, Score]
                          , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                          ]

prio2Methods :: [Method]
prio2Methods = [ Method gName eName wName rName
                          | gName <- [SimpleGraph, Top5PerNode ]
                          , eName <- [Unfiltered]
                          , wName <- [Count, Score]
                          , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                          ]
prio3Methods :: [Method]
prio3Methods = [ Method gName eName wName rName
                          | gName <- [Top10PerGraph, Top50PerGraph, Top200PerGraph ]
                          , eName <- [Unfiltered]
                          , wName <- [Count, Score]
                          , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                          ]

prio4Methods :: [Method]
prio4Methods = [ Method gName eName wName rName
                          | gName <- [Top100PerGraph, Top2000PerGraph, Random2000Graph, Top10PerGraph, Top50PerGraph, Top200PerGraph]
                          , eName <- [Unfiltered]
                          , wName <- [Count, Score, RecipRank, LinearRank, BucketRank]
                          , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                          ]

fix1Methods :: [Method]
fix1Methods = [ Method gName eName wName rName
                          | gName <- [Top100PerGraph, Top2000PerGraph ]
                          , eName <- [Unfiltered]
                          , wName <- [Count, Score]
                          , rName <- [AttriRank]
                          ]

fix2Methods :: [Method]
fix2Methods = [ Method gName eName wName rName
                          | gName <- [Random2000Graph ]
                          , eName <- [Unfiltered]
                          , wName <- [Count, Score]
                          , rName <- [PersPageRank, MargEdges, AttriRank, PageRank, ShortPath]
                          ]


testMethods :: [Method]
testMethods = [ Method gName eName wName rName
                          | gName <- [minBound :: GraphNames .. maxBound]
                          , eName <- [Unfiltered]
                          , wName <- [Binary, Score]
                          , rName <- [PersPageRank, MargEdges, AttriRank, ShortPath]
                          ]

topNPerGraphMethods :: [Method]
topNPerGraphMethods = [ Method gName eName wName rName
             | gName <- [Top100PerGraph, Top10PerGraph, Top50PerGraph, Top200PerGraph, Top2000PerGraph]
             , eName <- [minBound :: EdgeFilteringNames .. maxBound]
             , wName <- [Count, Score, RecipRank]
             , rName <- [PersPageRank, PageRank, ShortPath, MargEdges]
             ]


-- instance NFData GraphNames
-- instance NFData WeightingNames
-- instance NFData GraphRankingNames


type RankingFunction = forall elem. [Term] -> [(elem, T.Text)] -> [(elem, Double)]
