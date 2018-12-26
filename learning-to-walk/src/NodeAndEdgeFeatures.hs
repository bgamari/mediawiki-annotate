{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedLists #-}

module NodeAndEdgeFeatures
    ( makeStackedFeatures
    , makeStackedFeatures'
    , generateEdgeFeatureGraph
    )  where



import Control.Concurrent.Async
import Control.Parallel.Strategies

import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Maybe
import qualified Data.List.NonEmpty as NE

import CAR.Types hiding (Entity)
import GridFeatures

import GraphExpansion
import qualified SimplIR.FeatureSpace as F
import SimplIR.FeatureSpace.Normalise

import qualified CAR.RunFile
import qualified SimplIR.Format.QRel as QRel
import MultiTrecRunFile
import Graph

import EdgeDocCorpus
import CandidateGraph

-- | merge entity and edge features
combineEntityEdgeFeatures
    :: QueryId
    -> Candidates
    -> HM.HashMap (QueryId, PageId) CombinedFeatureVec
combineEntityEdgeFeatures query cands@Candidates{candidateEdgeDocs = allEdgeDocs, candidateEdgeRuns = edgeRun, candidateEntityRuns = entityRun} =
    let
--         edgeDocsLookup = wrapEdgeDocsTocs $ HM.fromList $ [ (edgeDocParagraphId edgeDoc, edgeDoc) | edgeDoc <- allEdgeDocs]
        edgeFeatureGraph :: Graph PageId (EdgeFeatureVec)
        edgeFeatureGraph = generateEdgeFeatureGraph query cands -- edgeDocsLookup query edgeRun entityRun
        Graph edgeFeatureGraph' = edgeFeatureGraph

        nodeFeatures :: HM.HashMap PageId EntityFeatureVec
        nodeFeatures = generateNodeFeatures query entityRun allEdgeDocs

        -- stack node vector on top of projected edge feature vector
        -- no need to use nodeEdgeFeatureGraph
    in HM.fromList
       [ ((query, u), F.concatFeatureVec combinedFSpace uFeats (F.aggregateWith (+) edgeFeats))
       | entityRankEntry <- entityRun
       , let u = multiRankingEntryGetDocumentName entityRankEntry

       , let uFeats = fromMaybe makeDefaultEntFeatVector $  u `HM.lookup` nodeFeatures
       , let edgeFeats =
                 case u `HM.lookup` edgeFeatureGraph' of
                    Just mxs
                      | Just xs <- NE.nonEmpty $ HM.elems mxs -- check that values of mxs is non-empty
                                         -> xs
                      | otherwise        -> [makeDefaultEdgeFeatVector]
                    Nothing ->  [makeDefaultEdgeFeatVector]
       ]




-- | merge node and edge features (used for both training, prediction, and walking)
makeCombinedFeatureVec :: CandidateGraphGenerator
                       -> M.Map CAR.RunFile.QueryId [MultiRankingEntry PageId GridRun]
                       -> M.Map CAR.RunFile.QueryId [MultiRankingEntry ParagraphId GridRun]
                       -> M.Map (QueryId, T.Text) CombinedFeatureVec
makeCombinedFeatureVec candidateGraphGenerator collapsedEntityRun collapsedEdgedocRun =
    M.unions
    $ withStrategy (parBuffer 200 rwhnf)
      [ M.fromList
        [ ((qid, T.pack $ unpackPageId pid), features)
        | ((qid, pid), features) <- HM.toList $ combineEntityEdgeFeatures query candidates
        ]
      | (query, edgeRun) <- M.toList collapsedEdgedocRun
      , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
      , let candidates = candidateGraphGenerator query edgeRun entityRun
      ]


-- | used for training
makeStackedFeatures :: CandidateGraphGenerator
                    -> M.Map QueryId [MultiRankingEntry PageId GridRun]
                    -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
                    -> F.FeatureSpace CombinedFeature
                    -> M.Map (QueryId, QRel.DocumentName) CombinedFeatureVec
makeStackedFeatures candidateGraphGenerator collapsedEntityRun collapsedEdgedocRun combinedFSpace' =
    let docFeatures'' = withStrategy (parTraversable rwhnf)
                        $ fmap crit
                        $ makeCombinedFeatureVec candidateGraphGenerator collapsedEntityRun collapsedEdgedocRun
                        where crit = filterExpSettings combinedFSpace'

        normalizer = zNormalizer $ M.elems docFeatures''
    in withStrategy (parTraversable rwhnf)
       $ fmap (normFeatures normalizer) docFeatures''

-- | used for prediction and graph walk
makeStackedFeatures' :: CandidateGraphGenerator
                     -> F.FeatureSpace CombinedFeature
                     -> M.Map QueryId [MultiRankingEntry PageId GridRun]
                     -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
                     -> M.Map (QueryId, QRel.DocumentName) CombinedFeatureVec
makeStackedFeatures' candidateGraphGenerator fspace collapsedEntityRun collapsedEdgedocRun =
    let docFeatures'' = withStrategy (parTraversable rwhnf)
                        $ fmap crit
                        $ makeCombinedFeatureVec candidateGraphGenerator collapsedEntityRun collapsedEdgedocRun
                        where crit = filterExpSettings fspace

        normalizer = zNormalizer $ M.elems docFeatures''
    in withStrategy (parTraversable rwhnf)
       $ fmap (normFeatures normalizer) docFeatures''

-- | rewrite a M.Map by mapping over keys
changeKey :: Ord k' => (k-> k') -> M.Map k v -> M.Map k' v
changeKey f map_ =
    M.fromList $ fmap (\(key,val) -> (f key, val)) $ M.toList map_



-- ------------ Make Node features --------------------

-- | generate node features
generateNodeFeatures :: QueryId -> [MultiRankingEntry PageId GridRun] -> [EdgeDoc] -> HM.HashMap PageId EntityFeatureVec
generateNodeFeatures query entityRun allEdgeDocs =
   let
        universalGraph :: HM.HashMap PageId [EdgeDoc]
        universalGraph = edgeDocsToUniverseGraph allEdgeDocs

   in HM.fromList [ (entity, (entityScoreVec entityRankEntry edgeDocs))
                  | entityRankEntry <- entityRun
                  , let entity = multiRankingEntryGetDocumentName entityRankEntry  -- for each entity in ranking...
--                   , Just edgeDocs <- pure $ entity `HM.lookup` universalGraph
                  , let edgeDocs = fromMaybe [] $ entity `HM.lookup` universalGraph
                  ]


entityScoreVec :: MultiRankingEntry PageId GridRun -> [EdgeDoc] -> EntityFeatureVec
entityScoreVec entityRankEntry incidentEdgeDocs = makeEntFeatVector  (
      [ (EntIncidentEdgeDocsRecip, recip numIncidentEdgeDocs)
  --  , (EntDegreeRecip, recip degree)
      , (EntDegree, degree)
      ]
      ++ entityScoreVecFromMultiRankings entityRankEntry
     )

  where
   numIncidentEdgeDocs = realToFrac $ length incidentEdgeDocs
   degree =  realToFrac $ HS.size $ HS.unions $ fmap edgeDocNeighbors incidentEdgeDocs



-- ------------------- make edge features ---------------

-- | used for train,test, and graph walk
generateEdgeFeatureGraph:: QueryId
                        -> Candidates
                        -> Graph PageId EdgeFeatureVec
generateEdgeFeatureGraph query cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                               , candidateEdgeRuns = edgeRun
                                               , candidateEntityRuns = entityRun} = -- edgeDocsLookup query edgeRun entityRun =
    let
        edgeDocsLookup = wrapEdgeDocsTocs $ HM.fromList $ [ (edgeDocParagraphId edgeDoc, edgeDoc) | edgeDoc <- allEdgeDocs]
        edgeDoc paraId = case edgeDocsLookup [paraId] of
                           [] -> error $ "No edgedoc for paraId "++show paraId
                           (a:_) -> a

        edgeFeat :: ParagraphId
                 -> MultiRankingEntry ParagraphId GridRun
                 -> F.FeatureVec EdgeFeature Double
        edgeFeat paraId edgeEntry = edgeScoreVec edgeEntry (edgeDoc paraId)

        divideEdgeFeats feats cardinality = F.scaleFeatureVec (1 / (realToFrac cardinality)) feats
        edgeCardinality ed = HS.size $ edgeDocNeighbors ed

        aggrFeatVecs :: EdgeFeatureVec -> EdgeFeatureVec -> EdgeFeatureVec
        aggrFeatVecs features1 features2 =
            F.aggregateWith (+) [features1, features2]

        oneHyperEdge :: (ParagraphId, MultiRankingEntry ParagraphId GridRun)
                     -> [((PageId, PageId), EdgeFeatureVec)]
        oneHyperEdge (paraId, edgeEntry) =
              [ ((u, v) , dividedFeatVec)
              | u <- HS.toList $ edgeDocNeighbors (edgeDoc paraId)
              , v <- HS.toList $ edgeDocNeighbors (edgeDoc paraId) -- include self links (v==u)!
              , let featVec = edgeFeat paraId edgeEntry
              , let dividedFeatVec = divideEdgeFeats featVec (edgeCardinality (edgeDoc paraId))
              ]

        allHyperEdges :: HM.HashMap (PageId, PageId) EdgeFeatureVec
        allHyperEdges = HM.fromListWith aggrFeatVecs
                      $ foldMap oneHyperEdge
                      $ [ (multiRankingEntryGetDocumentName edgeEntry, edgeEntry)
                        | edgeEntry <- edgeRun
                        ]


        edgeFeatureGraph :: HM.HashMap PageId (HM.HashMap PageId EdgeFeatureVec)
        edgeFeatureGraph = HM.fromListWith (<>)
                         $ fmap (\((u,v),f) -> (u, HM.singleton v f))
                         $ HM.toList allHyperEdges

        allNodes :: HM.HashMap PageId (HM.HashMap PageId EdgeFeatureVec)
        allNodes = HM.fromList
                   $ [ (entityId, HM.empty)
                   | run <- entityRun
                   , let entityId = CAR.RunFile.carDocument $ multiRankingEntryCollapsed run
                   ]

        edgeFeatureGraphWithSingleNodes = HM.unionWith (<>) edgeFeatureGraph allNodes

    in Graph edgeFeatureGraphWithSingleNodes




edgeScoreVec :: MultiRankingEntry ParagraphId GridRun
             -> EdgeDoc
             -> F.FeatureVec EdgeFeature Double
edgeScoreVec edgedocsRankEntry edgeDoc
                                 = makeEdgeFeatVector $
                                    [ (EdgeCount, 1.0)
                                    -- TODO
                                    --, ( EdgeDocKL
                                    --  , let Just edgeDocs = multiRankingEntryGetDocumentName edgedocsRankEntry `HM.lookup` edgeDocsByPara
                                    --    in edgeDocKullbackLeibler connectedEdgeDocs edgeDocs
                                    --  )
                                    ]
                                    ++ rankEdgeFeatures Aggr (multiRankingEntryCollapsed edgedocsRankEntry)
                                    ++ concat [ rankEdgeFeatures (GridRun' g) entry
                                       | (g, entry) <- multiRankingEntryAll edgedocsRankEntry
                                      ]
{-
  where

        connectedEdgeDocs :: ParagraphId -> [EdgeDoc]
        connectedEdgeDocs = undefined

        edgeDocKullbackLeibler :: [EdgeDoc] -> EdgeDoc -> Double
        edgeDocKullbackLeibler otherEdgeDocsOfConnectedEntities edgeDoc =

          let (backTermCounts, backTotal) =
                termCountsAndTotal
                $ fmap edgeDocContent
                $ HS.toList $ HS.fromList
                $ otherEdgeDocsOfConnectedEntities

              (termCounts, total) =
                termCountsAndTotal [(edgeDocContent edgeDoc)]
          in kullbackLeibler (termCounts, total) (backTermCounts, backTotal)


        kullbackLeibler :: (TermCounts, Int) -> (TermCounts, Int) -> Double
        kullbackLeibler (termCounts, total) (backTermCounts, backTotal) =
                            Foldable.sum $ [ pi * (log pi - log qi)
                                            | (term,count) <- HM.toList (getTermCounts termCounts)
                                            , let pi = (realToFrac count) / (realToFrac total)
                                            , let bcount = fromJust $ term `HM.lookup` (getTermCounts backTermCounts)
                                            , let qi = (realToFrac bcount) / (realToFrac backTotal)
                                            ]

        termCountsAndTotal :: [T.Text] -> (Retrieve.TermCounts, Int)
        termCountsAndTotal texts =
                              let termCounts =
                                    foldMap (textToTokens) texts
                                  total = Foldable.sum $ HM.elems $ getTermCounts $ termCounts
                              in (termCounts, total)


textToTokens :: T.Text -> Retrieve.TermCounts
textToTokens = foldMap Retrieve.oneTerm . Retrieve.textToTokens'
-}
