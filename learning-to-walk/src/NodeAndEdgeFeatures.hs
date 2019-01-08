{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BangPatterns #-}
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
    , mkFeatureSpaces
    , FeatureSpaces(..)
    )  where


import Debug.Trace

import Control.Parallel.Strategies

import Data.Functor.Identity
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Data.Maybe
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor

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
import LookupWrapper
import CandidateGraph

data FeatureSpaces entityPh edgePh = FeatureSpaces { edgeFSpace :: F.FeatureSpace EdgeFeature edgePh
                                                   , entityFSpace :: F.FeatureSpace EntityFeature entityPh
                                                   , combinedFSpace :: F.FeatureSpace CombinedFeature (F.Stack '[entityPh, edgePh])
                                                   }

mkFeatureSpaces :: F.FeatureSpace CombinedFeature s
                -> (F.FeatureMappingInto CombinedFeature s CombinedFeature (F.Stack '[entityPh, edgePh])
                     -> FeatureSpaces entityPh edgePh
                     -> r)
                -> r
mkFeatureSpaces fspace f = runIdentity $ do
    F.SomeFeatureSpace edgeFSpace <- pure $ F.mkFeatureSpace $ S.fromList $ mapMaybe (either (const Nothing) Just) $ F.featureNames fspace
    F.SomeFeatureSpace entityFSpace <- pure $ F.mkFeatureSpace $ S.fromList $ mapMaybe (either Just (const Nothing)) $ F.featureNames fspace
    let combinedFSpace = F.eitherSpaces entityFSpace edgeFSpace
    -- This is an identity
    modelToCombinedFeatureVec <- pure $ fromMaybe (error "mkFeatureSpaces: impossible") $ F.mapFeaturesInto fspace combinedFSpace Just
    pure $ f modelToCombinedFeatureVec FeatureSpaces{..}

-- | merge entity and edge features
combineEntityEdgeFeatures
    :: forall entityFSpace edgeFSpace.
       FeatureSpaces entityFSpace edgeFSpace
    -> QueryId
    -> PagesLookup
    -> Candidates
    -> HM.HashMap (QueryId, PageId) (CombinedFeatureVec entityFSpace edgeFSpace)
combineEntityEdgeFeatures (FeatureSpaces {..})
                          query pagesLookup
                          cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                          , candidateEdgeRuns = edgeRun
                                          , candidateEntityRuns = entityRun
                                          , candidatePages = candidatePages
                                          } =
    let
        edgeFeatureGraph :: Graph PageId (EdgeFeatureVec edgeFSpace)
        edgeFeatureGraph = generateEdgeFeatureGraph edgeFSpace query cands

        nodeFeatures :: HM.HashMap PageId (EntityFeatureVec entityFSpace)
        nodeFeatures = generateNodeFeatures entityFSpace query entityRun allEdgeDocs

        stackFeatures :: EntityFeatureVec entityFSpace
                      -> EdgeFeatureVec edgeFSpace
                      -> CombinedFeatureVec entityFSpace edgeFSpace
        stackFeatures uFeats eFeats =
            F.fromList combinedFSpace $ map (first Left) (F.toList uFeats) ++ map (first Right) (F.toList eFeats)

        -- stack node vector on top of projected edge feature vector
        -- no need to use nodeEdgeFeatureGraph
    in HM.fromList
       [ ((query, u), stackFeatures uFeats (F.aggregateWith (+) edgeFeats))
       | entityRankEntry <- entityRun
       , let u = multiRankingEntryGetDocumentName entityRankEntry

       , let uFeats = fromMaybe (makeDefaultEntFeatVector entityFSpace) $  u `HM.lookup` nodeFeatures
       , let edgeFeats =
                 fromMaybe [makeDefaultEdgeFeatVector edgeFSpace]
                 $ NE.nonEmpty $ HM.elems  -- gives Nothing in case of empty
                 $ getNeighbors edgeFeatureGraph u
       ]




-- | merge node and edge features (used for both training, prediction, and walking)
makeCombinedFeatureVec
    :: FeatureSpaces entityFSpace edgeFSpace
    -> CandidateGraphGenerator
    -> PagesLookup
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map (QueryId, T.Text) (CombinedFeatureVec entityFSpace edgeFSpace)
makeCombinedFeatureVec fspaces candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun =
    M.unions
    $ withStrategy (parBuffer 200 rwhnf)
      [ M.fromList
        [ ((qid, T.pack $ unpackPageId pid), features)
        | ((qid, pid), features) <- HM.toList $ combineEntityEdgeFeatures fspaces query pagesLookup candidates
        ]
      | (query, edgeRun) <- M.toList collapsedEdgedocRun
      , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
      , let candidates = candidateGraphGenerator query edgeRun entityRun
      ]


-- | used for training
makeStackedFeatures
    :: FeatureSpaces entityFSpace edgeFSpace
    -> CandidateGraphGenerator
    -> PagesLookup
    -> M.Map QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map (QueryId, QRel.DocumentName) (CombinedFeatureVec entityFSpace edgeFSpace)
makeStackedFeatures fspaces candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun =
    let docFeatures'' = withStrategy (parTraversable rwhnf)
                        $ fmap crit
                        $ makeCombinedFeatureVec fspaces candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun
                        where crit = filterExpSettings (combinedFSpace fspaces)

        normalizer = zNormalizer $ M.elems docFeatures''
    in withStrategy (parTraversable rwhnf)
       $ fmap (normFeatures normalizer) docFeatures''

-- | used for prediction and graph walk
makeStackedFeatures'
    :: FeatureSpaces entityFSpace edgeFSpace
    -> CandidateGraphGenerator
    -> PagesLookup
    -> M.Map QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map (QueryId, QRel.DocumentName) (CombinedFeatureVec entityFSpace edgeFSpace)
makeStackedFeatures' fspaces candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun =
    let docFeatures'' = withStrategy (parTraversable rwhnf)
                        $ fmap crit
                        $ makeCombinedFeatureVec fspaces candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun
                        where crit = filterExpSettings (combinedFSpace fspaces)

        normalizer = zNormalizer $ M.elems docFeatures''
    in withStrategy (parTraversable rwhnf)
       $ fmap (normFeatures normalizer) docFeatures''

-- | rewrite a M.Map by mapping over keys
changeKey :: Ord k' => (k-> k') -> M.Map k v -> M.Map k' v
changeKey f map_ =
    M.fromList $ fmap (\(key,val) -> (f key, val)) $ M.toList map_



-- ------------ Make Node features --------------------

-- | generate node features
generateNodeFeatures :: F.FeatureSpace EntityFeature entityFSpace
                     -> QueryId -> [MultiRankingEntry PageId GridRun] -> [EdgeDoc]
                     -> HM.HashMap PageId (EntityFeatureVec entityFSpace)
generateNodeFeatures entityFSpace query entityRun allEdgeDocs =
   let
        universalGraph :: HM.HashMap PageId [EdgeDoc]
        universalGraph = edgeDocsToUniverseGraph allEdgeDocs

   in HM.fromList [ (entity, (entityScoreVec entityFSpace entityRankEntry edgeDocs))
                  | entityRankEntry <- entityRun
                  , let entity = multiRankingEntryGetDocumentName entityRankEntry  -- for each entity in ranking...
                  , let edgeDocs = fromMaybe [] $ entity `HM.lookup` universalGraph
                  ]


entityScoreVec :: F.FeatureSpace EntityFeature entityFSpace -> MultiRankingEntry PageId GridRun -> [EdgeDoc] -> EntityFeatureVec entityFSpace
entityScoreVec entityFSpace entityRankEntry incidentEdgeDocs = makeEntFeatVector entityFSpace (
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
generateEdgeFeatureGraph :: forall edgeFSpace.
                            F.FeatureSpace EdgeFeature edgeFSpace
                         -> QueryId
                         -> Candidates
                         -> Graph PageId (EdgeFeatureVec edgeFSpace)
generateEdgeFeatureGraph edgeFSpace
                         query
                         cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                         , candidateEdgeRuns = edgeRun
                                         , candidateEntityRuns = entityRun
                                         , candidatePages = candidatePages
                                         } =
    let
        edgeDocsLookup = wrapEdgeDocsTocs $ HM.fromList $ [ (edgeDocParagraphId edgeDoc, edgeDoc) | edgeDoc <- allEdgeDocs]
        pagesLookup = wrapPagesTocs $ HM.fromList $  [ (pageDocId page, page) | page <- candidatePages]

        edgeFeaturesFromPara = edgesFromParas edgeFSpace edgeDocsLookup edgeRun
        edgeFeaturesFromPage = edgesFromPages edgeFSpace pagesLookup entityRun

        allHyperEdges :: HM.HashMap (PageId, PageId) (EdgeFeatureVec edgeFSpace)
        -- todo fidget about hoping to bring back my performance
        allHyperEdges = HM.fromListWith (F.^+^) edgeFeaturesFromPara
--         allHyperEdges = HM.fromListWith aggrFeatVecs $ edgeFeaturesFromPara ++ edgeFeaturesFromPage

        edgeFeaturesGraph :: [(PageId, PageId, EdgeFeatureVec edgeFSpace)]
        edgeFeaturesGraph = [ (n1, n2, e) | ((n1, n2), e) <- HM.toList allHyperEdges ]

        singletonNodes :: [PageId]
        singletonNodes =
            [ entityId
            | run <- entityRun
            , let entityId = CAR.RunFile.carDocument $ multiRankingEntryCollapsed run
            ]

        edgeFeatureGraphWithSingleNodes =
            Graph.graphFromEdgesAndSingletons edgeFeaturesGraph singletonNodes

    in traceShow ("generateEdgeFeatureGraph", query,
                   Graph.numNodes edgeFeatureGraphWithSingleNodes,
                   F.dimension $ F.featureSpace $ head $ HM.elems allHyperEdges)
      edgeFeatureGraphWithSingleNodes

edgesFromParas :: forall edgeFSpace.
                  F.FeatureSpace EdgeFeature edgeFSpace
               -> EdgeDocsLookup
               -> [MultiRankingEntry ParagraphId GridRun]
               -> [((PageId, PageId), EdgeFeatureVec edgeFSpace)]
edgesFromParas edgeFSpace edgeDocsLookup edgeRuns =
    let
        edgeDoc paraId = case edgeDocsLookup [paraId] of
                           [] -> error $ "No edgedoc for paraId "++show paraId
                           (a:_) -> a
        edgeFeat :: ParagraphId
                 -> MultiRankingEntry ParagraphId GridRun
                 -> F.FeatureVec EdgeFeature edgeFSpace Double
        edgeFeat paraId edgeEntry = edgeScoreVec edgeFSpace FromParas edgeEntry (edgeDoc paraId)

        divideEdgeFeats feats cardinality = F.scale (1 / (realToFrac cardinality)) feats
        edgeCardinality ed = HS.size $ edgeDocNeighbors ed


        oneHyperEdge :: (ParagraphId, MultiRankingEntry ParagraphId GridRun)
                     -> [((PageId, PageId), EdgeFeatureVec edgeFSpace )]
        oneHyperEdge (paraId, edgeEntry) =
              [ ((u, v) , dividedFeatVec)
              | u <- HS.toList $ edgeDocNeighbors (edgeDoc paraId)
              , v <- HS.toList $ edgeDocNeighbors (edgeDoc paraId) -- include self links (v==u)!
              , let featVec = edgeFeat paraId edgeEntry
              , let dividedFeatVec = divideEdgeFeats featVec (edgeCardinality (edgeDoc paraId))
              ]

    in mconcat [ oneHyperEdge (multiRankingEntryGetDocumentName edgeEntry, edgeEntry)
               | edgeEntry <- edgeRuns
               ]

data Role = RoleOwner | RoleLink
         deriving (Show, Read, Ord, Eq, Enum, Bounded)


edgesFromPages :: forall edgeFSpace.
                  F.FeatureSpace EdgeFeature edgeFSpace
               -> PagesLookup
               -> [MultiRankingEntry PageId GridRun]
               -> [((PageId, PageId), EdgeFeatureVec edgeFSpace)]
edgesFromPages edgeFSpace pagesLookup entityRuns =
    let
        page :: PageId -> PageDoc
        page pageId = case pagesLookup [pageId] of
                           [] -> error $ "No page for pageId "++show pageId
                           (a:_) -> a

        pageNeighbors :: PageDoc -> [(PageId, Role)]
        pageNeighbors p = ([(pageDocArticleId p, RoleOwner)]) ++ (fmap (\v -> (v, RoleLink)) $ HS.toList $ pageDocOnlyNeighbors p)

        edgeFeat :: PageId
                 -> MultiRankingEntry PageId GridRun
                 -> FromSource
                 -> F.FeatureVec EdgeFeature edgeFSpace Double
        edgeFeat pageId entityEntry source = edgePageScoreVec edgeFSpace source entityEntry (page pageId)

        divideEdgeFeats feats cardinality = F.scale (1 / (realToFrac cardinality)) feats


        oneHyperEdge :: (PageId, MultiRankingEntry PageId GridRun)
                     -> [((PageId, PageId), EdgeFeatureVec edgeFSpace)]
        oneHyperEdge (pageId, entityEntry) =
              [ ((u, v) , dividedFeatVec)
              | let !p = page pageId
                    neighbors = pageNeighbors p
                    !cardinality = HS.size (pageDocOnlyNeighbors p) + 1
              , (u, uRole) <- neighbors
              , (v, vRole) <- neighbors -- include self links (v==u)!
              , let !featVec = edgeFeat pageId entityEntry (getSource uRole vRole)
              , let !dividedFeatVec = divideEdgeFeats featVec cardinality
              ]
          where getSource :: Role -> Role -> FromSource
                getSource RoleOwner RoleLink = FromPagesOwnerLink
                getSource RoleLink RoleOwner = FromPagesLinkOwner
                getSource RoleLink RoleLink = FromPagesLinkLink
                getSource RoleOwner RoleOwner = FromPagesSelf
                getSource u v = error $ "edgesFromPages: Don't know source for roles "<>show u <> ", "<> show v


    in mconcat [ oneHyperEdge (multiRankingEntryGetDocumentName entityEntry, entityEntry)
               | entityEntry <- entityRuns
               ]




                          -- TODO use different features for page edge features

edgePageScoreVec :: F.FeatureSpace EdgeFeature edgeFSpace
                 -> FromSource
                 -> MultiRankingEntry p GridRun
                 -> PageDoc
                 -> F.FeatureVec EdgeFeature edgeFSpace Double
edgePageScoreVec fspace source pageRankEntry _page =
    makeEdgeFeatVector fspace
        $ ([ (EdgeCount source , 1.0)
           ]
          ++ rankEdgeFeatures source Aggr (multiRankingEntryCollapsed pageRankEntry)
          ++ concat [ rankEdgeFeatures source (GridRun' g) entry
                    | (g, entry) <- multiRankingEntryAll pageRankEntry
                    ]
         )


edgeScoreVec :: F.FeatureSpace EdgeFeature edgeFSpace
             -> FromSource
             -> MultiRankingEntry p GridRun
             -> EdgeDoc
             -> F.FeatureVec EdgeFeature edgeFSpace Double
edgeScoreVec fspace source edgedocsRankEntry edgeDoc
                                 = makeEdgeFeatVector fspace $
                                    [ (EdgeCount source, 1.0)
                                    -- TODO
                                    --, ( EdgeDocKL
                                    --  , let Just edgeDocs = multiRankingEntryGetDocumentName edgedocsRankEntry `HM.lookup` edgeDocsByPara
                                    --    in edgeDocKullbackLeibler connectedEdgeDocs edgeDocs
                                    --  )
                                    ]
                                    ++ rankEdgeFeatures source Aggr (multiRankingEntryCollapsed edgedocsRankEntry)
                                    ++ concat [ rankEdgeFeatures source (GridRun' g) entry
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

