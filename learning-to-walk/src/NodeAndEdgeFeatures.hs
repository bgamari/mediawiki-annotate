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
    , FeatureGraphSettings(..)
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
import AspectUtils
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
import Debug.Trace as Debug

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


stackFeatures :: forall entityPh edgePh.
                   FeatureSpaces entityPh edgePh
                -> EntityFeatureVec entityPh
                -> EdgeFeatureVec edgePh
                -> CombinedFeatureVec entityPh edgePh
stackFeatures FeatureSpaces{..} uFeats eFeats =
    F.fromList combinedFSpace $ map (first Left) (F.toList uFeats) ++ map (first Right) (F.toList eFeats)


-- | merge entity and edge features
combineEntityEdgeFeatures
    :: forall entityPh edgePh.
       FeatureSpaces entityPh edgePh
    -> FeatureGraphSettings
    -> QueryId
    -> PagesLookup
    -> Candidates
    -> HM.HashMap (QueryId, PageId) (CombinedFeatureVec entityPh edgePh)
combineEntityEdgeFeatures spaces@(FeatureSpaces {..})
                          featureGraphSettings
                          query pagesLookup
                          cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                          , candidateEdgeRuns = edgeRun
                                          , candidateEntityRuns = entityRun
                                          , candidatePages = candidatePages
                                          } =
    let
        edgeFeatureGraph :: Graph PageId (EdgeFeatureVec edgePh)
        edgeFeatureGraph = generateEdgeFeatureGraph edgeFSpace featureGraphSettings query pagesLookup cands

        nodeFeatures :: HM.HashMap PageId (EntityFeatureVec entityPh)
        nodeFeatures = generateNodeFeatures entityFSpace query entityRun allEdgeDocs


        -- no need to use nodeEdgeFeatureGraph
    in HM.fromList
       [ ((query, u), stackedFeatures)
       | entityRankEntry <- entityRun
       , let u = multiRankingEntryGetDocumentName entityRankEntry

       , let uFeats = fromMaybe (makeDefaultEntFeatVector entityFSpace) $  u `HM.lookup` nodeFeatures
       , let edgeFeats =
                 fromMaybe [makeDefaultEdgeFeatVector edgeFSpace]
                 $ NE.nonEmpty $ HM.elems  -- gives Nothing in case of empty
                 $ getNeighbors edgeFeatureGraph u
       , let stackedFeatures = stackFeatures spaces uFeats (F.aggregateWith (+) edgeFeats)
              -- stack node vector on top of projected edge feature vector
       , (not $ fgsRemoveLowFeatures featureGraphSettings) || (not $ isLowFeature spaces stackedFeatures)
              -- remove nodes whose feature vector is close to the default
       ]

isLowFeature :: forall entityPh edgePh.
                FeatureSpaces entityPh edgePh
             -> CombinedFeatureVec entityPh edgePh
             -> Bool
isLowFeature spaces@(FeatureSpaces {..}) featVec =
    F.l2Norm (defaultCombined F.^-^ featVec) < 10
  where
    !defaultCombined = stackFeatures spaces (makeDefaultEntFeatVector entityFSpace) (makeDefaultEdgeFeatVector edgeFSpace)



-- | merge node and edge features (used for both training, prediction, and walking)
makeCombinedFeatureVec
    :: FeatureSpaces entityPh edgePh
    -> FeatureGraphSettings
    -> CandidateGraphGenerator
    -> PagesLookup
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry AspectId GridRun]
    -> M.Map (QueryId, T.Text) (CombinedFeatureVec entityPh edgePh)
makeCombinedFeatureVec fspaces featureGraphSettings candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =
    M.unions
    $ withStrategy (parBuffer 200 rwhnf)
      [ M.fromList
        [ ((qid, T.pack $ unpackPageId pid), features)
        | ((qid, pid), features) <- HM.toList $ combineEntityEdgeFeatures fspaces featureGraphSettings query pagesLookup candidates
        ]
      | (query, edgeRun) <- M.toList collapsedEdgedocRun
      , let entityRun = fromMaybe [] $ query `M.lookup` collapsedEntityRun
      , let aspectRun = fromMaybe [] $ query `M.lookup` collapsedAspectRun
      , let candidates = candidateGraphGenerator query edgeRun entityRun aspectRun
      ]


-- | used for training
makeStackedFeatures
    :: FeatureSpaces entityPh edgePh
    -> FeatureGraphSettings
    -> CandidateGraphGenerator
    -> PagesLookup
    -> M.Map QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map QueryId [MultiRankingEntry AspectId GridRun]
    -> M.Map (QueryId, QRel.DocumentName) (CombinedFeatureVec entityPh edgePh)
makeStackedFeatures fspaces featureGraphSettings candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =
    let docFeatures'' = withStrategy (parTraversable rwhnf)
                        $ fmap crit
                        $ makeCombinedFeatureVec fspaces featureGraphSettings candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun
                        where crit = filterExpSettings (combinedFSpace fspaces)

        normalizer = zNormalizer $ M.elems docFeatures''
    in withStrategy (parTraversable rwhnf)
       $ fmap (normFeatures normalizer) docFeatures''

-- | used for prediction and graph walk
makeStackedFeatures'
    :: FeatureSpaces entityPh edgePh
    -> FeatureGraphSettings
    -> CandidateGraphGenerator
    -> PagesLookup
    -> M.Map QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map QueryId [MultiRankingEntry AspectId GridRun]
    -> M.Map (QueryId, QRel.DocumentName) (CombinedFeatureVec entityPh edgePh)
makeStackedFeatures' fspaces featureGraphSettings candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =
    let docFeatures'' = withStrategy (parTraversable rwhnf)
                        $ fmap crit
                        $ makeCombinedFeatureVec fspaces featureGraphSettings candidateGraphGenerator pagesLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun
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
generateNodeFeatures :: F.FeatureSpace EntityFeature entityPh
                     -> QueryId -> [MultiRankingEntry PageId GridRun] -> [EdgeDoc]
                     -> HM.HashMap PageId (EntityFeatureVec entityPh)
generateNodeFeatures entityFSpace query entityRun allEdgeDocs =
   let
        universalGraph :: HM.HashMap PageId [EdgeDoc]
        universalGraph = edgeDocsToUniverseGraph allEdgeDocs

   in HM.fromList [ (entity, (entityScoreVec entityFSpace entityRankEntry edgeDocs))
                  | entityRankEntry <- entityRun
                  , let entity = multiRankingEntryGetDocumentName entityRankEntry  -- for each entity in ranking...
                  , let edgeDocs = fromMaybe [] $ entity `HM.lookup` universalGraph
                  ]


entityScoreVec :: F.FeatureSpace EntityFeature entityPh -> MultiRankingEntry PageId GridRun -> [EdgeDoc] -> EntityFeatureVec entityPh
entityScoreVec entityFSpace entityRankEntry incidentEdgeDocs = makeEntFeatVector entityFSpace (
      [ --(EntIncidentEdgeDocsRecip, recip numIncidentEdgeDocs)
  --  , (EntDegreeRecip, recip degree)
       (EntDegree, degree)
      ]
      ++ entityScoreVecFromMultiRankings entityRankEntry
     )

  where
   numIncidentEdgeDocs = realToFrac $ length incidentEdgeDocs
   degree =  realToFrac $ HS.size $ HS.unions $ fmap edgeDocNeighbors incidentEdgeDocs



-- ------------------- make edge features ---------------
data FeatureGraphSettings = FeatureGraphSettings { fgsNoEdgeDocs :: Bool, fgsNoPageDocs :: Bool, fgsDisableDivideEdgeFeats :: Bool, fgsRemoveLowFeatures :: Bool }

-- | used for train,test, and graph walk
generateEdgeFeatureGraph :: forall edgePh.
                            F.FeatureSpace EdgeFeature edgePh
                         -> FeatureGraphSettings
                         -> QueryId
                         -> PagesLookup
                         -> Candidates
                         -> Graph PageId (EdgeFeatureVec edgePh)
generateEdgeFeatureGraph edgeFSpace
                         (FeatureGraphSettings includeEdgesFromParas includeEdgesFromPages divideEdgeFeats filterLowNodes)
                         query
                         pagesLookup
                         cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                         , candidateEdgeRuns = edgeRun
                                         , candidateEntityRuns = entityRun
                                         , candidatePages = candidatePages
                                         } =
    let
        edgeDocsLookup = wrapEdgeDocsTocs $ HM.fromList $ [ (edgeDocParagraphId edgeDoc, edgeDoc) | edgeDoc <- allEdgeDocs]
--         pagesLookup = wrapPagesTocs $ HM.fromList $  [ (pageDocId page, page) | page <- candidatePages]

        edgeFeaturesFromPara = if includeEdgesFromParas then edgesFromParas edgeFSpace edgeDocsLookup edgeRun divideEdgeFeats else []
        edgeFeaturesFromPage = if includeEdgesFromPages then edgesFromPages edgeFSpace pagesLookup entityRun divideEdgeFeats else []

        allHyperEdges :: HM.HashMap (PageId, PageId) (EdgeFeatureVec edgePh)
        allHyperEdges = HM.fromListWith (F.^+^) $ edgeFeaturesFromPara ++ edgeFeaturesFromPage

        edgeFeaturesGraph :: [(PageId, PageId, EdgeFeatureVec edgePh)]
        edgeFeaturesGraph = [ (n1, n2, e)
                            | ((n1, n2), e) <- HM.toList allHyperEdges
                            ]

        singletonNodes :: [PageId]
        singletonNodes =
            [ entityId
            | run <- entityRun
            , let entityId = CAR.RunFile.carDocument $ multiRankingEntryCollapsed run
            ]

        edgeFeatureGraphWithSingleNodes =
            Graph.graphFromEdgesAndSingletons edgeFeaturesGraph singletonNodes

    in traceShow ("generateEdgeFeatureGraph", query,
                   " nodes:",Graph.numNodes edgeFeatureGraphWithSingleNodes,
                   " edgeFeatures:", F.dimension $ F.featureSpace $ head $ HM.elems allHyperEdges)
      edgeFeatureGraphWithSingleNodes

edgesFromParas :: forall edgePh.
                  F.FeatureSpace EdgeFeature edgePh
               -> EdgeDocsLookup
               -> [MultiRankingEntry ParagraphId GridRun]
               -> Bool
               -> [((PageId, PageId), EdgeFeatureVec edgePh)]
edgesFromParas edgeFSpace edgeDocsLookup edgeRuns divideEdgeFeats =
    let
        edgeDoc paraId = case edgeDocsLookup [paraId] of
                           [] -> error $ "No edgedoc for paraId "++show paraId
                           (a:_) -> a
        edgeFeat :: ParagraphId
                 -> MultiRankingEntry ParagraphId GridRun
                 -> F.FeatureVec EdgeFeature edgePh Double
        edgeFeat paraId edgeEntry = edgeScoreVec edgeFSpace FromParas edgeEntry (edgeDoc paraId)

        dividingEdgeFeats feats cardinality = F.scale (1 / (realToFrac cardinality)) feats
        edgeCardinality ed = HS.size $ edgeDocNeighbors ed


        -- todo: Undo (temporarily deactivating feature division to diagnose loss of MAP)

        oneHyperEdge :: (ParagraphId, MultiRankingEntry ParagraphId GridRun)
                     -> [((PageId, PageId), EdgeFeatureVec edgePh )]
        oneHyperEdge (paraId, edgeEntry) =
              [ ((u, v) , (if divideEdgeFeats then dividedFeatVec else featVec)) -- featVec)-- dividedFeatVec)
              | u <- HS.toList $ edgeDocNeighbors (edgeDoc paraId)
              , v <- HS.toList $ edgeDocNeighbors (edgeDoc paraId) -- include self links (v==u)!
              , let featVec = edgeFeat paraId edgeEntry
              , let dividedFeatVec = dividingEdgeFeats featVec (edgeCardinality (edgeDoc paraId))
              ]

    in mconcat [ oneHyperEdge (multiRankingEntryGetDocumentName edgeEntry, edgeEntry)
               | edgeEntry <- edgeRuns
               ]

data Role = RoleOwner | RoleLink
         deriving (Show, Read, Ord, Eq, Enum, Bounded)


edgesFromPages :: forall edgePh.
                  F.FeatureSpace EdgeFeature edgePh
               -> PagesLookup
               -> [MultiRankingEntry PageId GridRun]
               -> Bool
               -> [((PageId, PageId), EdgeFeatureVec edgePh)]
edgesFromPages edgeFSpace pagesLookup entityRuns divideEdgeFeats =
    let
        page :: PageId -> Maybe PageDoc
        page pageId = case pagesLookup [pageId] of
                           [] -> Debug.trace ("No page for pageId "++show pageId) $ Nothing
                           (a:_) -> Just a

        pageNeighbors :: PageDoc -> [(PageId, Role)]
        pageNeighbors p = ([(pageDocArticleId p, RoleOwner)]) ++ (fmap (\v -> (v, RoleLink)) $ HS.toList $ pageDocOnlyNeighbors p)

        edgeFeat :: PageId
                 -> MultiRankingEntry PageId GridRun
                 -> FromSource
                 -> PageDoc
                 -> F.FeatureVec EdgeFeature edgePh Double
        edgeFeat pageId entityEntry source pg = edgePageScoreVec edgeFSpace source entityEntry pg

        dividingEdgeFeats feats cardinality = F.scale (1 / (realToFrac cardinality)) feats


        -- todo: Undo (temporarily deactivating feature division to diagnose loss of MAP)


        oneHyperEdge :: (PageId, MultiRankingEntry PageId GridRun)
                     -> [((PageId, PageId), EdgeFeatureVec edgePh)]
        oneHyperEdge (pageId, entityEntry) =
              [ ((u, v) , (if divideEdgeFeats then dividedFeatVec else featVec))
              | Just p <- pure $ page pageId
              , let neighbors = pageNeighbors p
                    !cardinality = HS.size (pageDocOnlyNeighbors p) + 1
              , (u, uRole) <- neighbors
              , (v, vRole) <- neighbors -- include self links (v==u)!
              , let !featVec = edgeFeat pageId entityEntry (getSource uRole vRole) p
              , let !dividedFeatVec = dividingEdgeFeats featVec cardinality
              ]
          where getSource :: Role -> Role -> FromSource
                getSource RoleOwner RoleLink = FromPagesOwnerLink
                getSource RoleLink RoleOwner = FromPagesLinkOwner
                getSource RoleLink RoleLink = FromPagesLinkLink
                getSource RoleOwner RoleOwner = FromPagesSelf
                getSource u v = error $ "edgesFromPages: Don't know source for roles "<>show u <> ", "<> show v


    in mconcat [ oneHyperEdge (multiRankingEntryGetDocumentName entityEntry, entityEntry)
               | entityEntry <- entityRuns
               , (any (\(_, entry) -> (CAR.RunFile.carRank entry) <= 10)  (multiRankingEntryAll entityEntry))
                || ( (CAR.RunFile.carRank $ multiRankingEntryCollapsed entityEntry )<= 10)      -- todo make 10 configurable
               ]




                          -- TODO use different features for page edge features

edgePageScoreVec :: F.FeatureSpace EdgeFeature edgePh
                 -> FromSource
                 -> MultiRankingEntry p GridRun
                 -> PageDoc
                 -> F.FeatureVec EdgeFeature edgePh Double
edgePageScoreVec fspace source pageRankEntry _page =
    makeEdgeFeatVector fspace
        $ ([ (EdgeCount source , 1.0)
           ]
          ++ rankEdgeFeatures source Aggr (multiRankingEntryCollapsed pageRankEntry)
          ++ concat [ rankEdgeFeatures source (GridRun' g) entry
                    | (g, entry) <- multiRankingEntryAll pageRankEntry
                    ]
         )


edgeScoreVec :: F.FeatureSpace EdgeFeature edgePh
             -> FromSource
             -> MultiRankingEntry p GridRun
             -> EdgeDoc
             -> F.FeatureVec EdgeFeature edgePh Double
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

