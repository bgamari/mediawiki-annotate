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
import Data.Foldable  as Foldable
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor
import Control.Monad.ST
import GHC.Stack
import qualified Control.Foldl as F

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

-- ---
import CAR.Retrieve  as Retrieve



(>!<) :: (Show k, Ord k, HasCallStack) => M.Map k v -> k -> v
m >!< key =
    case key `M.lookup` m  of
        Just v -> v
        Nothing -> error $ ">!<: Can't lookup key "<> show key <> " in map. Map size: "<> show (length m) <>" Example keys " <> (show $ take 10 $ M.keys m)<> "..."


data Role = RoleOwner | RoleLink
         deriving (Show, Read, Ord, Eq, Enum, Bounded)

data FeatureSpaces entityPh edgePh = FeatureSpaces { edgeFSpace :: F.FeatureSpace EdgeFeature edgePh
                                                   , entityFSpace :: F.FeatureSpace EntityFeature entityPh
                                                   , combinedFSpace :: F.FeatureSpace CombinedFeature (F.Stack '[entityPh, edgePh])
                                                   }


type EdgeFeatureList = [(PageId, EdgeFeature, Double)]

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

--
-- stackFeatures :: forall entityPh edgePh.
--                    FeatureSpaces entityPh edgePh
--                 -> EntityFeatureVec entityPh
--                 -> EdgeFeatureVec edgePh
--                 -> CombinedFeatureVec entityPh edgePh
-- stackFeatures FeatureSpaces{..} uFeats eFeats =
--     F.fromList combinedFSpace $ map (first Left) (F.toList uFeats) ++ map (first Right) (F.toList eFeats)
--

keySet :: HM.HashMap k v -> HS.HashSet k
keySet m = HS.fromMap $ fmap ( const () ) m

-- | merge entity and edge features
combineEntityEdgeFeatures
    :: forall entityPh edgePh.
       FeatureSpaces entityPh edgePh
    -> FeatureGraphSettings
    -> QueryId
    -> PagesLookup
    -> AspectLookup
    -> Candidates
    -> M.Map PageId (CombinedFeatureVec entityPh edgePh)
combineEntityEdgeFeatures spaces@(FeatureSpaces {..})
                          featureGraphSettings
                          query pagesLookup aspectLookup
                          cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                          , candidateEdgeRuns = edgeRun
                                          , candidateEntityRuns = entityRun
                                          , candidatePages = candidatePages
                                          , candidateAspectRuns = aspectRun
                                          } =
    let
        nodeFeatures :: HM.HashMap PageId [(EntityFeature, Double)]
        nodeFeatures = generateNodeFeatures entityFSpace query entityRun aspectRun allEdgeDocs

        edgeFeatures :: [((PageId, PageId), EdgeFeature, Double)]
        edgeFeatures = generateEdgeFeatureGraph' edgeFSpace featureGraphSettings query pagesLookup aspectLookup cands nodeFeatures

        res = makeCombinedFeatures combinedFSpace nodeFeatures edgeFeatures
    in res


-- | merge node and edge features (used for both training, prediction, and walking)
makeCombinedFeatureVec
    :: FeatureSpaces entityPh edgePh
    -> FeatureGraphSettings
    -> CandidateGraphGenerator
    -> PagesLookup
    -> AspectLookup
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map CAR.RunFile.QueryId [MultiRankingEntry AspectId GridRun]
    -> M.Map (QueryId, T.Text) (CombinedFeatureVec entityPh edgePh)
makeCombinedFeatureVec fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =
    M.unions
    $ withStrategy (parBuffer 200 rseq)
      [ M.fromList
        [ ((query, T.pack $ unpackPageId pid), features)
        | (pid, features) <- M.toList $ combineEntityEdgeFeatures fspaces featureGraphSettings query pagesLookup aspectLookup candidates
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
    -> AspectLookup
    -> M.Map QueryId [MultiRankingEntry PageId GridRun]
    -> M.Map QueryId [MultiRankingEntry ParagraphId GridRun]
    -> M.Map QueryId [MultiRankingEntry AspectId GridRun]
    -> M.Map (QueryId, QRel.DocumentName) (CombinedFeatureVec entityPh edgePh)
makeStackedFeatures fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun =
    let docFeatures'' = makeCombinedFeatureVec fspaces featureGraphSettings candidateGraphGenerator pagesLookup aspectLookup collapsedEntityRun collapsedEdgedocRun collapsedAspectRun

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
                     -> QueryId
                     -> [MultiRankingEntry PageId GridRun]
                     -> [MultiRankingEntry AspectId GridRun]
                     -> [EdgeDoc]
                     -> HM.HashMap PageId [(EntityFeature, Double)]
generateNodeFeatures entityFSpace query entityRun aspectRun allEdgeDocs =
   let
        pageIdToEdgeDocs :: HM.HashMap PageId [EdgeDoc]
        pageIdToEdgeDocs = edgeDocsToUniverseGraph allEdgeDocs
        pageIdToAspectRun :: HM.HashMap PageId [MultiRankingEntry AspectId GridRun]
        pageIdToAspectRun = HM.fromListWith (<>)
                            [ (aspectPageId, [entry])
                            | entry <- aspectRun
                            , aspectPageId <- toList $ aspectValidPageIds $ multiRankingEntryGetDocumentName entry
                            ]

        pageIdToEntityRun :: HM.HashMap PageId (MultiRankingEntry PageId GridRun)
        pageIdToEntityRun = HM.fromList [(entity, entityRankEntry)
                                        | entityRankEntry <- entityRun
                                        , let entity = multiRankingEntryGetDocumentName entityRankEntry
                                        ]

        -- we want entities that have all entities that have an entry in entityRun or aspectRun
        entities = toList $ (keySet pageIdToEntityRun) `HS.union` (keySet pageIdToAspectRun)
   in HM.fromList [ (entity, (entityScoreVec entityFSpace entityRankEntryOpt aspectRankEntries edgeDocs))
                  | entity <- entities
                  , let entityRankEntryOpt = entity `HM.lookup` pageIdToEntityRun
                  , let aspectRankEntries = fromMaybe [] $ entity `HM.lookup` pageIdToAspectRun
                  , let edgeDocs = fromMaybe [] $ entity `HM.lookup` pageIdToEdgeDocs
                  ]



entityScoreVec :: F.FeatureSpace EntityFeature entityPh
               -> Maybe (MultiRankingEntry PageId GridRun)
               -> [MultiRankingEntry AspectId GridRun]
               -> [EdgeDoc]
               -> [(EntityFeature, Double)]
entityScoreVec entityFSpace entityRankEntryOpt aspectRankEntries incidentEdgeDocs =
        [ (EntDegree, degree) ]
        ++ concat [ entityScoreVecFromMultiRankings entityRankEntry aspectRankEntries
                  | Just entityRankEntry <- pure $ entityRankEntryOpt
                  ]
   where
    degree =  realToFrac $ HS.size $ HS.unions $ fmap edgeDocNeighbors incidentEdgeDocs     -- todo aspect: degree only captures edgedocs: add PageDocs and AspectDocs



-- ------------------- make edge features ---------------
data FeatureGraphSettings = FeatureGraphSettings { fgsNoEdgeDocs :: Bool
                                                 , fgsNoPageDocs :: Bool
                                                 , fgsDisableDivideEdgeFeats :: Bool
                                                 , fgsRemoveLowFeatures :: Bool
                                                 , fgsNoAspectDocs :: Bool
                                                 }

makeEdgeFeatures :: forall edgePh entityPh .
                    F.FeatureSpace EdgeFeature edgePh
                 -> [((PageId, PageId), EdgeFeature, Double)]
                 -> M.Map (PageId, PageId) (EdgeFeatureVec edgePh)
makeEdgeFeatures edgeFSpace features = runST
                                     $ F.foldM (F.mkFeaturesF edgeFSpace defaultEdgeFeatures (+)) features


-- | used for train,test, and graph walk
generateEdgeFeatureGraph :: forall edgePh entityPh .
                            F.FeatureSpace EdgeFeature edgePh
                         -> FeatureGraphSettings
                         -> QueryId
                         -> PagesLookup
                         -> AspectLookup
                         -> Candidates
                         -> HM.HashMap PageId [(EntityFeature, Double)]
                         -> Graph PageId (EdgeFeatureVec edgePh)
generateEdgeFeatureGraph edgeFSpace
                         fs
                         query
                         pagesLookup aspectLookup
                         cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                         , candidateEdgeRuns = edgeRun
                                         , candidateEntityRuns = entityRun
                                         , candidatePages = candidatePages
                                         , candidateAspectRuns = aspectRun
                                         }
                         nodeFeatures =

    let allEdges = generateEdgeFeatureGraph' edgeFSpace fs query pagesLookup aspectLookup cands nodeFeatures
        edgeFeaturesGraph :: [(PageId, PageId, EdgeFeatureVec edgePh)]
        edgeFeaturesGraph = [ (n1, n2, e)
                            | ((n1, n2), e) <- M.toList $ makeEdgeFeatures edgeFSpace $ allEdges
                            ]

        singletonNodes :: [PageId]
        singletonNodes =
            [ entityId
            | run <- entityRun
            , let entityId = CAR.RunFile.carDocument $ multiRankingEntryCollapsed run
            ]

    in Graph.graphFromEdgesAndSingletons edgeFeaturesGraph singletonNodes


-- | used for train,test, and graph walk
generateEdgeFeatureGraph' :: forall edgePh entityPh .
                            F.FeatureSpace EdgeFeature edgePh
                         -> FeatureGraphSettings
                         -> QueryId
                         -> PagesLookup
                         -> AspectLookup
                         -> Candidates
                         -> HM.HashMap PageId [(EntityFeature, Double)]
                         -> [((PageId, PageId), EdgeFeature, Double)]
generateEdgeFeatureGraph' edgeFSpace
                         FeatureGraphSettings { fgsNoEdgeDocs =includeEdgesFromParas
                                              , fgsNoPageDocs = includeEdgesFromPages
                                              , fgsDisableDivideEdgeFeats = divideEdgeFeats
                                              , fgsNoAspectDocs = includeEdgesFromAspects
                                              }
                         query
                         pagesLookup aspectLookup
                         cands@Candidates{ candidateEdgeDocs = allEdgeDocs
                                         , candidateEdgeRuns = edgeRun
                                         , candidateEntityRuns = entityRun
                                         , candidatePages = candidatePages
                                         , candidateAspectRuns = aspectRun
                                         }
                         nodeFeatures =
    let
        edgeDocsLookup = wrapEdgeDocsTocs $ HM.fromList $ [ (edgeDocParagraphId edgeDoc, edgeDoc) | edgeDoc <- allEdgeDocs]
--         pagesLookup = wrapPagesTocs $ HM.fromList $  [ (pageDocId page, page) | page <- candidatePages]

        edgeFeaturesFromPara :: [((PageId, PageId), EdgeFeature, Double)]
        edgeFeaturesFromPara = if includeEdgesFromParas then edgesFromParas edgeDocsLookup edgeRun divideEdgeFeats nodeFeatures else []
        edgeFeaturesFromPage = if includeEdgesFromPages then edgesFromPages pagesLookup entityRun divideEdgeFeats nodeFeatures else []
        edgeFeaturesFromAspect = if includeEdgesFromAspects then edgesFromAspects aspectLookup aspectRun divideEdgeFeats nodeFeatures else []

        allHyperEdges :: [((PageId, PageId), EdgeFeature, Double)]
        allHyperEdges = edgeFeaturesFromPara ++ edgeFeaturesFromPage ++ edgeFeaturesFromAspect
     in allHyperEdges

dividingEdgeFeats ::  [( EdgeFeature, Double)] -> Int ->  [(EdgeFeature, Double)]
dividingEdgeFeats feats cardinality = fmap ( second $  (/ (realToFrac cardinality)) ) feats -- F.scale (1 / (realToFrac cardinality)) feats


-- rankScore in GridFeatures
edgeScoreScale :: MultiRankingEntry a GridRun -> Double
edgeScoreScale e =  CAR.RunFile.carScore $ multiRankingEntryCollapsed  e

edgesFromParas :: forall entityPh.
                  EdgeDocsLookup
               -> [MultiRankingEntry ParagraphId GridRun]
               -> Bool
               -> HM.HashMap PageId [(EntityFeature, Double)]
               -> [((PageId, PageId), EdgeFeature, Double)]
edgesFromParas edgeDocsLookup edgeRuns divideEdgeFeats nodeFeatures =
    let
        edgeDoc paraId = case edgeDocsLookup [paraId] of
                           [] -> error $ "No edgedoc for paraId "++show paraId
                           (a:_) -> a
        edgeFeat :: ParagraphId
                 -> MultiRankingEntry ParagraphId GridRun
                 -> [(EdgeFeature, Double)] -- F.FeatureVec EdgeFeature edgePh Double
        edgeFeat paraId edgeEntry = edgeScoreVec FromParas edgeEntry (edgeDoc paraId)

        edgeCardinality ed = HS.size $ edgeDocNeighbors ed


        oneHyperEdge :: (ParagraphId, MultiRankingEntry ParagraphId GridRun)
--                      -> [((PageId, PageId), EdgeFeatureVec edgePh )]
                     ->  [((PageId, PageId), EdgeFeature, Double)]
        oneHyperEdge (paraId, edgeEntry) =
              let !featVec = edgeFeat paraId edgeEntry
                  dividedFeatVec = dividingEdgeFeats featVec (edgeCardinality (edgeDoc paraId))
                  !normFeatVec = (if divideEdgeFeats then dividedFeatVec else featVec)
              in concat
                 [ prefixFeatureVectorWithItem (u,v) ( normFeatVec ++ neighFs)-- stack normFeatVec  oppositeNodeFeatVec ) -- featVec)-- dividedFeatVec)
                 | u <- HS.toList $ edgeDocNeighbors (edgeDoc paraId)
                 , v <- HS.toList $ edgeDocNeighbors (edgeDoc paraId) -- include self links (v==u)!
                 , let neighFs = neighborFeatures FromParas u v (edgeScoreScale edgeEntry) nodeFeatures
                 ]

    in mconcat [ oneHyperEdge (multiRankingEntryGetDocumentName edgeEntry, edgeEntry)
               | edgeEntry <- edgeRuns
               ]


edgesFromPages :: forall entityPh.
                  PagesLookup
               -> [MultiRankingEntry PageId GridRun]
               -> Bool
               -> HM.HashMap PageId [(EntityFeature, Double)]
               -> [((PageId, PageId), EdgeFeature, Double)]
edgesFromPages pagesLookup entityRuns divideEdgeFeats nodeFeatures =
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
                 -> [(EdgeFeature, Double)]
        edgeFeat pageId entityEntry source pg = edgeFragmentScoreVec source entityEntry pg

        oneHyperEdge :: (PageId, MultiRankingEntry PageId GridRun)
                     ->  [((PageId, PageId), EdgeFeature, Double)]
        oneHyperEdge (pageId, entityEntry) =
            concat
            $ [ prefixFeatureVectorWithItem (u, v) (normFeatVec ++ neighFs)
              | Just p <- pure $ page pageId
              , let neighbors = pageNeighbors p
                    !cardinality = HS.size (pageDocOnlyNeighbors p) + 1
              , (u, uRole) <- neighbors
              , (v, vRole) <- neighbors -- include self links (v==u)!
              , let source = (getSource uRole vRole)
              , let !featVec = edgeFeat pageId entityEntry source p
              , let !dividedFeatVec = dividingEdgeFeats featVec cardinality
              , let normFeatVec = (if divideEdgeFeats then dividedFeatVec else featVec)
              , let neighFs = neighborFeatures source u v (edgeScoreScale entityEntry) nodeFeatures
              ]
          where getSource :: Role -> Role -> FromSource
                getSource RoleOwner RoleLink = FromPagesOwnerLink
                getSource RoleLink RoleOwner = FromPagesLinkOwner
                getSource RoleLink RoleLink = FromPagesLinkLink
                getSource RoleOwner RoleOwner = FromPagesSelf
                getSource u v = error $ "edgesFromPages: Don't know source for roles "<>show u <> ", "<> show v


    in mconcat [ oneHyperEdge (multiRankingEntryGetDocumentName entityEntry, entityEntry)
               | entityEntry <- entityRuns
               , (any (\(_, entry) -> (CAR.RunFile.carRank entry) <= 100)  (multiRankingEntryAll entityEntry))
                || ( (CAR.RunFile.carRank $ multiRankingEntryCollapsed entityEntry )<= 100)      -- todo make 10 configurable
               ]



edgesFromAspects :: forall entityPh.
                  AspectLookup
               -> [MultiRankingEntry AspectId GridRun]
               -> Bool
               -> HM.HashMap PageId [(EntityFeature, Double)]
               -> [((PageId, PageId), EdgeFeature, Double)]
edgesFromAspects aspectLookup aspectRuns divideEdgeFeats nodeFeatures =
    let
        aspect :: AspectId -> Maybe AspectDoc
        aspect aspectId = case aspectLookup [aspectId] of
                           [] -> Debug.trace ("No aspectDoc for aspectId "++show aspectId) $ Nothing
                           (a:_) -> Just a
        pageNeighbors :: AspectDoc -> [(PageId, Role)]
        pageNeighbors aspectDoc = ([(pageDocArticleId aspectDoc, RoleOwner)])
                                ++ (fmap (\v -> (v, RoleLink)) $ HS.toList $ pageDocOnlyNeighbors aspectDoc)

        edgeFeat :: AspectId
                 -> MultiRankingEntry AspectId GridRun
                 -> FromSource
                 -> AspectDoc
                 -> [(EdgeFeature, Double)]
        edgeFeat aspectId aspectEntry source pg =
                edgeFragmentScoreVec source aspectEntry pg


        oneHyperEdge :: (AspectId, MultiRankingEntry AspectId GridRun)
                     -> [((PageId, PageId), EdgeFeature, Double)]
        oneHyperEdge (aspectId, aspectEntry) =
            concat
            $ [ prefixFeatureVectorWithItem (u, v) (normFeatVec ++ neighFs)
              | Just aspectDoc <- pure $ aspect aspectId
              , let neighbors = pageNeighbors aspectDoc
                    !cardinality = HS.size (pageDocOnlyNeighbors aspectDoc) + 1
              , (u, uRole) <- neighbors
              , (v, vRole) <- neighbors -- include self links (v==u)!
              , let source = (getSource uRole vRole)
              , let !featVec = edgeFeat aspectId aspectEntry source aspectDoc
              , let !dividedFeatVec = dividingEdgeFeats featVec cardinality
              , let normFeatVec = if divideEdgeFeats then dividedFeatVec else featVec
              , let neighFs = neighborFeatures source u v (edgeScoreScale aspectEntry) nodeFeatures
              ]
          where getSource :: Role -> Role -> FromSource
                getSource RoleOwner RoleLink = FromAspectsOwnerLink
                getSource RoleLink RoleOwner = FromAspectsLinkOwner
                getSource RoleLink RoleLink = FromAspectsLinkLink
                getSource RoleOwner RoleOwner = FromAspectsSelf
                getSource u v = error $ "edgesFromPages: Don't know source for roles "<>show u <> ", "<> show v


    in mconcat [ oneHyperEdge (multiRankingEntryGetDocumentName aspectEntry, aspectEntry)
               | aspectEntry <- aspectRuns
               , (any (\(_, entry) -> (CAR.RunFile.carRank entry) <= 100)  (multiRankingEntryAll aspectEntry))
                || ( (CAR.RunFile.carRank $ multiRankingEntryCollapsed aspectEntry )<= 100)      -- todo make 10 configurable
               ]




-- | stack node and edge features, while edge features are aggregated over incoming edges (i.e., summing over u)
-- | Also see 'neighborFeatures'
makeCombinedFeatures :: forall edgePh entityPh .
                    F.FeatureSpace CombinedFeature (F.Stack '[entityPh,edgePh])
                 -> HM.HashMap PageId [( EntityFeature, Double)]
                 -> [((PageId, PageId), EdgeFeature, Double)]
                 -> M.Map PageId (CombinedFeatureVec entityPh edgePh)
makeCombinedFeatures combinedFSpace nodeFeatures edgeFeatures =
    let nodeFeatures' = [ (u,f,v ) | (u, feats) <- HM.toList nodeFeatures, (f,v) <- feats]
        features = fmap projectEntFs nodeFeatures'
                 ++ fmap projectEdgeFs edgeFeatures
    in runST
       $ F.foldM (F.mkFeaturesF combinedFSpace defaultCombinedFeatures (+)) features
  where defaultCombinedFeatures (Left f) = defaultEntityFeatures f
        defaultCombinedFeatures (Right f) = defaultEdgeFeatures f
        projectEntFs (u, feat, value) =
            (u, Left (feat), value)
        projectEdgeFs ((u,v), feat, value) =  -- aggregate over incoming edges
            (v, Right (feat), value)



-- | convert node features into opposite/outgoing neigbor features. Opposite node is "from" u!
-- | Also see 'makeCombinedFeatures'
neighborFeatures :: FromSource -> PageId -> PageId -> Double -> HM.HashMap PageId [(EntityFeature, Double)] -> [(EdgeFeature, Double)]
neighborFeatures source u v scale nodeFeatures =
    (fmap ( first NeighborFeature ) $ uFeats)
    ++ (fmap ( first (NeighborSourceFeature source)) $ uFeats)
    ++ (fmap ( first (NeighborSourceScaleFeature source)) $ fmap (\(f,value) -> (f, value *scale)) uFeats)
  where uFeats = fromMaybe [] $ u `HM.lookup` nodeFeatures

prefixFeatureVectorWithItem :: a -> [(f,v)] -> [(a,f,v)]
prefixFeatureVectorWithItem item features =
    fmap (\(feat,value) -> (item, feat, value) ) features


edgeFragmentScoreVec ::  FromSource
                 -> MultiRankingEntry p GridRun
                 -> AbstractDoc id
                 ->  [(EdgeFeature, Double)]
edgeFragmentScoreVec source rankEntry _pageDoc =
    let  aggrFeats = rankEdgeFeatures source Aggr (multiRankingEntryCollapsed rankEntry)
         perRunFeat = concat [ rankEdgeFeatures source (GridRun' g) entry
                             | (g, entry) <- multiRankingEntryAll rankEntry
                             ]
    in ([ (EdgeCount source , 1.0)
           ]
          ++ aggrFeats
          ++ perRunFeat
         )


edgeScoreVec :: FromSource
             -> MultiRankingEntry p GridRun
             -> EdgeDoc
             -> [(EdgeFeature, Double)]
edgeScoreVec source edgedocsRankEntry edgeDoc
                                 =  [ (EdgeCount source, 1.0) ]
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

-- --------------------------------


edgeDocKullbackLeibler :: [T.Text] -> [T.Text] -> Double
edgeDocKullbackLeibler baseDoc frontDoc =
    let (backTermCounts, backTotal) = termCountsAndTotal baseDoc
        (termCounts, total) = termCountsAndTotal frontDoc
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

