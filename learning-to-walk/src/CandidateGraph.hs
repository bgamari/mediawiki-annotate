{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module CandidateGraph where

import GHC.Generics
import Control.DeepSeq
import Control.Concurrent.Async
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable
import Data.Text as T

import CAR.Types hiding (Entity)
import CAR.Utils
import AspectUtils
import qualified CAR.RunFile as CarRun
import GridFeatures

import EdgeDocCorpus
import LookupWrapper

import qualified CAR.RunFile
import MultiTrecRunFile



import Debug.Trace



-- --------------- Candidate graph  ------------------------
data Candidates = Candidates { candidateEdgeDocs :: [EdgeDoc]
                             , candidateEdgeRuns :: [MultiRankingEntry ParagraphId GridRun]
                             , candidateEntityRuns :: [MultiRankingEntry PageId GridRun]
                             , candidatePages :: [PageDoc]
                             , candidateAspectRuns :: [MultiRankingEntry AspectId GridRun]
                             }
                deriving (Generic)

instance NFData Candidates

type CandidateGraphGenerator =
     QueryId
    -> [MultiRankingEntry ParagraphId GridRun]
    -> [MultiRankingEntry PageId GridRun]
    -> [MultiRankingEntry AspectId GridRun]
    -> Candidates



selectGenerousCandidateGraph
    :: EdgeDocsLookup
    -> PagesLookup
    -> CandidateGraphGenerator
selectGenerousCandidateGraph edgeDocsLookup pagesLookup _queryId edgeRun entityRun aspectRun =
    candidates
  where
    !candidates =
        force $
        Candidates { candidateEdgeDocs = edgeDocs''
                   , candidateEdgeRuns = edgeRun''
                   , candidateEntityRuns = entityRun''
                   , candidateAspectRuns = aspectRun''
--                    , candidateAspectDocs = aspectDocs''
                   , candidatePages = [] --entityPages
                   }

    restrict :: (Eq a, Hashable a) => [a] -> HM.HashMap a b -> HM.HashMap a b
    restrict keys m =
        let m2 = HM.fromList [(k, ()) | k <- keys]
        in m `HM.intersection` m2

    uniqBy :: (Eq b, Hashable b) => (a->b) -> [a] -> [a]
    uniqBy keyF elems =
        HM.elems $ HM.fromList [ (keyF e, e) | e <- elems]

    -- goal: select the subset of entityRunEntries, edgesRunEntries, and edgeDocs that
    -- fulfill these criteria:
    --
    -- edgeDocs has entry in edgeRuns
    -- entities have entityRun entries
    -- But not:  entities have indicent edgeDocs
    --
    -- but otherwise edgeFeatures are only considered,
    -- if a) they belong to one indicent endgeDoc
    -- and b) they have an edgeRun entry

    paraIdToEdgeRun = HM.fromList [ (multiRankingEntryGetDocumentName run, run) | run <- edgeRun]
    aspectIdToAspectRun :: HM.HashMap AspectId (MultiRankingEntry AspectId GridRun)
    aspectIdToAspectRun = HM.fromList [ (multiRankingEntryGetDocumentName run, run) | run <- aspectRun]
    pageIdToEntityRun = [(multiRankingEntryGetDocumentName run, run)  | run <- entityRun]

    edgeDocs = edgeDocsLookup $ HM.keys paraIdToEdgeRun
--     aspectDocs = aspectDocsLookup $ HM.keys aspectIdToAspectRun -- todo cache aspect docs


    (edgeRun', edgeDocs')  = unzip
                                      $ [ (edgeEntry, edgeDoc)
                                        | edgeDoc <- edgeDocs
                                        , let paraId = edgeDocParagraphId edgeDoc
                                        , Just edgeEntry <- pure $ paraId `HM.lookup` paraIdToEdgeRun
                                        ]


--     (aspectRun', aspectDocs')  = unzip
--                                       $ [ (aspectEntry, aspectDoc)
--                                         | aspectDoc <- aspectDocs
--                                         , let aspectId = aspectDocAspectId aspectDoc
--                                         , Just aspectEntry <- pure $ aspectId `HM.lookup` aspectIdToAspectRun
--                                         ]

  -- todo add entities adjacent to edgedocs

    entityRun'  =  [ entry
                   | (docName, entry) <- pageIdToEntityRun
                   ,  not $ "enwiki:Category:" `T.isPrefixOf` (T.pack $ unpackPageId docName)
                   ]

    aspectRun'  =  [ entry
                   | (aspectName, entry) <- HM.toList aspectIdToAspectRun
--                    ,  not $ "enwiki:Category:" `T.isPrefixOf` (T.pack $ unpackPageId docName)
                   ]

    entityRun'' = uniqBy multiRankingEntryGetDocumentName (entityRun')-- <> entityRunFake')
    edgeRun'' = uniqBy multiRankingEntryGetDocumentName edgeRun'
    edgeDocs'' = uniqBy edgeDocParagraphId edgeDocs'
    aspectRun'' = uniqBy multiRankingEntryGetDocumentName aspectRun'
--     aspectDocs'' = uniqBy aspectDocAspectId aspectDocs'

    fakeMultiPageEntry query doc =
        buildMultiTrecEntry  1000 (0.0, [])



selectStrictCandidateGraph
    :: EdgeDocsLookup
    -> PagesLookup
    -> CandidateGraphGenerator
selectStrictCandidateGraph edgeDocsLookup pagesLookup _queryId edgeRun entityRun _aspectRun =
    Candidates { candidateEdgeDocs = edgeDocs''
               , candidateEdgeRuns = edgeRun''
               , candidateEntityRuns = entityRun''
               , candidatePages = entityPages
               , candidateAspectRuns = []
               }
  where
    restrict :: (Eq a, Hashable a) => [a] -> HM.HashMap a b -> HM.HashMap a b
    restrict keys m =
        let m2 = HM.fromList [(k, ()) | k <- keys]
        in m `HM.intersection` m2

    uniqBy :: (Eq b, Hashable b) => (a->b) -> [a] -> [a]
    uniqBy keyF elems =
        HM.elems $ HM.fromList [ (keyF e, e) | e <- elems]

    -- goal: select the subset of entityRunEntries, edgesRunEntries, and edgeDocs that
    -- fulfill these criteria:
    --
    -- edgeDocs has entry in edgeRuns
    -- entities have entityRun entries
    -- entities have indicent edgeDocs
    --
    -- but otherwise edgeFeatures are only considered,
    -- if a) they belong to one indicent endgeDoc
    -- and b) they have an edgeRun entry

    paraIdToEdgeRun = HM.fromList [ (multiRankingEntryGetDocumentName run, run) | run <- edgeRun]
    pageIdToEntityRun = [(multiRankingEntryGetDocumentName run, run)  | run <- entityRun]

    edgeDocs = edgeDocsLookup $ HM.keys paraIdToEdgeRun


    (entityRun', edgeRun', edgeDocs')  = unzip3
                                      $ [ (entityEntry, edgeEntry, edgeDoc)
                                        | (pageId, entityEntry) <- pageIdToEntityRun
                                        , edgeDoc <- edgeDocs
                                        , pageId `HS.member` (edgeDocNeighbors edgeDoc)
                                        , let paraId = edgeDocParagraphId edgeDoc
                                        , Just edgeEntry <- pure $ paraId `HM.lookup` paraIdToEdgeRun
                                        ]

    entityRun'' = uniqBy multiRankingEntryGetDocumentName entityRun'
    edgeRun'' = uniqBy multiRankingEntryGetDocumentName edgeRun'
    edgeDocs'' = uniqBy edgeDocParagraphId edgeDocs'
    entityPages :: [PageDoc]
    entityPages = pagesLookup $  HS.toList $ (HS.fromList entitiesFromRuns) `HS.union` entitiesFromEdgeDocs
        where entitiesFromRuns :: [PageId]
              entitiesFromRuns = fmap multiRankingEntryGetDocumentName entityRun''
              entitiesFromEdgeDocs :: HS.HashSet PageId
              entitiesFromEdgeDocs = mconcat (fmap edgeDocNeighbors edgeDocs'')

