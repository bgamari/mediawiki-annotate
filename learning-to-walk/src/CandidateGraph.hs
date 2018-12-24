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

import Control.Concurrent.Async
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable

import CAR.Types hiding (Entity)
import qualified CAR.RunFile as CarRun
import GridFeatures

import EdgeDocCorpus

import qualified CAR.RunFile
import MultiTrecRunFile


import Debug.Trace



-- --------------- Candidate graph  ------------------------
data Candidates = Candidates { candidateEdgeDocs :: [EdgeDoc]
                             , candidateEdgeRuns :: [MultiRankingEntry ParagraphId GridRun]
                             , candidateEntityRuns :: [MultiRankingEntry PageId GridRun]
                             }

type CandidateGraphGenerator =
     QueryId
    -> [MultiRankingEntry ParagraphId GridRun]
    -> [MultiRankingEntry PageId GridRun]
    -> Candidates



selectGenerousCandidateGraph
    :: EdgeDocsLookup
    -> CandidateGraphGenerator
selectGenerousCandidateGraph edgeDocsLookup _queryId edgeRun entityRun =
    Candidates { candidateEdgeDocs = edgeDocs''
               , candidateEdgeRuns = edgeRun''
               , candidateEntityRuns = entityRun''
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
    -- But not:  entities have indicent edgeDocs
    --
    -- but otherwise edgeFeatures are only considered,
    -- if a) they belong to one indicent endgeDoc
    -- and b) they have an edgeRun entry

    paraIdToEdgeRun = HM.fromList [ (multiRankingEntryGetDocumentName run, run) | run <- edgeRun]
    pageIdToEntityRun = [(multiRankingEntryGetDocumentName run, run)  | run <- entityRun]

    edgeDocs = edgeDocsLookup $ HM.keys paraIdToEdgeRun


    (edgeRun', edgeDocs')  = unzip
                                      $ [ (edgeEntry, edgeDoc)
                                        |edgeDoc <- edgeDocs
                                        , let paraId = edgeDocParagraphId edgeDoc
                                        , Just edgeEntry <- pure $ paraId `HM.lookup` paraIdToEdgeRun
                                        ]

  -- todo add entities adjacent to edgedocs

    entityRun'  =                  entityRun
    entityRun'' = uniqBy multiRankingEntryGetDocumentName (entityRun')-- <> entityRunFake')
    edgeRun'' = uniqBy multiRankingEntryGetDocumentName edgeRun'
    edgeDocs'' = uniqBy edgeDocParagraphId edgeDocs'

    fakeMultiPageEntry query doc =
        buildMultiTrecEntry  100 (0.0, [])

--     fakeMultiPageEntry query page =
--            MultiRankingEntry { multiRankingEntryCollapsed = fakePageEntry query page
--                              , multiRankingEntryAll       = []
--                                                               }
--     fakePageEntry query page =     CAR.RunFile.RankingEntry { carQueryId     = query
--                                          , carDocument    = page
--                                          , carRank        = 1000
--                                          , carScore       = 0.0
--                                          , carMethodName  = CAR.RunFile.MethodName $ "fake"
--                                          }


selectStrictCandidateGraph
    :: EdgeDocsLookup
    -> CandidateGraphGenerator
selectStrictCandidateGraph edgeDocsLookup _queryId edgeRun entityRun =
    Candidates { candidateEdgeDocs = edgeDocs''
               , candidateEdgeRuns = edgeRun''
               , candidateEntityRuns = entityRun''
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

