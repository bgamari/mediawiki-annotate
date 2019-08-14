-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields#-}

module Main where


import Options.Applicative
import Control.Monad
import GHC.Generics
import Control.Exception
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Hashable
import Data.Foldable
import Data.Time
import System.FilePath.Posix

import Options.Applicative
import qualified Data.Text as T

--import QRel as Q
import CAR.QRelFile as Q

import CAR.Types
import Types
import PageStats
import CAR.QRelFile

import Utils
import MisoPostprocessing.Utils

import SimplIR.Types.Relevance
type QrelEntry = Q.Annotation GradedRelevance

data SectionOrPageLevel = SectionLevel | PageLevel
    deriving (Eq, Show)
data IncludeFacets = IncludeFacetsOnlyHeading | IncludeFacetsOtherFacets
    deriving (Eq, Show)


opts :: Parser (IO ())
opts = subparser
    $ cmd "doIt" doIt'
  where cmd name action = command name (info (helper <*> action) fullDesc)
        doIt' = doIt
                  <$> many (argument str (metavar "AssessmentFILE" <> help "assessmentFile"))
                  <*> (option str (short 'o' <> long "out" <> metavar "Qrels" <> help "File where Qrels are written to") )
                  <*> flag PageLevel SectionLevel ( long "section-level" <> help "Write section-level qrels (default: page-level)" )
                  <*> flag IncludeFacetsOnlyHeading IncludeFacetsOtherFacets ( long "include-other-facets" <> help "Include intro and other facet in qrels (applies only to section-level qrels)" )


doIt :: [FilePath] -> FilePath -> SectionOrPageLevel -> IncludeFacets -> IO ()
doIt assessmentFiles outQrels sectionOrPageLevel includeFacets = do
    user2Assessment <- loadUserAssessments assessmentFiles
    when (or [length lst > 1 | ((u,q), lst) <- M.toList user2Assessment])
        $ fail "More than one assessment status per user/query. Please use merge-assessment-pages to merge multiple assessment files."

    let user2Assessment' = fmap head user2Assessment

    let qrelData = concat $ fmap (assessment2FacetQrels sectionOrPageLevel) $ M.elems user2Assessment'
    writeParagraphQRel outQrels qrelData
  where  assessment2FacetQrels sectionOrPageLevel (SavedAssessments { savedData = AssessmentState {facetState=facetState', nonrelevantState2=nonrelevantState2'}}) =
            let nrPageEntries =
                    [ CAR.QRelFile.Annotation sq p rel
                    | (key@AssessmentKey{queryId = q, paragraphId = p}
                      , AnnotationValue { value = v}
                      ) <- M.toList $ fromMaybe mempty nonrelevantState2'
                    , v  -- discard entries with value "False"
                    , let rel = fromJust $ assessmentLabelToGradedRelevance NonRelLabel
                    , let pageId = squidToQueryId q
                    , let sq = SectionPath {sectionPathPageId = pageId, sectionPathHeadings = [] }    -- build SectionPath
                    ]

                qrelEntries =
                    case sectionOrPageLevel of
                      SectionLevel ->
                        [ CAR.QRelFile.Annotation sq p rel'
                        | ( key@AssessmentKey{queryId = q, paragraphId = p}
                          , annValueList
                          ) <- M.toList facetState'
                        , not $ isNotRelevant key
                        , let pageId = squidToQueryId q
                        , AnnotationValue{ value = (FacetValue { facet = AssessmentFacet{apHeadingId = facetId}
                                                               , relevance = rel})
                                         } <- annValueList
                        , Just rel' <- pure $ assessmentLabelToGradedRelevance rel
                        , (includeFacets == IncludeFacetsOtherFacets) || ( facetId /= noneFacetId && facetId /= introFacetId)
                        , let sq = SectionPath {sectionPathPageId = pageId, sectionPathHeadings = [facetId] }    -- build SectionPath
                        ]
                      PageLevel ->
                        nrPageEntries <>
                        [ CAR.QRelFile.Annotation sq p rel'
                        | ( key@AssessmentKey{queryId = q, paragraphId = p}
                          , annValueList
                          ) <- M.toList facetState'
                        , not $ isNotRelevant key
                        , let pageId = squidToQueryId q
                        , let rel = maxRelevance annValueList
                        , Just rel' <- pure $ assessmentLabelToGradedRelevance rel
                        , let sq = SectionPath {sectionPathPageId = pageId, sectionPathHeadings = [] }    -- build SectionPath
                        ]
                entries = sortBy entriesOrdering $ qrelEntries
            in entries

           where isNotRelevant :: AssessmentKey -> Bool
                 isNotRelevant key =
                    let nrMap = fromMaybe mempty nonrelevantState2'
                    in unwrapMaybeAnnotationValue False $ key `M.lookup` nrMap


         maxRelevance :: [AnnotationValue FacetValue] -> AssessmentLabel
         maxRelevance annValueList =
            maximum
            $ fmap (\AnnotationValue{ value = (FacetValue {relevance = rel})} -> rel)
            $ annValueList

         entriesOrdering :: QrelEntry -> QrelEntry -> Ordering
         entriesOrdering (CAR.QRelFile.Annotation q1 d1 r1) (CAR.QRelFile.Annotation q2 d2 r2)  =
            case compare q1 q2 of
                EQ -> case compare d1 d2 of
                        EQ -> compare r1 r2
                        y -> y
                x -> x




main :: IO ()
main = join $ execParser' (helper <*> opts) mempty

execParser' :: Parser a -> InfoMod a -> IO a
execParser' parser pinfo =
    execParser $ info (parser) pinfo
