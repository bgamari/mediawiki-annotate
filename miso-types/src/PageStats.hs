{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields#-}
{-# LANGUAGE StandaloneDeriving #-}

module PageStats where


import CAR.Types
import Types

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Maybe



slidingWindow :: Int -> [a] -> [[a]]
slidingWindow chunkSize lst =
    go lst
  where go :: [a] -> [[a]]
        go lst' | length lst' <= chunkSize = [lst']
        go lst' = (L.take chunkSize lst': go (L.drop 1 lst'))

defaultFacetValue :: FacetValue
defaultFacetValue = FacetValue {facet = noneFacet, relevance = UnsetLabel}
defaultFacetValues :: [FacetValue]
defaultFacetValues = [defaultFacetValue]


data MissingAssessmentStats =
    MissingAssessmentStats { --queryId :: QueryId
                           numMissingFacetAsessments:: Int
                           , numMissingTransitionAssessments :: Int
                           }
    deriving (Eq, Show)


convertToParagraphIds :: AssessmentPage -> [ParagraphId]
convertToParagraphIds AssessmentPage{..} =
    [ paraId |  Paragraph{paraId = paraId}  <- apParagraphs]

data PageAssessmentData = PageAssessmentData {
                            visibleParas :: [ParagraphId]
                            , removedParagraphs :: [ParagraphId]
                            , facetAssessments :: [(ParagraphId, [FacetValue])]
                            , transitionAssessments :: [(ParagraphId, ParagraphId, Maybe AssessmentTransitionLabel)]
                          }


pageAssessments ::  QueryId -> [ParagraphId] -> AssessmentState -> PageAssessmentData
pageAssessments queryId' paragraphIds (AssessmentState { transitionLabelState = transitionState'
                          , nonrelevantState = hiddenState'
                          , nonrelevantState2 = hiddenState2'
                          , notesState = notesState'
                          , facetState = facetState'
                          }) =
    PageAssessmentData {..}
  where visibleParas = [ paraId
                        | paraId  <- paragraphIds
                        , isVisible  (AssessmentKey{paragraphId = paraId, queryId = queryId'})
                        ]
        isVisible :: AssessmentKey -> Bool
        isVisible key =
            if (null hiddenState2') && (not $ null hiddenState') then
                -- this is still an old model.
                not $ key `M.member` hiddenState'
            else
                -- this is the new hidden model
                let hiddenEntry = key `M.lookup` (fromMaybe mempty hiddenState2')
                in unwrapMaybeAnnotationValue False hiddenEntry == False

        facetAssessments :: [(ParagraphId, [FacetValue])]
        facetAssessments =   [ (paraId, facetValues)
                            | paraId  <- L.nub visibleParas
                            , let entry = AssessmentKey{paragraphId = paraId, queryId = queryId'} `M.lookup` facetState'
                            , let facetValues = unwrapMaybeAnnotationValueList defaultFacetValues entry
                            ]
        transitionAssessments :: [(ParagraphId, ParagraphId, Maybe AssessmentTransitionLabel)]
        transitionAssessments = [ (paraId1, paraId2, fmap unwrapAnnotationValue transitionEntry)
                                | x@[paraId1, paraId2] <- L.nub $ slidingWindow 2 visibleParas
                                , let transitionEntry =  (AssessmentTransitionKey {paragraphId1 = paraId1, paragraphId2=paraId2, queryId = queryId'} `M.lookup` transitionState')
                                ]

        removedParagraphs = [paraId
                            | paraId <- paragraphIds
                            , not $ isVisible  (AssessmentKey{paragraphId = paraId, queryId = queryId'})
                            ]

pageStats ::  QueryId -> [ParagraphId] -> AssessmentState -> MissingAssessmentStats
pageStats queryId' paragraphIds assessmentState =
    let PageAssessmentData{..} = pageAssessments queryId' paragraphIds assessmentState
        numMissingFacetAsessments :: Int
        numMissingFacetAsessments = length
                                  $ [ 1
                                    | (_, facetValues) <- facetAssessments
                                    , L.all (\FacetValue{relevance = rel} -> rel == UnsetLabel) facetValues
                                    ]
        numMissingTransitionAssessments :: Int
        numMissingTransitionAssessments = length
--                                                $ Debug.traceShowId
                                        $ [ 1
                                        | (_, _, transitionEntry ) <- transitionAssessments
                                        , fromMaybe UnsetTransition transitionEntry == UnsetTransition
                                        ]

    in MissingAssessmentStats { numMissingFacetAsessments = numMissingFacetAsessments
                              , numMissingTransitionAssessments = numMissingTransitionAssessments
                              }

