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

pageStats ::  QueryId -> [ParagraphId] -> AssessmentState -> MissingAssessmentStats
pageStats queryId' paragraphIds (AssessmentState { transitionLabelState = transitionState'
                          , nonrelevantState = hiddenState'
                          , nonrelevantState2 = hiddenState2'
                          , notesState = notesState'
                          , facetState = facetState'
                          }) =
    MissingAssessmentStats { --queryId = apSquid
                           numMissingFacetAsessments = numMissingFacetAsessments
                           , numMissingTransitionAssessments = numMissingTransitionAssessments
                           }
          where visibleParas = -- Debug.traceShowId $
                               [ paraId
                                | paraId  <- paragraphIds
--                                , let hiddenEntry = AssessmentKey{paragraphId = paraId, queryId = queryId'} `M.lookup` (fromMaybe mempty hiddenState2')
--                                , unwrapMaybeAnnotationValue False hiddenEntry == False
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

                numMissingFacetAsessments :: Int
                numMissingFacetAsessments = length
                                          $ [ paraId
                                            | paraId  <- L.nub visibleParas
                                            , let entry = AssessmentKey{paragraphId = paraId, queryId = queryId'} `M.lookup` facetState'
                                            , let facetValues = unwrapMaybeAnnotationValueList defaultFacetValues entry
                                            , L.all (\FacetValue{relevance = rel} -> rel == UnsetLabel) facetValues
                                            ]
                numMissingTransitionAssessments :: Int
                numMissingTransitionAssessments = length
--                                                $ Debug.traceShowId
                                                $ [x
                                                | x@[paraId1, paraId2] <- L.nub $ slidingWindow 2 visibleParas
                                                , let transitionEntry =  (AssessmentTransitionKey {paragraphId1 = paraId1, paragraphId2=paraId2, queryId = queryId'} `M.lookup` transitionState')
                                                , unwrapMaybeAnnotationValue UnsetTransition transitionEntry == UnsetTransition
                                                ]