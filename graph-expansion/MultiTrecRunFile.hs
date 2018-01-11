{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}


module MultiTrecRunFile where

import qualified Data.DList as DList
import Data.Foldable
import Data.Ord
import Data.List
import Data.Monoid
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified CAR.RunFile as RunFile
import CAR.Types


type Score = Double
type Rank = Int
type QueryId = RunFile.QueryId

-- data RankingEntry' doc = RankingEntry { carQueryId     :: !QueryId
--                                       , carDocument    :: doc
--                                       , carRank        :: !Int
--                                       , carScore       :: !Run.Score
--                                       , carMethodName  :: !MethodName
--                                       }
--                        deriving  (Show)

-- data RankingEntry = RankingEntry { queryId       :: !QueryId
--                                  , documentName  :: !DocumentName
--                                  , documentRank  :: !Rank
--                                  , documentScore :: !Score
--                                  , methodName    :: !MethodName
--                                  }
--                   deriving (Show)

type RankingEntry doc = RunFile.RankingEntry' doc -- CAR.RunFile definition

data MultiRankingEntry doc =  MultiRankingEntry { multiRankingEntryCollapsed :: !(RankingEntry doc)
                                                , multiRankingEntryAll       :: [RankingEntry doc]
                                                }




multiAsRankingEntry :: MultiRankingEntry doc -> RankingEntry doc
multiAsRankingEntry multiRankingEntry = multiRankingEntryCollapsed multiRankingEntry

-- Precondition: MethodNames need to be unique!

collapseRuns :: forall doc . (Eq doc, Hashable doc) =>  [[RankingEntry doc]]  -> M.Map QueryId [MultiRankingEntry doc]
collapseRuns runs =
    let listOfRunMaps :: M.Map QueryId [RankingEntry doc]
        listOfRunMaps = fmap toList . RunFile.groupByQuery $ concat runs

     in M.fromList $ [ (query, collapseRankings rankings)
                      | (query, rankings) <- M.toList listOfRunMaps  -- fetch rankings for query
                      ]


-- rankings: rank entries for the same query, across different run files
collapseRankings ::  forall doc . (Eq doc, Hashable doc) => [RankingEntry doc] -> [MultiRankingEntry doc]
collapseRankings rankingEntries =
    -- docid -> [ entries ]
    -- groupBy (docId) rankingEntries
    let groupByDoc = map snd $ HM.toList -- equivalent for HM.values
                  $ HM.fromListWith (<>)
                  $ [ ((RunFile.carDocument entry), DList.singleton entry ) |  entry <- rankingEntries ]

        -- with score but not sorted
        scoredMultiRankings :: [ ( Score,  [RankingEntry doc] ) ]
        scoredMultiRankings = [ ( aggregatedScore rankingsPerDoc', rankingsPerDoc')
                              | rankingsPerDoc <- groupByDoc
                              , let rankingsPerDoc' = toList rankingsPerDoc
                              ]


        -- sorted and with ranks
    in zipWith buildData [1 .. ]
       $ sortOn (Down . fst) scoredMultiRankings

  where buildData :: Rank -> (Score, [RankingEntry doc]) -> MultiRankingEntry doc
        buildData r (s, rankings) =  MultiRankingEntry {
                                    multiRankingEntryCollapsed =
                                        RunFile.RankingEntry { RunFile.carQueryId = qId
                                                             , RunFile.carDocument = docName
                                                             , RunFile.carRank = r
                                                             , RunFile.carScore = s
                                                             , RunFile.carMethodName = RunFile.MethodName "collapsed"
                                                             }
                                    , multiRankingEntryAll = rankings
                                    }
          where
            qId = RunFile.carQueryId $ head rankings
            docName = RunFile.carDocument $ head rankings



-- precondition: all RankingEntry share the same queryId
-- precondition2: all RankingEntry share the same documentName
aggregatedScore :: [RankingEntry doc] -> Score
aggregatedScore rankings =
    recipRankAggregation $ fmap  ( RunFile.carRank ) rankings
  where
    recipRankAggregation :: [Rank] -> Score
    recipRankAggregation rs =
        sum $ fmap recipRank rs
      where
          recipRank :: Int -> Double
          recipRank x | x < 0 = error $ "rank needs to be positive but is "++ (show x)
          recipRank r = 1.0/(realToFrac r+1)

--  Todo: compare or merge with MergeRankings.hs