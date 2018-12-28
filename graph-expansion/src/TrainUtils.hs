{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}

module TrainUtils
  ( -- * Folds
    Folds(..)
  , FoldIdx
  , mkSequentialFolds
  , kFolds
    -- * Restarts
  , Restarts(..)
  , RestartIdx
  , kFoldsAndRestarts
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Control.DeepSeq
import System.Random

newtype Folds a = Folds { getFolds :: [a] }
                deriving (Foldable, Functor, Traversable)
                deriving newtype (NFData)

newtype FoldIdx = FoldIdx Int
                deriving (Eq, Ord, Show, Enum)

numberFolds :: Folds a -> Folds (FoldIdx, a)
numberFolds (Folds xs) = Folds $ zip [FoldIdx 0..] xs

mkSequentialFolds :: Int -> [a] -> Folds [a]
mkSequentialFolds k xs = Folds $ chunksOf foldLen xs
  where
    foldLen
      | len >= 2 * k = (len `div` k) + 1  -- usual case: prevents overpopulation of last fold, e.g. [1,2] [3,4] [5,6] [7]
      | otherwise = len `div` k  -- to prevent last folds to be empty, accept overpopulation of last fold, e.g. [1] [2] [3] [4] [5,6,7]
    len = length xs

-- | A list of restarts.
newtype Restarts a = Restarts { getRestarts :: [a] }
                   deriving (Foldable, Functor, Traversable)
                   deriving newtype (NFData)

newtype RestartIdx = RestartIdx Int
                   deriving (Eq, Ord, Show, Enum)

numberRestarts :: Restarts a -> Restarts (RestartIdx, a)
numberRestarts (Restarts xs) = Restarts $ zip [RestartIdx 0..] xs

mkRestartSeeds :: StdGen -> Restarts StdGen
mkRestartSeeds = Restarts . unfoldr (Just . System.Random.split)

-- r might be: [(Model, Double)]


-- TODO think about turning Fold[q] into Fold (S.Set q)
kFolds
    :: forall q d r. (Eq q, Ord q)
    => (FoldIdx -> M.Map q d -> r)
       -- ^ a training function, producing a trained result from a
       -- set of training data. 'FoldIdx' provided for diagnostics.
    -> M.Map q d
       -- ^ training data
    -> Folds [q]
       -- ^ partitioning of queries into folds
    -> Folds (M.Map q d, r)
       -- ^ the training result and set of test data for the fold
kFolds train allData foldQueries =
    fmap trainSingleFold (numberFolds foldQueries)
  where
    trainSingleFold :: (FoldIdx, [q]) -> (M.Map q d, r)
    trainSingleFold (foldIdx, testQueries) =
      let testData :: M.Map q d
          testQueries' = S.fromList testQueries
          testData =  M.filterWithKey (\query _ -> query `S.member` testQueries') allData

          trainData :: M.Map q d
          trainData =  M.filterWithKey (\query _ -> query `S.notMember` testQueries') allData
      in (testData, train foldIdx trainData)



kFoldsAndRestarts
    ::  forall q d r. (Eq q, Ord q)
    => (FoldIdx -> RestartIdx
        -> StdGen -> M.Map q d -> r)
        -- ^ a training function, producing a trained result from a
        -- set of training data. 'FoldIdx' and 'RestartIdx' provided for
        -- diagnostics.
    -> M.Map q d
        -- ^ training data
    -> Folds [q]
        -- ^ partitioning of queries into folds
    -> StdGen
        -- ^ random generator
    -> Folds (M.Map q d, Restarts r)
        -- ^ the training result and set of test data for the fold
kFoldsAndRestarts train allData foldQueries gen0 =
    kFolds train' allData foldQueries
  where
    train' :: FoldIdx -> M.Map q d -> Restarts r
    train' foldIdx trainData =
        fmap (\(restartIdx, gen) -> train foldIdx restartIdx gen trainData)
             (numberRestarts gens)

    gens :: Restarts StdGen
    gens = mkRestartSeeds gen0
