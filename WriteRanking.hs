{-# LANGUAGE OverloadedStrings #-}

module WriteRanking where

import Data.Ord (comparing)
import Data.List (sortBy, intersperse)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import CAR.Types

rankingLength = 100

formatEntityRankings :: T.Text -> T.Text -> [(PageId, Double)] -> TL.Text
formatEntityRankings runName queryId scoredItems =
      TB.toLazyText
    $ mconcat
    $ intersperse "\n"
    $ zipWith formatEntry [1..]
    $ take rankingLength
    $ toRanking
    $ scoredItems
  where formatEntry :: Int -> (PageId, Double) -> TB.Builder
        formatEntry rank (element, score) =
            mconcat
            $ intersperse " "
            [ TB.fromText queryId
            , "Q0"
            , TB.fromString $ unpackPageId element
            , TB.decimal rank
            , TB.realFloat score
            , TB.fromText runName]




toRanking ::  [(elem, Double)] -> [(elem, Double)]
toRanking =
    sortBy (flip $ comparing snd)

