{-# LANGUAGE OverloadedStrings #-}

module WriteRanking where

import Data.Semigroup
import Data.Ord (comparing)
import Data.List (sortBy, intersperse)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import CAR.Types

rankingLength :: Int
rankingLength = 1000

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


formatEntityPassageRankings :: T.Text -> T.Text -> [(PageId, Maybe ParagraphId, Double)] -> TL.Text
formatEntityPassageRankings runName queryId scoredItems =
      TB.toLazyText
    $ mconcat
    $ intersperse "\n"
    $ zipWith formatEntry [1..]
    $ take rankingLength
    $ toRanking
    $ map (\(a,b,c) -> ((a,b), c))
    $ scoredItems
  where formatEntry :: Int -> ((PageId, Maybe ParagraphId), Double) -> TB.Builder
        formatEntry rank ((entity, passage), score) =
            mconcat
            $ intersperse " "
            [ TB.fromText queryId
            , "Q0"
            , formatPsgEntity passage entity
            , TB.decimal rank
            , TB.realFloat score
            , TB.fromText runName]
        formatPsgEntity (Just passage) entity =
            TB.fromString (unpackParagraphId passage) <> "/" <> TB.fromString (unpackPageId entity)
        formatPsgEntity Nothing entity =
            TB.fromString (unpackPageId entity)


formatPassageRankings :: T.Text -> T.Text -> [(ParagraphId, Double)] -> TL.Text
formatPassageRankings runName queryId scoredItems =
      TB.toLazyText
    $ mconcat
    $ intersperse "\n"
    $ zipWith formatEntry [1..]
    $ take rankingLength
    $ toRanking
    $ scoredItems
  where formatEntry :: Int -> (ParagraphId, Double) -> TB.Builder
        formatEntry rank (element, score) =
            mconcat
            $ intersperse " "
            [ TB.fromText queryId
            , "Q0"
            , TB.fromString $ unpackParagraphId element
            , TB.decimal rank
            , TB.realFloat score
            , TB.fromText runName]



toRanking ::  [(elem, Double)] -> [(elem, Double)]
toRanking =
    sortBy (flip $ comparing snd)

