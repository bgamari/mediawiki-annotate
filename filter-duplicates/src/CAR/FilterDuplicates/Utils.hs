{-# LANGUAGE RankNTypes #-}

module CAR.FilterDuplicates.Utils where

import Data.Char
import Data.Maybe
import Data.List (tails)
import Debug.Trace

import Control.Lens
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as M
import Control.Parallel.Strategies

import CAR.Types
import SimplIR.StopWords
import NLP.Snowball

type Term = T.Text

listStatus :: String -> Int -> [a] -> [a]
listStatus str period = go 0 period
  where
    go m 0 (x:xs) = trace (str ++ ": "++show (period*m)) (x : go (m+1) period xs)
    go m n (x:xs) = x : go m (n-1) xs
    go _ _ []     = []

tokenise :: TL.Text -> [Term]
tokenise =
    stems English
    . killStopwords enInquery
    . map TL.toStrict
    . TL.words
    . TL.toCaseFold
    . TL.filter (not . isPunctuation)

toBigrams :: [Term] -> [(Term, Term)]
toBigrams = mapMaybe f . tails
  where
    f (x:y:_) = Just (x,y)
    f _ = Nothing

chunksOf :: Int -> V.Vector a -> [V.Vector a]
chunksOf n = go
  where
    go xs
      | V.null xs       = []
      | V.length xs < n = [xs]
      | otherwise       =
        let (a,b) = V.splitAt n xs
        in a : go b

jaccard :: HS.HashSet (Term, Term) -> HS.HashSet (Term, Term) -> Double
jaccard xs ys
  | denom == 0 = 0
  | otherwise  = num / denom
  where
    num = realToFrac $ HS.size (xs `HS.intersection` ys)
    denom = realToFrac $ HS.size (xs `HS.union` ys)

parVectorChunksOf :: Int -> Strategy (V.Vector a) -> Strategy (V.Vector a)
parVectorChunksOf chunkSz s =
    \v -> V.fromListN (V.length v) . foldMap V.toList <$> evalList s (chunksOf chunkSz v)

parseDuplicates :: String -> [(ParagraphId, ParagraphId)]
parseDuplicates = map (toPair . words) . filter (not . null) . lines
  where
    toPair [a,b] = (packParagraphId a, packParagraphId b)
    toPair x = error $ "parseDuplicates: "++show x

internTerms :: Traversal' a Term -> a -> a
internTerms trv = snd . mapAccumLOf trv f mempty
  where
    f :: M.Map Term Term -> Term -> (M.Map Term Term, Term)
    f acc t
      | Just t' <- acc ^. at t = (acc, t')
      | otherwise = (M.insert t t acc, t)
