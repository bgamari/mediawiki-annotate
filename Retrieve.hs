{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Retrieve
    ( Score
    , TermCounts
    , Doc(..)
    , computeTermCounts
    , retrieve
    ) where

import Numeric.Log (ln)
import Data.Function (on)
import qualified Control.Foldl as Foldl
import Data.Bifunctor
import Data.Binary
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.TopK
import SimplIR.Term as Term
import SimplIR.Tokenise

newtype TermCounts = TermCounts (HM.HashMap Term Int)
                    deriving (Show)

instance Binary TermCounts where
    get = TermCounts . HM.fromList <$> get
    put (TermCounts x) = put $ HM.toList x

instance Monoid TermCounts where
    mempty = TermCounts mempty
    TermCounts a `mappend` TermCounts b = TermCounts (HM.unionWith (+) a b)

data Doc meta a = Doc { docMeta :: meta, docThing :: a }
                deriving (Show, Functor, Foldable)

instance Eq a => Eq (Doc meta a) where
    (==) = (==) `on` docThing

instance Ord a => Ord (Doc meta a) where
    compare = compare `on` docThing

oneTerm :: Term -> TermCounts
oneTerm t = TermCounts $ HM.singleton t 1

type TermFilter = Term -> Bool

textToTokens' :: T.Text -> [Term]
textToTokens' = map Term.fromText . tokenise

textToTokens :: TermFilter -> T.Text -> TermCounts
textToTokens termFilter = foldMap oneTerm . filter termFilter . textToTokens'

computeTermCounts :: T.Text -> [Doc doc T.Text] -> TermCounts
computeTermCounts queryTerms = foldMap (foldMap $ textToTokens termFilter)
  where
    queryTermSet = HS.fromList $ map Term.fromText $ tokenise queryTerms
    termFilter = (`HS.member` queryTermSet)

retrieve :: TermCounts -> T.Text -> [Doc doc T.Text]
         -> [Doc doc Double]
retrieve stats query docs =
    Foldl.fold (topK 100)
    $ map (fmap (scoreDoc stats queryTerms))
    $ map (fmap $ textToTokens termFilter)
    $ docs
  where
    queryTerms = textToTokens' query
    queryTermSet = HS.fromList queryTerms
    termFilter = (`HS.member` queryTermSet)


scoreDoc :: TermCounts -> [Term] -> TermCounts
         -> Double
scoreDoc (TermCounts stats) queryTerms =
    \ (TermCounts tokens) ->
      let docLength = toEnum $ sum tokens
      in score docLength $ map (second realToFrac) $ HM.toList tokens
  where
    score docLen = ln . queryLikelihood smoothing (zip queryTerms (repeat 1)) docLen

    total = realToFrac $ sum stats
    termProb term =
        maybe (error "didn't see term") (\x -> realToFrac x / total) $ HM.lookup term stats
    smoothing = Dirichlet 100 termProb
