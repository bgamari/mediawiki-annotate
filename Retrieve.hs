{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Retrieve
    ( Term
    , TermCounts
    , Doc(..)
    , computeTermCounts
    , retrieve
    , textToTokens'
    ) where

import Numeric.Log (ln)
import Data.Function (on)
import qualified Control.Foldl as Foldl
import Data.Bifunctor
import Data.Monoid
import Data.Functor.Identity
import Data.Binary
import Pipes
import qualified Pipes.Prelude as P.P
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T

import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.TopK
import SimplIR.Term as Term
import SimplIR.Tokenise
import SimplIR.Utils
import SimplIR.Types

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

computeTermCounts :: [Term] -> [Doc doc T.Text] -> TermCounts
computeTermCounts queryTerms docs =
    foldMap (foldMap $ textToTokens termFilter) docs
    <> foldMap oneTerm queryTerms
  where
    queryTermSet = HS.fromList $  queryTerms
    termFilter = (`HS.member` queryTermSet)

retrieve :: TermCounts -> [Term] -> [Doc doc T.Text]
         -> [Doc doc Double]
retrieve stats queryTerms docs =
        runIdentity
     $  foldProducer (Foldl.generalize $ topK 100)
     $  each docs
    >-> P.P.map (fmap $ textToTokens termFilter)
    >-> P.P.map (fmap (scoreDoc stats queryTerms))
  where
    queryTermSet = HS.fromList queryTerms
    termFilter = (`HS.member` queryTermSet)


scoreDoc :: TermCounts -> [Term] -> TermCounts
         -> Double
scoreDoc (TermCounts stats) queryTerms =
    \ (TermCounts docTokens) ->
      let docLength = toEnum $ sum docTokens
      in score docLength $ map (second realToFrac) $ HM.toList docTokens
  where
    score :: DocumentLength -> [ (Term, Double) ] -> Double
    score docLen docTerms =
      ln $ queryLikelihood smoothing (zip queryTerms (repeat 1)) docLen docTerms

    collectionLen = realToFrac $ sum stats
    termProb term =
        maybe (error $ "didn't see term " ++ show term) (\x -> realToFrac x / collectionLen) $ HM.lookup term stats
    smoothing = Dirichlet 100 termProb

