{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Retrieve
    ( Score
    , Term
    , TermCounts(..)
    , Doc(..)
    , computeTermCounts
    , retrieve
    , textToTokens'
    ) where

import Numeric.Log (ln)
import Data.Function (on)
import Data.List (sortBy)
import qualified Control.Foldl as Foldl
import Data.Bifunctor
import Data.Monoid
import Data.Binary
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.CharSet as CS
import qualified Data.CharSet.Common as CS

import NLP.Snowball
import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.TopK
import SimplIR.Term as Term
import SimplIR.Tokenise
import SimplIR.Types
import SimplIR.StopWords


newtype TermCounts = TermCounts { getTermCounts :: HM.HashMap Term Int }

instance Show TermCounts where
    showsPrec _ = shows . sortBy (flip (compare `on` snd)) . HM.toList . getTermCounts

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


--
-- bothStemAndOrig :: Term -> [Term]
-- bothStemAndOrig term =
--     [stems English [term], term]

textToTokens' :: T.Text -> [Term]
-- textToTokens' = map Term.fromText .  bothStemAndOrig . killStopwords enInquery . tokenise
textToTokens' text =
    let acronyms = fmap (T.filter (`CS.member` acronymPunctuation))
                   $ filter isAcronym
                   $ T.words
                   $ text
        rawterms = T.words
                   $ T.filter (/='\'')
                   $ T.filter (/='"')
                   $ T.toCaseFold text
        terms =    T.words
                   $ killCharSet notLatin1Letters  -- replaces with space
                   $ T.filter (/='\'')             -- drop chars without substitution
                   $ T.toCaseFold text
        stemmedTerms = stems English terms
    in fmap Term.fromText --trace ("terms: "++show terms ++ "\nrawterms: "++ show rawterms ++ "\nacronyms: "++ show acronyms)
       $ filter (\t -> T.length t > 1)
       $ killStopwords enInquery
       $ acronyms ++ rawterms ++ terms ++ stemmedTerms
    where isAcronym  =
            T.all (`CS.member` CS.upper)
            . T.filter (`CS.member` acronymPunctuation)
          acronymPunctuation = CS.fromList ".-"

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
    Foldl.fold (topK 100)
    $ map (fmap (scoreDoc stats queryTerms))
    $ map (fmap $ textToTokens termFilter)
    $ docs
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

