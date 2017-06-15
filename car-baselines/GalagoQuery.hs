{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Foldable
import Data.Bifunctor
import Data.Functor.Identity
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid
import GHC.Generics
import System.FilePath

import Data.Binary
import qualified Data.Binary.Serialise.CBOR as CBOR
import Data.Aeson as Aeson
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.DList as DList
import qualified Data.CharSet as CS
import qualified Data.CharSet.Common as CS
import qualified Numeric.Log as Log

import CAR.Types
import CAR.CarExports
import SimplIR.RetrievalModels.CorpusStats
import SimplIR.RetrievalModels.QueryLikelihood
import SimplIR.RetrievalModels.BM25 as BM25
import SimplIR.TopK
import SimplIR.Types
import SimplIR.Term as Term
import qualified SimplIR.Format.TrecRunFile as TrecRun
import SimplIR.Utils
import SimplIR.DiskIndex.Posting.Collect (collectPostings)
import qualified SimplIR.DiskIndex.Build as DiskIdx
import qualified SimplIR.DiskIndex as DiskIdx



import Pipes
import qualified Pipes.Prelude as P.P
import Pipes.Safe
import qualified Control.Foldl as Foldl
import Options.Applicative

import SimplIR.StopWords
import SimplIR.Tokenise



tokenize :: T.Text -> [Term]
tokenize text =
    let terms =    T.words
                   $ killCharSet notLatin1Letters  -- replaces with space
                   $ T.filter (/='\'')             -- drop chars without substitution
                   $ T.toCaseFold text
    in fmap Term.fromText
       $ filter (\t -> T.length t > 1)
       $ killStopwords enInquery
       $ terms
    where isAcronym  =
            T.all (`CS.member` CS.upper)
            . T.filter (`CS.member` acronymPunctuation)
          acronymPunctuation = CS.fromList ".-"


opts :: Parser (IO ())
opts =
    go <$> option str (long "output" <> short 'o' <> help "output file for galago query json format")
       <*> option str (long "outlines" <> short 'O' <> help "File containing page outlines to predict (one per line)")
  where
    go outFile outlinesFile = do
        outlines <- readCborList outlinesFile :: IO [Stub]
        BSL.writeFile outFile $ Aeson.encode $ foldMap stubToGalagoQuery outlines

type GalagoQueryId = T.Text
type GalagoQueryText = T.Text

stubToGalagoQuery :: Stub -> GalagoQuerySet
stubToGalagoQuery outline =
    GalagoQuerySet $ fmap (uncurry querySection) $ stubPaths outline
  where
    querySection :: [Term] -> SectionPath -> GalagoQuery
    querySection queryTerms sectionPath =
            let queryId = T.pack $ escapeSectionPath sectionPath
                queryText =  "#sdm ( " --"$rm (
                          <> T.intercalate " " (map Term.toText queryTerms)
                          <> " )"
            in GalagoQuery queryId queryText

data GalagoQuery = GalagoQuery { galagoQueryId :: GalagoQueryId
                               , galagoQueryText :: GalagoQueryText
                               }

instance ToJSON GalagoQuery where
    toJSON q = object [ "number" .= galagoQueryId q, "text" .= galagoQueryText q ]

newtype GalagoQuerySet = GalagoQuerySet [GalagoQuery]
                       deriving (Monoid)

instance ToJSON GalagoQuerySet where
    toJSON (GalagoQuerySet qs) = object [ "queries" .= qs ]

stubPaths :: Stub -> [([Term], SectionPath)]
stubPaths (Stub pageName pageId skel) = foldMap (go mempty (titleWords pageName)) skel
  where
    go :: DList.DList HeadingId -> [Term] -> PageSkeleton -> [([Term], SectionPath)]
    go _ _ (Para _) = [] -- this should really never happen
    go _ _ (Image {}) = [] -- this should really never happen
    go parents parentTerms (Section heading headingId children) =
        (terms, SectionPath pageId (toList me)) : foldMap (go me terms) children
      where
        terms = parentTerms <> headingWords heading
        me = parents `DList.snoc` headingId

    headingWords :: SectionHeading -> [Term]
    headingWords (SectionHeading t) = tokenize t
    titleWords (PageName t) = tokenize t

main :: IO ()
main = do
    mode <- execParser $ info (helper <*> opts) fullDesc
    mode

