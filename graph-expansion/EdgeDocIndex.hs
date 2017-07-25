module EdgeDocIndex where

import Data.Maybe

import Pipes
import qualified Pipes.Prelude as PP
import Pipes.Safe
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Control.Foldl as Foldl
import qualified Data.Binary.Serialise.CBOR as S
import System.FilePath

import CAR.Types
import CAR.Retrieve
import NLP.Snowball
import EdgeDocCorpus

import SimplIR.Types (DocumentId)
import SimplIR.DiskIndex.Build
import SimplIR.DiskIndex.Posting.Collect
import SimplIR.DiskIndex as DiskIndex
import SimplIR.StopWords
import SimplIR.Tokenise
import SimplIR.Term as Term

indexEdgeDocs :: FilePath -> [EdgeDoc] -> IO (TermCounts, OnDiskIndex EdgeDoc Int)
indexEdgeDocs indexPath edocs = do
    runSafeT $ Foldl.foldM ((,) <$> Foldl.generalize (Foldl.premap (toTermCounts . snd) $ Foldl.mconcat)
                                <*> buildIndex 1024 indexPath)
                           (map toDoc edocs)
  where
    toTermCounts = TermCounts . HM.fromList . M.toList
    toDoc :: EdgeDoc -> (EdgeDoc, M.Map Term Int)
    toDoc edoc = (edoc, toks)
      where toks = M.fromListWith (+) [ (tok, 1) | tok <- textToTokens' $ edgeDocContent edoc ]

retrievalRanking :: FilePath -> IO ([Term] -> [(Double, EdgeDoc)])
retrievalRanking indexPath = do
    idx <- DiskIndex.openOnDiskIndex (OnDiskIndex indexPath)
    bgStats <- S.deserialise <$> BSL.readFile (indexPath <.> "stats")
    let retrieve :: [Term] -> [(Double, EdgeDoc)]
        retrieve queryTerms =
            let queryCounts = foldMap oneTerm queryTerms

                postings :: [(DocumentId, [(Term, Int)])]
                postings =
                      PP.toList $ collectPostings
                    [ (term, Pipes.each $ fromMaybe [] $ DiskIndex.lookupPostings' term idx)
                    | term <- queryTerms
                    ]

                score docTerms = scoreDoc bgStats queryTerms docTerms
            in [ (score $ fromCounts counts, doc)
               | (docId, counts) <- postings
               , Just doc <- pure $ lookupDoc docId idx
               ]
    return retrieve
