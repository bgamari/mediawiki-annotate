module EdgeDocIndex where

import Pipes.Safe
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Control.Foldl as Foldl

import CAR.Types
import NLP.Snowball
import EdgeDocCorpus
import SimplIR.DiskIndex.Build
import SimplIR.DiskIndex
import SimplIR.StopWords
import SimplIR.Tokenise
import SimplIR.Term as Term

tokenize :: T.Text -> [Term]
tokenize =
      fmap Term.fromText
    . stems English
    . killStopwords enInquery
    . T.words
    . T.toCaseFold
    . T.filter (/= '\'')
    . killPunctuation
    . killCharSet notLatin1Letters

indexEdgeDocs :: FilePath -> [EdgeDoc] -> IO (OnDiskIndex EdgeDoc Int)
indexEdgeDocs indexPath edocs = do
    runSafeT $ Foldl.foldM (buildIndex 1024 indexPath) (map toDoc edocs)
  where
    toDoc edoc = (edoc, toks)
      where toks = M.fromListWith (+) [ (tok, 1) | tok <- tokenize $ edgeDocContent edoc ]
