{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module EdgeDocIndex where

import GHC.Generics
import Data.Binary

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

data DocMeta = DocMeta { docMetaParagraphId :: ParagraphId
                       , docMetaArticleId :: PageId
                       , docMetaNeighbors :: [PageId]
                       }
             deriving (Show, Generic)

instance Binary DocMeta
deriving instance Binary ParagraphId
deriving instance Binary PageId

edgeDocMeta :: EdgeDoc -> DocMeta
edgeDocMeta EdgeDoc{..} = DocMeta { docMetaParagraphId = edgeDocParagraphId
                                  , docMetaArticleId  = edgeDocArticleId
                                  , docMetaNeighbors  = edgeDocNeighbors
                                  }

indexEdgeDocs :: FilePath -> [EdgeDoc] -> IO (OnDiskIndex DocMeta Int)
indexEdgeDocs indexPath edocs = do
    runSafeT $ Foldl.foldM (buildIndex 1024 indexPath) (map toDoc edocs)
  where
    toDoc edoc = (edgeDocMeta edoc, toks)
      where toks = M.fromListWith (+) [ (tok, 1) | tok <- tokenize $ edgeDocContent edoc ]
