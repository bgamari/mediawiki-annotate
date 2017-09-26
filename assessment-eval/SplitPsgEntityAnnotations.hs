{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Data.Maybe
import Data.Semigroup hiding (option)
import Data.Hashable
import Data.List
import Data.Function
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Options.Applicative
import System.FilePath
import Data.Time.Clock


import qualified SimplIR.Format.QRel as QRel
import AssessmentEval

data ExportType = Entity | Psg | EntityPsg
    deriving (Show, Enum, Bounded,Eq, Read)



-- annotator, date, query, docid, relevance
-- bucket on (query, docid)
-- for each bucket, prefer the relevance that is: annotator1 (later=better), otherwise any annotator (later=better)
-- i.e. sort each bucket by 1. annotator (asc), 2. date (desc)

opts :: Parser ([FilePath], FilePath, ExportType)
opts =
    (,,)
    <$> some (argument str (metavar "assessments" <> help "A glob pattern for assessments"))
    <*> option str (short 'o' <> long "output" <> help "Output file for entity qrels")
    <*> option auto (short 'x' <> long "export" <> help "Type of export: Entity Psg EntityPsg")

main :: IO ()
main = do
    (files, outputFile, exportType) <- execParser $ info opts mempty
    (anns :: HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance)
          <- HM.unions <$> mapM readAssessments files

    let filteredAnns =  case exportType of
                          Entity ->  [ QRel.Entry queryId entityId  rel
                                     | ((QueryId queryId, DocumentId docId), rel) <- HM.toList anns
                                     , [psgId, entityId]  <- pure $ docId `T.splitOn` "/"  -- contains both arguments
                                     ]
                          Psg ->     [ QRel.Entry queryId psgId  rel
                                     | ((QueryId queryId, DocumentId docId), rel) <- HM.toList anns
                                     , [psgId]  <- pure $ docId `T.splitOn` "/"            -- contains only psg
                                     ]
                          EntityPsg ->  [ QRel.Entry queryId docId  rel
                                        | ((QueryId queryId, DocumentId docId), rel) <- HM.toList anns
                                        , [psgId, entityId]  <- pure $ docId  `T.splitOn` "/"
                                        ]


    QRel.writeQRel outputFile filteredAnns

  
