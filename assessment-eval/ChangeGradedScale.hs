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


import qualified SimplIR.Format.QRel as QRel
import AssessmentEval

data ExportType = Entity | Psg | EntityPsg
    deriving (Show, Enum, Bounded,Eq, Read)



-- annotator, date, query, docid, relevance
-- bucket on (query, docid)
-- for each bucket, prefer the relevance that is: annotator1 (later=better), otherwise any annotator (later=better)
-- i.e. sort each bucket by 1. annotator (asc), 2. date (desc)

opts :: Parser ([FilePath], FilePath, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int, Maybe Int)
opts =
    (,,,,,,,,,)
    <$> some (argument str (metavar "assessments" <> help "A glob pattern for assessments"))
    <*> option str (short 'o' <> long "output" <> help "Output file for entity qrels")
    <*> optional (option auto (short '0'  <> metavar "INT" <> help "change grade for 0") )
    <*> optional (option auto  (short '1'  <> metavar "INT" <> help "change grade for 1") )
    <*> optional (option auto  (short '2'  <> metavar "INT" <> help "change grade for 2") )
    <*> optional (option auto  (short '3'  <> metavar "INT" <> help "change grade for 3") )
    <*> optional (option auto  (short '4'  <> metavar "INT" <> help "change grade for 4") )
    <*> optional (option auto  (short '5'  <> metavar "INT" <> help "change grade for 5") )
    <*> optional (option auto  (long "neg1"  <> metavar "INT" <> help "change grade for -1"))
    <*> optional (option auto  (long "neg2"  <> metavar "INT" <> help "change grade for -2"))

main :: IO ()
main = do
    (files, outputFile, newGrade0, newGrade1, newGrade2, newGrade3, newGrade4, newGrade5, newGradeNeg1, newGradeNeg2) <- execParser $ info (helper <*> opts) mempty
    (anns :: HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance)
          <- HM.unions <$> mapM readAssessments files

    let gradeMap = HM.fromList
                 $ [ 0 .= newGrade0
                   , 1 .= newGrade1
                   , 2 .= newGrade2
                   , 3 .= newGrade3
                   , 4 .= newGrade4
                   , 5 .= newGrade5
                   , (-1) .= newGradeNeg1
                   , (-2) .= newGradeNeg2
                   ]
          where x .= y = (QRel.GradedRelevance x, y)

    let changeGrade rel =
              case rel `HM.lookup` gradeMap of
                Just (Just newGrade) -> QRel.GradedRelevance newGrade
                _                    -> rel

    let filteredAnns =  [ QRel.Entry queryId docId  (changeGrade rel)
                        | ((QueryId queryId, DocumentId docId), rel) <- HM.toList anns
                        ]


    QRel.writeQRel outputFile filteredAnns

  