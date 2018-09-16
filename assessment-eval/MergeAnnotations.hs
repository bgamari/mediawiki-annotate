{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Data.Semigroup hiding (option)
import qualified Data.HashMap.Strict as HM
import Options.Applicative
import Data.Time.Clock


import qualified SimplIR.Format.QRel as QRel
import AssessmentEval


-- annotator, date, query, docid, relevance
-- bucket on (query, docid)
-- for each bucket, prefer the relevance that is: annotator1 (later=better), otherwise any annotator (later=better)
-- i.e. sort each bucket by 1. annotator (asc), 2. date (desc)

opts :: Parser ([FilePath], FilePath)
opts =
    (,)
    <$> some (argument str (metavar "assessments" <> help "A glob pattern for assessments"))
    <*> option str (short 'o' <> long "output" <> help "Output file for qrels")

main :: IO ()
main = do
    (files, outputFile) <- execParser $ info  (helper <*> opts) mempty
    let readAssessmentTuples :: FilePath -> IO [((QueryId, DocumentId), ((Assessor, UTCTime), QRel.GradedRelevance))]
        readAssessmentTuples path = do
            anns <- readAssessments path

            return $ [ ((queryId, documentId), ((annotator, date), relevance))
                     | ((queryId, documentId), relevance) <- HM.toList anns ]
          where annotator = assessorFromFilepath path
                date = dateFromFilepath path



    nestedAssessments <- mapM readAssessmentTuples files
    let assessmentsMerged = HM.fromListWith mergeBucket $ mconcat nestedAssessments
    let result =  [ QRel.Entry queryId  documentId relevance
                  | ((QueryId queryId, DocumentId documentId), ((annotator, date), relevance)) <- HM.toList assessmentsMerged ]

    QRel.writeQRel outputFile result

  where mergeBucket d1@((annotator1, date1), relevance1) d2@((annotator2, date2), relevance2) =
           if annotator1 < annotator2 then d1
           else if date1 > date2 then d1
           else d2

