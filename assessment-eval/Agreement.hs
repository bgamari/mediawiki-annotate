{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Data.Maybe
import Data.Semigroup
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Options.Applicative
import System.FilePath

import qualified SimplIR.Format.QRel as QRel
import SimplIR.Assessment.Agreement
import AssessmentEval

opts :: Parser [FilePath]
opts = some $ argument str (metavar "QREL" <> help "A qrel file with judgements from a single assessor")

main :: IO ()
main = do
    files <- execParser $ info  (helper <*> opts) mempty
    let readAssessor path = do
            as <- readAssessments path
            let toBinary (QRel.GradedRelevance n)
                  | n > 2     = QRel.Relevant
                  | otherwise = QRel.NotRelevant
            return $ HM.singleton (assessorFromFilepath path) (fmap toBinary as)

    assessments <- HM.unions <$> mapM readAssessor files
    putStrLn $ "Assessment counts: "++show (fmap HM.size assessments)
    putStrLn "Cohen:"
    putStrLn $ unlines [ show a <> "\t" <> show b <> "\t" <> show (cohenKappa a' b')
                       | (a, a') <- HM.toList assessments
                       , (b, b') <- HM.toList assessments
                       -- , a < b
                       ]
    putStrLn $ "Fleiss: "<>show (fleissKappa $ HM.elems assessments)
    return ()
