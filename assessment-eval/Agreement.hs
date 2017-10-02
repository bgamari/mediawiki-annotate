{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.Maybe
import Data.Semigroup hiding (option)
import Numeric

import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Options.Applicative
import System.FilePath

import Text.Tabular
import qualified Text.Tabular.Latex as Latex
import qualified Text.Tabular.AsciiArt as AsciiArt
import qualified Text.Tabular.SimpleText as SimpleText

import qualified SimplIR.Format.QRel as QRel
import SimplIR.Assessment.Agreement
import AssessmentEval

newtype TableRenderer = TableRenderer (forall rh ch a. (rh -> String) -> (ch -> String) -> (a -> String) -> Table rh ch a -> String)


tableRenderer :: Parser TableRenderer
tableRenderer =
    option (str >>= parse) (long "table" <> value (TableRenderer AsciiArt.render))
  where
    parse "latex" = pure $ TableRenderer Latex.render
    parse "tsv"   = pure $ TableRenderer $ SimpleText.render "\t"
    parse "ascii" = pure $ TableRenderer AsciiArt.render
    parse s       = fail $ concat ["unknown table rendering method "
                                  , s
                                  , "; expected 'latex', 'tsv', or 'ascii'"]

opts :: Parser ([FilePath], TableRenderer)
opts =
    (,)
    <$> some (argument str (metavar "QREL" <> help "A qrel file with judgements from a single assessor"))
    <*> tableRenderer


main :: IO ()
main = do
    (files, TableRenderer renderTable) <- execParser $ info  (helper <*> opts) mempty
    let readAssessor path = do
            as <- readAssessments path
            let toBinary (QRel.GradedRelevance n)
                  | n > 2     = QRel.Relevant
                  | otherwise = QRel.NotRelevant
            return $ HM.singleton (assessorFromFilepath path) (fmap toBinary as)

    assessments <- HM.unions <$> mapM readAssessor files
        :: IO (HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) QRel.IsRelevant))
    putStrLn $ "Assessment counts: "++show (fmap HM.size assessments)
    let assessors :: [Assessor]
        assessors = sort $ HM.keys assessments

    putStrLn "Cohen:"
    putStrLn $ renderTable showAssessor showAssessor (maybe "" showFloat3)
      $ Table (Group SingleLine $ map Header assessors) (Group SingleLine $ map Header assessors)
              [ [ if a /= b
                  then Just $ cohenKappa a' b'
                  else Nothing
                | b <- assessors
                , let Just b' = HM.lookup b assessments
                ]
              | a <- assessors
              , let Just a' = HM.lookup a assessments
              ]

    putStrLn $ "Fleiss: "<>showFloat3 (fleissKappa $ HM.elems assessments)
    return ()

showAssessor :: Assessor -> String
showAssessor = T.unpack . unAssessor

showFloat3 :: RealFloat a => a -> String
showFloat3 x = showFFloat (Just 3) x ""
