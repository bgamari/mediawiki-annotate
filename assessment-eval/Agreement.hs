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
import qualified Data.HashSet as HS
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
    option (str >>= parse) (long "table" <> value (TableRenderer AsciiArt.render) <> help "table output type")
  where
    parse "latex" = pure $ TableRenderer Latex.render
    parse "tsv"   = pure $ TableRenderer $ SimpleText.render "\t"
    parse "ascii" = pure $ TableRenderer AsciiArt.render
    parse s       = fail $ concat ["unknown table rendering method "
                                  , s
                                  , "; expected 'latex', 'tsv', or 'ascii'"]

data RelType = Binary | Graded | OffByOne

relType :: Parser RelType
relType =
    option (str >>= parse) (short 'r' <> long "relevance" <> help "relevance type")
  where
    parse "binary"     = pure Binary
    parse "graded"     = pure Graded
    parse "off-by-one" = pure OffByOne
    parse s            = fail $ concat ["unknown relevance type"
                                       , s
                                       , "; expected 'binary', 'graded', or 'off-by-one'"]

opts :: Parser ([FilePath], RelType, TableRenderer)
opts =
    (,,)
    <$> some (argument str (metavar "QREL" <> help "Each a qrel file with judgements from a single assessor"))
    <*> relType
    <*> tableRenderer


main :: IO ()
main = do
    (files, relType, renderTable) <- execParser $ info  (helper <*> opts) mempty
    let readAssessor path = HM.singleton (assessorFromFilepath path) <$> readAssessments path
    assessments <- HM.unions <$> mapM readAssessor files
        :: IO (HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance))

    case relType of
      Binary   -> report renderTable Nothing $ fmap (fmap toBinary) assessments
      Graded   -> report renderTable Nothing assessments
      OffByOne -> report renderTable (Just agreementClasses) assessments

agreementClasses :: [HS.HashSet QRel.GradedRelevance]
agreementClasses = map (HS.fromList . map QRel.GradedRelevance)
    [ [-2,0]
    , [0,2]
    , [2,3]
    , [3,4]
    , [4,5]
    ]

toBinary :: QRel.GradedRelevance -> QRel.IsRelevant
toBinary (QRel.GradedRelevance n)
  | n > 2     = QRel.Relevant
  | otherwise = QRel.NotRelevant

report :: (Hashable rel, Eq rel)
       => TableRenderer
       -> Maybe [HS.HashSet rel]
       -> HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) rel)
       -> IO ()
report (TableRenderer renderTable) agreementClasses assessments = do
    putStrLn $ "Assessment counts: "++show (fmap HM.size assessments)
    let assessors :: [Assessor]
        assessors = sort $ HM.keys assessments

    putStrLn "Cohen:"
    putStrLn $ renderTable showAssessor showAssessor (maybe "" showFloat3)
      $ Table (Group SingleLine $ map Header assessors) (Group SingleLine $ map Header assessors)
              [ [ if a /= b
                  then case agreementClasses of
                         Just clss -> Just $ cohenKappa' clss a' b'
                         Nothing   -> Just $ cohenKappa a' b'
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
