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
    option (str >>= parse) (short 'r' <> long "relevance" <> help "relevance type; one of 'binary', 'graded', 'off-by-one'")
  where
    parse "binary"     = pure Binary
    parse "graded"     = pure Graded
    parse "off-by-one" = pure OffByOne
    parse s            = fail $ concat ["unknown relevance type"
                                       , s
                                       , "; expected 'binary', 'graded', or 'off-by-one'"]

opts :: Parser ([FilePath], [FilePath], RelType, TableRenderer)
opts =
    (,,,)
    <$> some (argument str (metavar "QREL" <> help "Each a qrel file with judgements from a single assessor"))
    <*> many (option str (short 'a'<> long "auto-qrels" <> metavar "AUTO-QREL" <> help "Each a qrel file with auto assessments"))
    <*> relType
    <*> tableRenderer


main :: IO ()
main = do
    (files, autoFiles, relType, renderTable) <- execParser $ info  (helper <*> opts) mempty
    let readAssessor path = HM.singleton (assessorFromFilepath path) <$> readAssessments path
    manuAssessments <- HM.unions <$> mapM readAssessor files
        :: IO (HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance))


    autoAssessments' <- HM.unions <$> mapM readAssessor autoFiles
        :: IO (HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance))
    let autoAssessments :: HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance)
        autoAssessments = fmap assumeMissingAsNonRelevant autoAssessments'
          where assumeMissingAsNonRelevant :: HM.HashMap  (QueryId, DocumentId) QRel.GradedRelevance ->  HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance
                assumeMissingAsNonRelevant autoMap' =
                    HM.unionWith (\judg _default -> judg) autoMap' qdDefaultAsNegatives
                qdDefaultAsNegatives ::  HM.HashMap  (QueryId, DocumentId) QRel.GradedRelevance
                qdDefaultAsNegatives =  HM.fromList
                                     $ [ ((q,d), (QRel.GradedRelevance 0))
                                       | (_, lst) <- HM.toList manuAssessments
                                       , ((q,d),_) <- HM.toList lst
                                       ]

    let assessments = HM.union manuAssessments autoAssessments


    case relType of
      Binary   -> report renderTable binaryRel Nothing  $ fmap (fmap toBinary) assessments
      Graded   -> report renderTable gradedCarRel Nothing assessments
      OffByOne -> report renderTable gradedCarRel (Just agreementClasses) assessments
  where gradedCarRel = (map QRel.GradedRelevance [-2, -1, 0, 1, 2,3])
        binaryRel = [QRel.Relevant, QRel.NotRelevant]


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
  | n > 0     = QRel.Relevant
  | otherwise = QRel.NotRelevant

invertAssessments ::   HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) rel) ->  HM.HashMap (QueryId, DocumentId) [(Assessor, rel)]
invertAssessments assessments =
    HM.fromListWith (<>)
    $ [ ((query,doc), [(assessor, r)] )
      | (assessor, qd2r) <- HM.toList assessments
      , ((query, doc), r) <- HM.toList qd2r
      ]

countConfusion :: (Eq rel, Hashable rel) => HM.HashMap (QueryId, DocumentId) [(Assessor, rel)] -> HM.HashMap (rel,rel) Int
countConfusion invAssessments =
    HM.fromListWith (+)
    $ [ ((r1,r2), 1)
      | ((query,doc), lst) <- HM.toList invAssessments
      , ((assess1, r1),(assess2,r2)) <- allPairs lst
      ]
  where allPairs xs =
            [ (x,y)
            | (x,ys) <- zip xs (drop 1 $ tails xs)
            , y <- ys
            ]

report :: (Hashable rel, Eq rel, Show rel)
       => TableRenderer
       -> [rel]
       -> Maybe [HS.HashSet rel]
       -> HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) rel)
       -> IO ()
report (TableRenderer renderTable) relLabels agreementClasses assessments = do
    putStrLn $ "Assessment counts: "++show (fmap HM.size assessments)
    reportCohen  (TableRenderer renderTable) agreementClasses assessments
    reportFleiss assessments
    reportConfusion  (TableRenderer renderTable) relLabels assessments
    return ()

reportCohen :: (Hashable rel, Eq rel, Show rel)
       => TableRenderer
       -> Maybe [HS.HashSet rel]
       -> HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) rel)
       -> IO ()
reportCohen (TableRenderer renderTable) agreementClasses assessments = do
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


reportFleiss :: (Hashable rel, Eq rel, Show rel)
       => HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) rel)
       -> IO ()
reportFleiss  assessments  = do
    let assessors :: [Assessor]
        assessors = sort $ HM.keys assessments
    putStrLn $ "Fleiss: "<>showFloat3 (fleissKappa $ HM.elems assessments)



reportConfusion :: (Hashable rel, Eq rel, Show rel)
       => TableRenderer
       -> [rel]
       -> HM.HashMap Assessor (HM.HashMap (QueryId, DocumentId) rel)
       -> IO ()
reportConfusion (TableRenderer renderTable) relLabels  assessments = do
    let assessors :: [Assessor]
        assessors = sort $ HM.keys assessments
        invAssessments = invertAssessments assessments
        confTable =  countConfusion invAssessments
    putStrLn $ "Confusion matrix:"
    putStrLn $ renderTable show show (maybe "" show)
             $ Table (Group SingleLine $ map Header relLabels) (Group SingleLine $ map Header relLabels)
               [ [ (r1,r2) `HM.lookup` confTable
                 | r2 <- relLabels
                 ]
               | r1 <- relLabels
               ]
showAssessor :: Assessor -> String
showAssessor = T.unpack . unAssessor

showFloat3 :: RealFloat a => a -> String
showFloat3 x = showFFloat (Just 3) x ""
