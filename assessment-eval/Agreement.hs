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

newtype QueryId = QueryId T.Text
                deriving (Eq, Ord, Show, Hashable)

newtype DocumentId = DocumentId T.Text
                   deriving (Eq, Ord, Show, Hashable)

type Assessments = HM.HashMap (QueryId, DocumentId) QRel.GradedRelevance

newtype Assessor = Assessor T.Text
                 deriving (Eq, Ord, Show, Hashable)

readAssessments :: FilePath -> IO Assessments
readAssessments = fmap (foldMap toAssessments) . QRel.readQRel
  where
    toAssessments :: QRel.Entry QRel.GradedRelevance -> Assessments
    toAssessments QRel.Entry{..} =
        HM.singleton (QueryId queryId, DocumentId documentName) relevance

assessorFromFilepath :: FilePath -> Assessor
assessorFromFilepath path =
    Assessor $ fromMaybe t $ foldMap (t `T.stripPrefix`) [".rel", ".qrel"]
  where
    t = T.pack $ takeFileName path

opts :: Parser [FilePath]
opts = some $ argument str (metavar "QREL" <> help "A qrel file with judgements from a single assessor")

main :: IO ()
main = do
    files <- execParser $ info opts mempty
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

cohenKappa :: forall subj cat. (Eq cat, Hashable cat, Eq subj, Hashable subj)
           => HM.HashMap subj cat
           -> HM.HashMap subj cat
           -> Double
cohenKappa a b =
    1 - (1 - po) / (1 - pe)
  where
    !agreementCount = length [ ()
                             | (ka,kb) <- HM.elems inter
                             , ka == kb
                             ]
    !po = realToFrac agreementCount / realToFrac allCount
    !pe = realToFrac (sum $ HM.intersectionWith (*) na nb) / realToFrac allCount^(2::Int)
    -- how many times a and b predict class k
    na, nb :: HM.HashMap cat Int
    !na = HM.fromListWith (+) [ (k, 1) | (_, (k, _)) <- HM.toList inter ]
    !nb = HM.fromListWith (+) [ (k, 1) | (_, (_, k)) <- HM.toList inter ]

    inter :: HM.HashMap subj (cat,cat)
    !inter = HM.intersectionWith (,) a b
    !allCount = HM.size inter

fleissKappa :: forall subj cat. (Eq cat, Hashable cat, Eq subj, Hashable subj)
            => [HM.HashMap subj cat]  -- ^ the assessments of each assessor
            -> Double
fleissKappa assessments' =
    (barP - barPe) / (1 - barPe)
  where
    assessments = onlyOverlappingAssessments assessments'

    -- n_i
    numAssessments :: HM.HashMap subj Int
    numAssessments = HM.fromListWith (+) [ (x, 1)
                                         | a <- assessments
                                         , x <- HM.keys a ]
    -- N
    numSubjects = HM.size numAssessments
    totalAssessments = sum numAssessments

    -- n_ij
    nij :: HM.HashMap subj (HM.HashMap cat Int)
    nij = HM.fromListWith (HM.unionWith (+))
        [ (x, HM.singleton k 1)
        | a <- assessments
        , (x, k) <- HM.toList a
        ]

    -- p_j, probability that class k is predicted
    pj :: HM.HashMap cat Double
    pj = HM.fromListWith (+)
         [ (k, realToFrac n / realToFrac totalAssessments)
         | xs <- HM.elems nij
         , (k, n) <- HM.toList xs
         ]

    barP :: Double
    barP = (/ realToFrac numSubjects) $ sum
           [ (inner / realToFrac ni / realToFrac (ni - 1)) - 1 / realToFrac (ni - 1)
           | (x, njs) <- HM.toList nij
           , let Just ni = x `HM.lookup` numAssessments
           , let inner = sum [ (realToFrac v)^(2::Int)
                             | v <- HM.elems njs
                             ]
           ]

    barPe = sum [ v^(2 :: Int) | v <- HM.elems pj ]

onlyOverlappingAssessments
    :: forall subj cat. (Eq cat, Hashable cat, Eq subj, Hashable subj)
    => [HM.HashMap subj cat] -> [HM.HashMap subj cat]
onlyOverlappingAssessments assessments =
    map (HM.filterWithKey overlaps) assessments
  where
    numAssessments :: HM.HashMap subj Int
    numAssessments = HM.fromListWith (+) [ (x, 1)
                                         | a <- assessments
                                         , x <- HM.keys a ]
    overlaps x _ = n > 1
      where Just n = x `HM.lookup` numAssessments
