{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.Semigroup
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Options.Applicative

import qualified SimplIR.Format.QRel as QRel

newtype QueryId = QueryId T.Text
                deriving (Eq, Ord, Show, Hashable)

newtype DocumentId = DocumentId T.Text
                   deriving (Eq, Ord, Show, Hashable)

newtype Relevance = Relevance Int
                  deriving (Eq, Ord, Show, Hashable)

type Assessments = HM.HashMap (QueryId, DocumentId) Relevance

newtype Assessor = Assessor T.Text
                 deriving (Eq, Ord, Show, Hashable)

readAssessments :: FilePath -> IO Assessments
readAssessments = fmap (foldMap toAssessments) . QRel.readQRel (Relevance . QRel.gradedRelevance)
  where
    toAssessments :: QRel.Entry Relevance -> Assessments
    toAssessments QRel.Entry{..} =
        HM.singleton (QueryId queryId, DocumentId documentName) relevance

assessorFromFilepath :: FilePath -> Assessor
assessorFromFilepath path =
    Assessor $ fromMaybe t $ foldMap (t `T.stripPrefix`) [".rel", ".qrel"]
  where
    t = T.pack path

opts :: Parser [FilePath]
opts = some $ argument str (metavar "QREL" <> help "A qrel file with judgements from a single assessor")

main :: IO ()
main = do
    files <- execParser $ info opts mempty
    let readAssessor path = do
            as <- readAssessments path
            return $ HM.singleton (assessorFromFilepath path) as

    assessments <- HM.unions <$> mapM readAssessor files
    putStrLn "Cohen:"
    putStrLn $ unlines [ show a <> "\t" <> show b <> "\t" <> show (cohenKappa a' b')
                       | (a, a') <- HM.toList assessments
                       , (b, b') <- HM.toList assessments
                       , a < b
                       ]
    print $ "Fleiss: "<>show (fleissKappa $ HM.elems assessments)
    return ()

cohenKappa :: forall subj cat. (Eq cat, Hashable cat, Eq subj, Hashable subj)
           => HM.HashMap subj cat
           -> HM.HashMap subj cat
           -> Double
cohenKappa a b =
    1 - (1 - po) / (1 - pe)
  where
    po = realToFrac (length [ () | (ka,kb) <- HM.elems inter, ka == kb ]) / realToFrac (HM.size inter)
    pe = sum (HM.intersectionWith (*) na nb) / realToFrac (HM.size inter)^(2::Int)
    -- how many times a and b predict class k
    na, nb :: HM.HashMap cat Double
    na = HM.fromListWith (+) [ (k, 1) | (_, (k, _)) <- HM.toList inter ]
    nb = HM.fromListWith (+) [ (k, 1) | (_, (_, k)) <- HM.toList inter ]
    inter = HM.intersectionWith (,) a b

fleissKappa :: forall subj cat. (Eq cat, Hashable cat, Eq subj, Hashable subj)
            => [HM.HashMap subj cat]  -- ^ the assessments of each assessor
            -> Double
fleissKappa assessments =
    (barP - barPe) / (1 - barPe)
  where
    -- n_i
    numAssessments :: HM.HashMap subj Int
    numAssessments = HM.fromListWith (+) [ (x, 1)
                                         | a <- assessments
                                         , x <- HM.keys a ]
    -- N
    numSubjects = HM.size numAssessments

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
         [ (k, realToFrac n / realToFrac ni)
         | (x, xs) <- HM.toList nij
         , let Just ni = x `HM.lookup` numAssessments
         , (k, n) <- HM.toList xs
         ]

    barP :: Double
    barP = (/ realToFrac numSubjects) $ sum
           [ sum [ (realToFrac v)^(2::Int) - realToFrac numSubjects * ni
                 | v <- HM.elems nijs
                 ]
           | (x, nijs) <- HM.toList nij
           , let Just ni = realToFrac <$> x `HM.lookup` numAssessments
           ]

    barPe = sum [ v^(2 :: Int) | v <- HM.elems pj ]
