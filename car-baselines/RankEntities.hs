{-# LANGUAGE TypeApplications #-}

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Ord
import Data.Semigroup hiding (option)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Options.Applicative

import qualified CAR.RunFile as Run
import CAR.Types
import qualified CAR.TocFile as TocFile
import CAR.Utils

newtype EntityCounts = EntityCounts (M.Map PageName (Sum Int, Max (Run.Score, ParagraphId)))

instance Monoid EntityCounts where
    mempty = EntityCounts mempty
    EntityCounts a `mappend` EntityCounts b = EntityCounts $ M.unionWith (<>) a b


queryEntities :: (ParagraphId -> Paragraph) -> Seq.Seq Run.RankingEntry -> [(PageName, ParagraphId)]
queryEntities lookupPara ranking =
    let EntityCounts counts = foldMap countEntities ranking
        entityRanking :: [(PageName, (Max (Run.Score, ParagraphId)))]
        entityRanking = map (second snd) $ sortBy (flip $ comparing $ fst . snd) $ M.toList counts
    in fmap (fmap $ snd . getMax) entityRanking
  where
    countEntities :: Run.RankingEntry -> EntityCounts
    countEntities r = foldMap (toCounts . linkTarget) (paraLinks para)
      where
        toCounts target =
            EntityCounts $ M.singleton target (Sum 1, Max (Run.carScore r, paraId para))
        para = lookupPara $ Run.carParagraphId r

opts :: Parser (FilePath, TocFile.IndexedCborPath ParagraphId Paragraph)
opts = (,)
    <$> option str (short 'r' <> long "ranking" <> help "TREC ranking file")
    <*> option (TocFile.IndexedCborPath <$> str) (short 'p' <> long "paragraphs" <> help "Paragraphs file")

main :: IO ()
main = do
    (runFile, parasFile) <- execParser $ info (helper <*> opts) mempty
    queries <- Run.groupByQuery <$> Run.readRunFile runFile
    paras <- TocFile.open parasFile
    let lookupPara = fromMaybe (error "uh oh") . flip TocFile.lookup paras
    putStrLn $ unlines [ unlines $ (show query : map (("  "++) . show) entityRanking) ++ [""]
                       | (query, entityRanking) <- M.toList $ fmap (queryEntities lookupPara) queries
                       ]

