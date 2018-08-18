{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.List
import Data.Maybe
import Data.Ord
import Data.Semigroup hiding (option)
import qualified Data.Sequence as Seq
import qualified Data.Map.Strict as M
import Options.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as HM
import Debug.Trace

import qualified CAR.RunFile as Run
import CAR.Types
import qualified CAR.TocFile as TocFile
import CAR.Utils
import CAR.KnowledgeBase as Kb


opts :: Parser (FilePath,  FilePath, TocFile.IndexedCborPath PageId Page)
opts = (,,)
    <$> option str (short 'o' <> long "output" <> help "Output TREC ranking file")
    <*> option str (short 'r' <> long "ranking" <> help "Entity TREC ranking file")
    <*> option (TocFile.IndexedCborPath <$> str) (short 'b' <> long "kb" <> help "cbor file with knowledge base")

main :: IO ()
main = do
    (outputFile, runFile, kbFile) <- execParser $ info (helper <*> opts) mempty
    queries <- Run.groupByQuery <$> Run.readEntityRun runFile
    kb <- TocFile.open kbFile


    let lookupLeadPara ::  PageId -> Maybe ParagraphId
        lookupLeadPara entityId =  entity2lead entityId
          where entity2lead :: PageId -> Maybe ParagraphId
                entity2lead entityid = join $ fmap page2leadPId $ TocFile.lookup entityid kb

                page2leadPId :: Page -> Maybe ParagraphId
                page2leadPId page =
                  let leadSkeleton = listToMaybe $ pageParas page
                  in case leadSkeleton of
                    Just(Paragraph pid _ ) -> Just pid
                    _ -> Nothing

    entityRun <- Run.readEntityParagraphRun runFile
              :: IO [Run.PassageEntityRankingEntry]

    let entityPassageRun ::  [Run.PassageEntityRankingEntry]
        entityPassageRun = mapMaybe augmentLeadParagraph entityRun
          where augmentLeadParagraph :: Run.PassageEntityRankingEntry -> Maybe Run.PassageEntityRankingEntry
                augmentLeadParagraph r
                  | Run.EntityOnly e <- Run.carDocument r
                  , Just para <- lookupLeadPara e
                  = Just $ r { Run.carDocument = Run.EntityAndPassage e para }

                  | Run.EntityAndPassage e p <- Run.carDocument r
                  = Just $ r { Run.carDocument = Run.EntityAndPassage e p }

                  | otherwise
                  = trace ("unknown lead paragraph for entity.  "<>show r) $ Nothing

    Run.writeEntityParagraphRun outputFile entityPassageRun

readDedupTable :: FilePath -> IO (HM.HashMap PageId PageId)
readDedupTable fname =
    HM.fromList . fmap (toPair . words) . lines <$> readFile fname
  where
    toPair [fromPid, toPid] = (packPageId fromPid, packPageId toPid)


