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


opts :: Parser (FilePath,  FilePath, FilePath, TocFile.IndexedCborPath PageId Page)
opts = (,,,)
    <$> option str (short 'o' <> long "output" <> help "Output TREC ranking file")
    <*> option str (short 'r' <> long "ranking" <> help "Entity TREC ranking file")
    <*> option str (short 'd' <> long "redirect" <> help "Redirect table file")
    <*> option (TocFile.IndexedCborPath <$> str) (short 'b' <> long "kb" <> help "cbor file with knowledge base")

main :: IO ()
main = do
    (outputFile, runFile, dedupTableFile, kbFile) <- execParser $ info (helper <*> opts) mempty
    queries <- Run.groupByQuery <$> Run.readEntityRun runFile
    kb <- TocFile.open kbFile
    redirectTable <- readDedupTable dedupTableFile

    let entity2lead :: PageId -> Maybe ParagraphId
        entity2lead entityid = join $ fmap page2leadPId $ TocFile.lookup entityid kb
          where page2leadPId :: Page -> Maybe ParagraphId
                page2leadPId page =
                  let leadSkeleton = listToMaybe $ kbDocLeadPara $ Kb.pageToKbDoc page
                  in case leadSkeleton of
                    Just(Para (Paragraph pid _ )) -> Just pid
                    _ -> Nothing


    

    let redirectEntity :: PageId -> PageId
        redirectEntity pageId =
          case pageId `HM.lookup` redirectTable of
          Just toPid -> toPid
          Nothing -> pageId


    let lookupLeadPara ::  PageId -> Maybe ParagraphId
        lookupLeadPara entityId =  entity2lead $ redirectEntity entityId

    entityRun <- Run.readEntityRun runFile
    let entityPassageRun = mapMaybe augmentLeadParagraph entityRun
          where augmentLeadParagraph r
                  | Just para <- lookupLeadPara $ Run.carDocument r
                  = Just $ r { Run.carDocument = Run.EntityAndPassage (Run.carDocument r) para }
                  | otherwise
                  = trace ("unknown lead paragraph for entity.  "<>show r) $ Nothing

    
    Run.writeEntityParagraphRun outputFile entityPassageRun

readDedupTable :: FilePath -> IO (HM.HashMap PageId PageId)
readDedupTable fname =
    HM.fromList . fmap (toPair . words) . lines <$> readFile fname
  where
    toPair [fromPid, toPid] = (packPageId fromPid, packPageId toPid)
