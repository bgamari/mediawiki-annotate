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

data EntityPassageWithFlag = OnlyWithPassage | BothPassage
data EntityPassageWithoutFlag = OnlyWithoutPassage | BothPassage'
data EntityPassageOverwriteFlag = OverwritePassage | KeepPassage
data AugmentFormat = RunFileFormat | QrelFileFormat


opts :: Parser (FilePath,  FilePath, AugmentFormat, TocFile.IndexedCborPath PageId Page, EntityPassageWithFlag, EntityPassageWithoutFlag,EntityPassageOverwriteFlag)
opts = (,,,,,,)
    <$> option str (short 'o' <> long "output" <> help "Output TREC ranking file")
    <*> option str (short 'r' <> long "ranking" <> help "Entity TREC ranking file")
    <*> flag RunFileFormat QrelFileFormat  (short 'q' <> long "qrels" <> help "Entity TREC qrels file")
    <*> option (TocFile.IndexedCborPath <$> str) (short 'b' <> long "kb" <> help "cbor file with knowledge base")
    <*> flag BothPassage OnlyWithPassage (long "only-with-passage" <> help "Only include entities with defined passages")
    <*> flag BothPassage' OnlyWithoutPassage (long "only-without-passage" <> help "Only include entities without defined passages (and fill with leads)")
    <*> flag KeepPassage OverwritePassage (long "overwrite-passage" <> help "Overwrite defined passages with leads)")
main :: IO ()
main = do
    (outputFile, runFile, augmentFormat, kbFile, onlyWithPassage, onlyWithoutPassage, overwritePassage) <- execParser $ info (helper <*> opts) mempty
    queries <- Run.groupByQuery <$> Run.readEntityRun runFile
    kb <- TocFile.open kbFile

    case augmentFormat of
        RunFileFormat -> do
            entityRun <- Run.readEntityParagraphRun runFile
                      :: IO [Run.PassageEntityRankingEntry]

            let entityPassageRun ::  [Run.PassageEntityRankingEntry]
                entityPassageRun =
                        mapMaybe (\r -> merge r
                                        $ rewrite overwritePassage
                                        $ filterFlags onlyWithPassage onlyWithoutPassage
                                        $ augmentLeadParagraph (lookupLeadPara kb) r) entityRun

            Run.writeEntityParagraphRun outputFile entityPassageRun
        QrelFileFormat -> do
               error "Not implemented yet"
--             entityRun <- Run.readEntityParagraphRun runFile
--                       :: IO [Run.PassageEntityRankingEntry]
--
--             let entityPassageRun ::  [Run.PassageEntityRankingEntry]
--                 entityPassageRun =
--                         mapMaybe (\r -> merge r
--                                         $ rewrite overwritePassage
--                                         $ filterFlags onlyWithPassage onlyWithoutPassage
--                                         $ augmentLeadParagraph (lookupLeadPara kb) r) entityRun
--
--             Run.writeEntityParagraphRun outputFile entityPassageRun

lookupLeadPara :: TocFile.IndexedCbor PageId Page ->  PageId -> Maybe ParagraphId
lookupLeadPara kb entityId =  entity2lead entityId
  where entity2lead :: PageId -> Maybe ParagraphId
        entity2lead entityid = join $ fmap page2leadPId $ TocFile.lookup entityid kb

        page2leadPId :: Page -> Maybe ParagraphId
        page2leadPId page =
          let leadSkeleton = listToMaybe $ pageParas page
          in case leadSkeleton of
            Just(Paragraph pid _ ) -> Just pid
            _ ->  Nothing


augmentLeadParagraph :: (PageId -> Maybe ParagraphId) -> Run.PassageEntityRankingEntry -> Maybe (Run.PassageEntity, PageId, Maybe ParagraphId, Maybe ParagraphId)
augmentLeadParagraph lookupLeadPara r
  | pa@(Run.EntityOnly e) <- Run.carDocument r
  = Just $ (pa , e, lookupLeadPara' e, Nothing)

  | pa@(Run.EntityAndPassage e p) <- Run.carDocument r
  = Just $ (pa, e, lookupLeadPara' e, Just p)

    where lookupLeadPara' entityId =
            let paramaybe = lookupLeadPara entityId
            in case paramaybe of
                Nothing -> trace ("unknown lead paragraph for entity.  "<>show r) $ Nothing
                otherwise -> paramaybe

filterFlags :: EntityPassageWithFlag -> EntityPassageWithoutFlag -> Maybe (Run.PassageEntity, PageId, Maybe ParagraphId, Maybe ParagraphId) -> Maybe (Run.PassageEntity, PageId, Maybe ParagraphId, Maybe ParagraphId)
filterFlags onlyWithPassage onlyWithoutPassage Nothing = Nothing
filterFlags onlyWithPassage onlyWithoutPassage (Just t@(Run.EntityOnly _, _, _, _)) =
    case onlyWithPassage of
      OnlyWithPassage -> Nothing
      _ -> Just t
filterFlags onlyWithPassage onlyWithoutPassage  (Just t@(Run.EntityAndPassage _ _, _, _, _)) =
    case onlyWithoutPassage of
      OnlyWithoutPassage -> Nothing
      _ ->  Just t

rewrite :: EntityPassageOverwriteFlag -> Maybe (Run.PassageEntity, PageId, Maybe ParagraphId, Maybe ParagraphId) -> Maybe (Run.PassageEntity, PageId, Maybe ParagraphId, Maybe ParagraphId)
rewrite overwritePassage Nothing = Nothing
rewrite overwritePassage (Just t@(pa@(Run.EntityAndPassage _ _), e, loadedPara, origPara)) =
    case overwritePassage of
      OverwritePassage -> Just (pa, e, loadedPara, loadedPara)
      _ -> Just (pa, e, loadedPara, origPara)
rewrite overwritePassage (Just t) = Just t

merge ::  Run.PassageEntityRankingEntry -> Maybe (Run.PassageEntity, PageId, Maybe ParagraphId, Maybe ParagraphId) -> Maybe Run.PassageEntityRankingEntry
merge r (Just (_ , e, _, Just para)) =   Just $ r { Run.carDocument = Run.EntityAndPassage e para }
merge r (Just (_ , e, Just para, _)) =   Just $ r { Run.carDocument = Run.EntityAndPassage e para }
merge _ _ = Nothing
