{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CAR.RunFile
    ( -- * Types
      RankingEntry'(..)
    , QueryId(..)
    , MethodName(..)
    , Run.Score

      -- ** Entity/paragraph rankings
    , RankingEntry
    , PassageEntity(..)
    , carEntity, carPassage
    , readEntityParagraphRun
    , writeEntityParagraphRun

      -- ** Paragraph ranking
    , ParagraphRankingEntry
    , readParagraphRun
    , writeParagraphRun

      -- ** Entity ranking
    , EntityRankingEntry
    , readEntityRun
    , writeEntityRun

      -- * Grouping and sorting runs
    , groupByQuery

      -- * Conversion
    , pageIdToQueryId
    , sectionPathToQueryId
    , parsePassageEntity
    , constructPassageEntity
    ) where

import Control.Exception
import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified SimplIR.Format.TrecRunFile as Run
import qualified Data.SmallUtf8 as Utf8
import CAR.Types

newtype QueryId = QueryId { unQueryId :: T.Text }
                deriving (Eq, Ord, Show, FromJSON, ToJSON)

newtype MethodName = MethodName { unMethodName :: T.Text }
                   deriving (Eq, Ord, Show, FromJSON, ToJSON)

data RankingEntry' doc = RankingEntry { carQueryId     :: !QueryId
                                      , carDocument    :: doc
                                      , carRank        :: !Int
                                      , carScore       :: !Run.Score
                                      , carMethodName  :: !MethodName
                                      }

-- | Paragraph/entity ranking entry
type RankingEntry = RankingEntry' PassageEntity

data PassageEntity = EntityOnly PageId
                   | EntityAndPassage !PageId !ParagraphId

carEntity :: RankingEntry -> PageId
carEntity r =
    case carDocument r of
      EntityOnly pid         -> pid
      EntityAndPassage pid _ -> pid

carPassage :: RankingEntry -> Maybe ParagraphId
carPassage r =
    case carDocument r of
      EntityOnly _pid         -> Nothing
      EntityAndPassage _ pid  -> Just pid

type ParagraphRankingEntry = RankingEntry' ParagraphId
type EntityRankingEntry = RankingEntry' PageId

toCarRankingEntry :: (Run.DocumentName -> doc) -> Run.RankingEntry -> RankingEntry' doc
toCarRankingEntry parseDocument r =
    RankingEntry { carQueryId     = QueryId $ Run.queryId r
                 , carDocument    = parseDocument $ Run.documentName r
                 , carRank        = Run.documentRank r
                 , carScore       = Run.documentScore r
                 , carMethodName  = MethodName $ Run.methodName r
                 }

data ParseError = ParseError String Run.DocumentName
                deriving (Show)

instance Exception ParseError

parsePassageEntity :: Run.DocumentName -> PassageEntity
parsePassageEntity docName =
    case (passage, entity) of
      (Just p,  Just e)  -> EntityAndPassage e p
      (Nothing, Just e)  -> EntityOnly e
      (Just _,  Nothing) -> throw $ ParseError "Passage but no entity" docName
      (Nothing, Nothing) -> throw $ ParseError "Neither a passage nor an entity" docName
  where
    (psg,ent)
      | T.null a  = throw $ ParseError "Invalid document name" docName
      | otherwise = (a, T.drop 1 b)
      where (a,b) = T.breakOn "/" docName

    passage
      | T.null psg = Nothing
      | otherwise  = Just $ packParagraphId $ T.unpack psg
    entity
      | T.null ent = Nothing
      | otherwise  = Just $ packPageId $ T.unpack ent

fromCarRankingEntry :: (doc -> Run.DocumentName) -> RankingEntry' doc -> Run.RankingEntry
fromCarRankingEntry construct r =
    Run.RankingEntry { Run.queryId       = unQueryId $ carQueryId r
                     , Run.documentName  = construct $ carDocument r
                     , Run.documentRank  = carRank r
                     , Run.documentScore = carScore r
                     , Run.methodName    = unMethodName $ carMethodName r
                     }

constructPassageEntity :: PassageEntity -> Run.DocumentName
constructPassageEntity ep =
    fromMaybe "" (T.pack . unpackParagraphId <$> maybePara) <> "/" <>
    fromMaybe "" (T.pack . unpackPageId <$> maybeEntity)
  where
    (maybePara, maybeEntity) =
        case ep of
          EntityOnly e -> (Nothing, Just e)
          EntityAndPassage e p -> (Just p, Just e)

pageIdToQueryId :: PageId -> QueryId
pageIdToQueryId (PageId s) = QueryId $ Utf8.toText s

sectionPathToQueryId :: SectionPath -> QueryId
sectionPathToQueryId = QueryId . T.pack . escapeSectionPath

data ReadRunError = ReadRunError FilePath ParseError
                  deriving (Show)
instance Exception ReadRunError

readEntityParagraphRun :: FilePath -> IO [RankingEntry]
readEntityParagraphRun path =
    handle (throwIO . ReadRunError path)
    (map (toCarRankingEntry parsePassageEntity) <$> Run.readRunFile path)

writeEntityParagraphRun :: FilePath -> [RankingEntry] -> IO ()
writeEntityParagraphRun path =
    Run.writeRunFile path . map (fromCarRankingEntry constructPassageEntity)

readParagraphRun :: FilePath -> IO [ParagraphRankingEntry]
readParagraphRun path = map (toCarRankingEntry parseDoc) <$> Run.readRunFile path
  where parseDoc = packParagraphId . T.unpack

writeParagraphRun :: FilePath -> [ParagraphRankingEntry] -> IO ()
writeParagraphRun path = Run.writeRunFile path . map (fromCarRankingEntry constructDoc)
  where constructDoc = T.pack . unpackParagraphId

readEntityRun :: FilePath -> IO [EntityRankingEntry]
readEntityRun path = map (toCarRankingEntry parseDoc) <$> Run.readRunFile path
  where parseDoc = packPageId . T.unpack

writeEntityRun :: FilePath -> [EntityRankingEntry] -> IO ()
writeEntityRun path = Run.writeRunFile path . map (fromCarRankingEntry constructDoc)
  where constructDoc = T.pack . unpackPageId

-- | Group a run by query and sort each query by score
groupByQuery :: [RankingEntry' doc] -> M.Map QueryId (Seq.Seq (RankingEntry' doc))
groupByQuery run =
    fmap (Seq.sortBy $ comparing carScore)
    $ M.fromListWith mappend [ (carQueryId r, Seq.singleton r) | r <- run ]
