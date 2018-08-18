{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module CAR.RunFile
    ( -- * Types
      RankingEntry'(..)
    , QueryId(..)
    , MethodName(..)
    , Run.Score

      -- ** Entity/paragraph rankings
    , PassageEntityRankingEntry
    , traverseText
    , PassageEntity(..)
    , carEntity, carPassage
    , readEntityParagraphRun
    , writeEntityParagraphRun
      -- *** Lenses
    , queryId
    , document
    , rank
    , score
    , methodName

      -- ** Paragraph ranking
    , ParagraphRankingEntry
    , readParagraphRun
    , writeParagraphRun

      -- ** Entity ranking
    , EntityRankingEntry
    , readEntityRun
    , writeEntityRun

      -- * Grouping and sorting runs
    , groupByQuery, groupByQuery'

      -- * Conversion
    , pageIdToQueryId
    , sectionPathToQueryId
    , parsePassageEntity
    , constructPassageEntity
    ) where

import Control.Exception
import Control.DeepSeq
import Control.Lens hiding (MethodName)
import Data.Ord
import Data.Maybe
import Data.Monoid
import Data.Aeson
import Data.Hashable
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified SimplIR.Format.TrecRunFile as Run
import qualified Data.SmallUtf8 as Utf8
import CAR.Types


newtype QueryId = QueryId { unQueryId :: T.Text }
                deriving (Eq, Ord, Show, FromJSON, ToJSON, Hashable)
                deriving newtype NFData


newtype MethodName = MethodName { unMethodName :: T.Text }
                   deriving (Eq, Ord, Show, FromJSON, ToJSON)

data RankingEntry' doc = RankingEntry { carQueryId     :: !QueryId
                                      , carDocument    :: doc
                                      , carRank        :: !Int
                                      , carScore       :: !Run.Score
                                      , carMethodName  :: !MethodName
                                      }
                       deriving  (Show)

$(makeLensesWith abbreviatedFields ''RankingEntry')
$(makeWrapped ''QueryId)
$(makeWrapped ''MethodName)

traverseText :: Monad m
             => Optical (->) (->) m doc doc T.Text T.Text
             -> Optical (->) (->) m (RankingEntry' doc) (RankingEntry' doc) T.Text T.Text
traverseText traverseDoc f x =
    (document . traverseDoc) f x >>= (queryId . _Wrapped) f >>= (methodName . _Wrapped) f


data PassageEntity = EntityOnly !PageId
                   | EntityAndPassage !PageId !ParagraphId
                   deriving (Show)

carEntity :: PassageEntityRankingEntry -> PageId
carEntity r =
    case carDocument r of
      EntityOnly pid         -> pid
      EntityAndPassage pid _ -> pid

carPassage :: PassageEntityRankingEntry -> Maybe ParagraphId
carPassage r =
    case carDocument r of
      EntityOnly _pid         -> Nothing
      EntityAndPassage _ pid  -> Just pid

type ParagraphRankingEntry = RankingEntry' ParagraphId
type EntityRankingEntry = RankingEntry' PageId
-- | Paragraph/entity ranking entry
type PassageEntityRankingEntry = RankingEntry' PassageEntity

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

readEntityParagraphRun :: FilePath -> IO [PassageEntityRankingEntry]
readEntityParagraphRun path =
    handle (throwIO . ReadRunError path)
    (map (toCarRankingEntry parsePassageEntity) <$> Run.readRunFile path)

writeEntityParagraphRun :: FilePath -> [PassageEntityRankingEntry] -> IO ()
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
    fmap (Seq.sortBy $ flip $ comparing carScore)
    $ M.fromListWith mappend [ (carQueryId r, Seq.singleton r) | r <- run ]

-- | Group a run by query and sort each query by score
groupByQuery' :: [(key, RankingEntry' doc)] -> M.Map QueryId (Seq.Seq (key, RankingEntry' doc))
groupByQuery' run =
    fmap (Seq.sortBy $ flip $ comparing (carScore . snd))
    $ M.fromListWith mappend [ (carQueryId r, Seq.singleton (k, r)) | (k, r) <- run ]
