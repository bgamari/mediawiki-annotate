{-# LANGUAGE OverloadedStrings #-}

module CAR.RunFile
    ( -- * Types
      RankingEntry(..)
    , QueryId(..)
    , MethodName(..)
      -- * I/O
    , readRunFile
    , writeRunFile
      -- * Conversion
    , sectionPathToQueryId
    , parsePassageEntity
    , constructPassageEntity
    ) where

import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
--import SimplIR.Format.TrecRunFile hiding (MethodName, QueryId, RankingEntry)
import qualified SimplIR.Format.TrecRunFile as Run
import CAR.Types

newtype QueryId = QueryId { unQueryId :: T.Text }
newtype MethodName = MethodName { unMethodName :: T.Text }

data RankingEntry = RankingEntry { carQueryId     :: !QueryId
                                 , carPassage     :: Maybe ParagraphId
                                 , carEntity      :: Maybe PageId
                                 , carRank        :: !Int
                                 , carScore       :: !Run.Score
                                 , carMethodName  :: !MethodName
                                 }

toCarRankingEntry :: Run.RankingEntry -> RankingEntry
toCarRankingEntry r =
    RankingEntry { carQueryId     = QueryId $ Run.queryId r
                 , carPassage     = passage
                 , carEntity      = entity
                 , carRank        = Run.documentRank r
                 , carScore       = Run.documentScore r
                 , carMethodName  = MethodName $ Run.methodName r
                 }
  where
    (passage, entity) = parsePassageEntity $ Run.documentName r

parsePassageEntity :: Run.DocumentName -> (Maybe ParagraphId, Maybe PageId)
parsePassageEntity docName = (passage, entity)
  where
    (p,e) = case T.splitOn "/" docName of
              [a,b] -> (a,b)
              _     -> error $ "toCarRankingEntry: Invalid document name: " ++ show docName
    passage
      | T.null p  = Nothing
      | otherwise = Just $ packParagraphId $ T.unpack p
    entity
      | T.null e  = Nothing
      | otherwise = Just $ packPageId $ T.unpack e




fromCarRankingEntry :: RankingEntry -> Run.RankingEntry
fromCarRankingEntry r =
    Run.RankingEntry { Run.queryId       = unQueryId $ carQueryId r
                     , Run.documentName  = constructPassageEntity (carPassage r) (carEntity r)
                     , Run.documentRank  = carRank r
                     , Run.documentScore = carScore r
                     , Run.methodName    = unMethodName $ carMethodName r
                     }

constructPassageEntity :: Maybe ParagraphId -> Maybe PageId -> Run.DocumentName
constructPassageEntity maybePara maybeEntity =
      fromMaybe "" (T.pack . unpackParagraphId <$> maybePara) <> "/" <>
      fromMaybe "" (T.pack . unpackPageId <$> maybeEntity)

sectionPathToQueryId :: SectionPath -> QueryId
sectionPathToQueryId = QueryId . T.pack . escapeSectionPath

readRunFile :: FilePath -> IO [RankingEntry]
readRunFile path = map toCarRankingEntry <$> Run.readRunFile path

writeRunFile :: FilePath -> [RankingEntry] -> IO ()
writeRunFile path = Run.writeRunFile path . map fromCarRankingEntry
