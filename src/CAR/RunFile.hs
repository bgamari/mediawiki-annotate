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
    ) where

import qualified Data.Text as T
--import SimplIR.Format.TrecRunFile hiding (MethodName, QueryId, RankingEntry)
import qualified SimplIR.Format.TrecRunFile as Run
import CAR.Types

newtype QueryId = QueryId { unQueryId :: T.Text }
newtype MethodName = MethodName { unMethodName :: T.Text }

data RankingEntry = RankingEntry { carQueryId     :: !QueryId
                                 , carParagraphId :: !ParagraphId
                                 , carRank        :: !Int
                                 , carScore       :: !Run.Score
                                 , carMethodName  :: !MethodName
                                 }

toCarRankingEntry :: Run.RankingEntry -> RankingEntry
toCarRankingEntry r =
    RankingEntry { carQueryId     = QueryId $ Run.queryId r
                 , carParagraphId = packParagraphId $ T.unpack $ Run.documentName r
                 , carRank        = Run.documentRank r
                 , carScore       = Run.documentScore r
                 , carMethodName  = MethodName $ Run.methodName r
                 }

fromCarRankingEntry :: RankingEntry -> Run.RankingEntry
fromCarRankingEntry r =
    Run.RankingEntry { Run.queryId       = unQueryId $ carQueryId r
                     , Run.documentName  = T.pack $ unpackParagraphId $ carParagraphId r
                     , Run.documentRank  = carRank r
                     , Run.documentScore = carScore r
                     , Run.methodName    = unMethodName $ carMethodName r
                     }

sectionPathToQueryId :: SectionPath -> QueryId
sectionPathToQueryId = QueryId . T.pack . escapeSectionPath

readRunFile :: FilePath -> IO [RankingEntry]
readRunFile path = map toCarRankingEntry <$> Run.readRunFile path

writeRunFile :: FilePath -> [RankingEntry] -> IO ()
writeRunFile path = Run.writeRunFile path . map fromCarRankingEntry
