module CAR.RunFile
    ( -- * Types
      RankingEntry(..)
    , QueryId(..)
    , MethodName(..)
    , Run.Score
      -- * I/O
    , readRunFile
    , writeRunFile
      -- * Grouping and sorting runs
    , groupByQuery
      -- * Conversion
    , sectionPathToQueryId
    ) where

import Data.Ord
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq
import qualified SimplIR.Format.TrecRunFile as Run
import CAR.Types

newtype QueryId = QueryId { unQueryId :: T.Text }
                deriving (Eq, Ord, Show)
newtype MethodName = MethodName { unMethodName :: T.Text }
                   deriving (Eq, Ord, Show)

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

-- | Group a run by query and sort each query by score
groupByQuery :: [RankingEntry] -> M.Map QueryId (Seq.Seq RankingEntry)
groupByQuery run =
    fmap (Seq.sortBy $ comparing carScore)
    $ M.fromListWith mappend [ (carQueryId r, Seq.singleton r) | r <- run ]
