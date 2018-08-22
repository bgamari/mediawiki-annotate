{-# language RecordWildCards #-}

import qualified Data.DList as DList
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text as T
import Data.Foldable

import Options.Applicative as Opts

import SimplIR.TREC.News as TREC
import SimplIR.TREC as TREC
import SimplIR.TREC as TREC
import SimplIR.Format.TrecRunFile as Run
import SimplIR.Ranking as Ranking

opts :: Opts.Parser (FilePath, FilePath, FilePath)
opts =
    (,,) <$> argument str (metavar "TOPICS" <> help "XML topic file")
         <*> argument str (metavar "RUN" <> help "Run file")
         <*> option str (short 'o' <> long "output" <> help "Output run file")

main :: IO ()
main = do
    (queryFile, runFile, outFile) <- execParser $ info (helper <*> opts) mempty

    topics <- TREC.parseMany TREC.newsQuery <$> TLIO.readFile queryFile
    let knownEntities :: M.Map Run.QueryId (M.Map Run.DocumentName T.Text)
        knownEntities =
            M.unionsWith (<>)
            [ M.singleton (T.pack $ show $ TREC.topicNumber topic)
              $ M.fromList
              [ (TREC.entityLink entity, TREC.entityId entity)
              | entity <- TREC.topicEntities topic
              ]
            | topic <- topics
            ]

    rankings <- partitionRankings <$> Run.readRunFile runFile
    let rankings' :: M.Map (Run.QueryId, Run.MethodName) (Ranking Run.Score T.Text)
        rankings' = M.mapWithKey (\(qid,_) ranking ->
                                    let Just knownQueryEntities = qid `M.lookup` knownEntities
                                        isKnownDoc :: Run.DocumentName -> Maybe T.Text
                                        isKnownDoc = (`M.lookup` knownQueryEntities)
                                        ranking' = Ranking.mapMaybe isKnownDoc ranking
                                        notMentioned = S.fromList (M.elems knownQueryEntities) `S.difference` S.fromList (toList ranking)
                                    in Ranking.fromList $ Ranking.toSortedList ranking'
                                         <> [(0, ent) | ent <- S.toList notMentioned]
                                 )
                    rankings

    Run.writeRunFile outFile $ combineRankings rankings'

partitionRankings :: [Run.RankingEntry]
                  -> M.Map (Run.QueryId, Run.MethodName) (Ranking Run.Score Run.DocumentName)
partitionRankings entries =
    fmap (Ranking.fromList . DList.toList)
    $ M.fromListWith (<>)
      [ ( (Run.queryId ent, Run.methodName ent)
        , DList.singleton (Run.documentScore ent, Run.documentName ent)
        )
      | ent <- entries
      ]

combineRankings :: M.Map (Run.QueryId, Run.MethodName) (Ranking Run.Score Run.DocumentName)
                -> [Run.RankingEntry]
combineRankings entries =
    [ Run.RankingEntry {..}
    | ((queryId, methodName), ranking) <- M.toList entries
    , (documentRank, (documentScore, documentName))
          <- zip [1..] (Ranking.toSortedList ranking)
    ]
