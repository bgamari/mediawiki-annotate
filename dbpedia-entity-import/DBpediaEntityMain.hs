{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Monoid hiding (All, Any)
import Control.Monad

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import Data.Maybe
import Data.Char
import Data.Void
import Control.Monad (void)
import Options.Applicative
import qualified Data.Binary.Serialise.CBOR as CBOR

import CAR.Types.AST as CAR
import CAR.ToolVersion
import CAR.Types
import qualified SimplIR.Format.QRel as QF
import qualified SimplIR.Format.TrecRunFile as RF
import CAR.AnnotationsFile as CAR
import qualified Debug.Trace as Debug


helpDescr :: PP.Doc
helpDescr =
    "Convert DBpedia qrels file to TREC CAR ids."


opts :: Parser (IO ())
opts = subparser
    $  cmd "transform-qrels"   transformQrels'
    <>  cmd "transform-runs"   transformRuns'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    pagesFile = option str (short 'A' <> long "articles" <> help "articles file" <> metavar "ANNOTATIONS FILE")
    inputQrelsFile = argument str (help "dbpedia qrels file" <> metavar "QRELS")
    inputRunsFile = argument str (help "dbpedia run file" <> metavar "RUN")
    outputFile = option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    transformQrels' =
        transformQrels <$> inputQrelsFile <*> pagesFile <*> outputFile
    transformRuns' =
        transformRuns <$> inputRunsFile <*> pagesFile <*> outputFile


    transformEntity :: (T.Text -> PageName) -> (PageName -> Maybe (S.Set PageId)) -> PageBundle -> T.Text -> [PageId]
    transformEntity cantParseHandler cantFindHandler articlesBundle dbPediaEntityId =
--    dbPediaEntityId example: <dbpedia:Cellophane_noodles> ->  Cellophane_noodles
         let cleanDbpediaEntityName :: PageName
             cleanDbpediaEntityName =
                 case parseEntity dbPediaEntityId of
                    Nothing -> cantParseHandler dbPediaEntityId
                    Just cleanName -> CAR.packPageName $ T.unpack cleanName
             trecCarPageIds :: Maybe (S.Set PageId)
             trecCarPageIds =
                 case CAR.bundleLookupRedirect articlesBundle cleanDbpediaEntityName of
                    Nothing -> cantFindHandler cleanDbpediaEntityName
                    Just pageIdSet -> Just pageIdSet

         in S.toList $ fromMaybe S.empty trecCarPageIds

    defaultCantParseHandler dbPediaEntityId = error ("Can't parse DBpedia entity id \"" ++ (show dbPediaEntityId) ++ "\"")  Nothing
    defaultCantFindHandler cleanDbpediaEntityName =  Debug.trace ("Can't find entity \"" ++show cleanDbpediaEntityName++" \" in TREC CAR. Skipping ") $ Nothing

    silentCantParseHandler dbPediaEntityId = Debug.trace ("Warning: Can't parse DBpedia entity id \"" ++ (show dbPediaEntityId) ++ "\"" ) ((packPageName (T.unpack dbPediaEntityId)))
    silentCantFindHandler cleanDbpediaEntityName =  Debug.trace ("Warning: Can't find entity \"" ++show cleanDbpediaEntityName++" \" in TREC CAR ") $ Just (dummyPageIdSet cleanDbpediaEntityName)

    dummyPageIdSet :: PageName -> S.Set PageId
    dummyPageIdSet name =
         S.singleton $ packPageId ("undefined:" ++ T.unpack (T.replace " " "_" (T.pack (unpackPageName name))))

    transformQrels :: FilePath -> FilePath -> FilePath -> IO()
    transformQrels inQrelsFile articlesFile outputFile = do

        inQrels <- QF.readQRel inQrelsFile
                 :: IO [QF.Entry T.Text T.Text QF.GradedRelevance]
        articlesBundle <- CAR.openPageBundle articlesFile
        let transFormEntity' = transformEntity  defaultCantParseHandler defaultCantFindHandler articlesBundle

            outQrels = [ (QF.Entry query (unwrapPageId doc') rel)
                       | (QF.Entry query doc rel) <- inQrels
                       , doc' <- transFormEntity' doc
                       ]
        QF.writeQRel outputFile $ filterDuplicateQrels outQrels
      where

        unwrapPageId = T.pack . CAR.unpackPageId

        filterDuplicateQrels :: Ord rel => [QF.Entry QF.QueryId  QF.DocumentName rel] ->  [QF.Entry QF.QueryId  QF.DocumentName rel]
        filterDuplicateQrels qrelEntries =
            HM.elems
            $ HM.fromListWith chooseHigher
            [ ((QF.queryId entry, QF.documentName entry), entry) |  entry <- qrelEntries]

      --where --chooseHigher :: (QF.Entry query doc rel -> (QF.Entry query doc rel) -> (QF.Entry query doc rel)

        chooseHigher entry1 entry2 =
           if QF.relevance entry1 >= QF.relevance entry2 then
                entry1
           else
                entry2


    transformRuns :: FilePath -> FilePath -> FilePath -> IO()
    transformRuns inRunsFile articlesFile outputFile = do

        inRun <- RF.readRunFile inRunsFile
                 :: IO [RF.RankingEntry]
        articlesBundle <- CAR.openPageBundle articlesFile

        let transFormEntity' = transformEntity  silentCantParseHandler silentCantFindHandler articlesBundle

            outRun =  [ entry {RF.documentName = unwrapPageId doc'}
                       | entry <- inRun
                       , let doc = RF.documentName entry
                       , doc' <- transFormEntity' doc
                       ]
        RF.writeRunFile outputFile $ filterDuplicateEntries outRun
      where
        unwrapPageId = T.pack . CAR.unpackPageId

        filterDuplicateEntries :: [RF.RankingEntry] ->  [RF.RankingEntry]
        filterDuplicateEntries runEntries =
            HM.elems
            $ HM.fromListWith chooseHigher
            [ ((RF.queryId entry, RF.documentName entry), entry) |  entry <- runEntries]

        chooseHigher entry1 entry2 =
           if RF.documentScore entry1 >= RF.documentScore entry2 then
                entry1
           else
                entry2


parseEntity :: T.Text -> Maybe T.Text
parseEntity s = do
    s' <- T.stripPrefix "<dbpedia:" $ T.strip s
    s'' <- T.stripSuffix ">" s'
    return $ T.replace "_" " " s''

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)