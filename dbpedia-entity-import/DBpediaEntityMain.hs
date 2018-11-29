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
import SimplIR.Format.QRel as QF
import CAR.AnnotationsFile as CAR
import qualified Debug.Trace as Debug


helpDescr :: PP.Doc
helpDescr =
    "Convert DBpedia qrels file to TREC CAR ids."


opts :: Parser (IO ())
opts = subparser
    $  cmd "transform-qrels"   transformQrels'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    pagesFile = option str (short 'A' <> long "articles" <> help "articles file" <> metavar "ANNOTATIONS FILE")
    inputQrelsFile = argument str (help "dbpedia qrels file" <> metavar "QRELS")
    inputRunsFile = argument str (help "dbpedia run file" <> metavar "RUN")
    outputFile = option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    transformQrels' =
        transformQrels <$> inputQrelsFile <*> pagesFile <*> outputFile



    transformQrels :: FilePath -> FilePath -> FilePath -> IO()
    transformQrels inQrelsFile articlesFile outputFile = do

        inQrels <- QF.readQRel inQrelsFile
                 :: IO [QF.Entry T.Text T.Text QF.GradedRelevance]
        articlesBundle <- CAR.openPageBundle articlesFile

        let outQrels = [ (QF.Entry query (unwrapPageId doc') rel)
                       | (QF.Entry query doc rel) <- inQrels
                       , doc' <- transformEntity articlesBundle doc
                       ]
        QF.writeQRel outputFile $ filterDuplicateQrels outQrels
      where
        transformEntity :: PageBundle -> T.Text -> [PageId]
        transformEntity articlesBundle dbPediaEntityId =
--    dbPediaEntityId example: <dbpedia:Cellophane_noodles> ->  Cellophane_noodles
             let cleanDbpediaEntityName :: PageName
                 cleanDbpediaEntityName =
                     case parseEntity dbPediaEntityId of
                        Nothing -> error $ "Can't parse DBpedia entity id \"" ++ (show dbPediaEntityId) ++ "\""
                        Just cleanName -> CAR.packPageName $ T.unpack cleanName
                 trecCarPageIds :: Maybe (S.Set PageId)
                 trecCarPageIds =
                     case CAR.bundleLookupRedirect articlesBundle cleanDbpediaEntityName of
                        Nothing -> Debug.trace ("Can't find entity \"" ++show cleanDbpediaEntityName++" \" in TREC CAR ")
                                   Nothing
                        Just pageIdSet -> Just pageIdSet

             in S.toList $ fromMaybe S.empty trecCarPageIds
        unwrapPageId = T.pack . CAR.unpackPageId

        filterDuplicateQrels :: Ord rel => [Entry QueryId  DocumentName rel] ->  [Entry QueryId  DocumentName rel]
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


parseEntity :: T.Text -> Maybe T.Text
parseEntity s = do
    s' <- T.stripPrefix "<dbpedia:" $ T.strip s
    s'' <- T.stripSuffix ">" s'
    return $ T.replace "_" " " s''

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)