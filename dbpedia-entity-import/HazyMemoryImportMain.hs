{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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


-- import Control.DeepSeq
import Data.Char
import Data.Maybe
import Data.Semigroup hiding (option)

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Text.Lazy.Read as TL.Read
import qualified Data.Text.Lazy.IO as TL

type QueryId = T.Text
type Content = T.Text
type Rank = Int
type Score = Double
type MethodName = T.Text
type EntityId = T.Text

data HazyMemoryLine = HazyMemoryLine { queryId       :: !QueryId
                                 , questionTitle  :: !Content
                                 , questionContent  :: !Content
                                 , answerWikiEntity :: !EntityId
                                 , answerImdbEntity :: !EntityId
                                 , answerTexts :: ![Content]
                                 }
                  deriving (Show)

-- instance NFData HazyMemoryLine where
--     rnf r = r `seq` ()


readHazyMemoryFiles :: [FilePath] ->  IO [HazyMemoryLine]
readHazyMemoryFiles (f1:rest) = do
    d1 <- readHazyMemoryFile f1
    drest <- readHazyMemoryFiles rest
    return $ mconcat [d1, drest]
readHazyMemoryFiles [] = do
    pure []


readHazyMemoryFile :: FilePath -> IO [HazyMemoryLine]
readHazyMemoryFile fname = do
    catMaybes . flip (zipWith parse) [1 :: Int ..] . TL.lines <$> TL.readFile fname
  where
    parse x lineNo
      | lineNo == 1 = Nothing  --  dismiss headers
      | qid:qtitle:qcontent: wikiAnswer:imdbAnswer:answerText1:answerText2:answerText3:_ <- TL.splitOn "\t" x
      = Just HazyMemoryLine { queryId = TL.toStrict qid
                          , questionTitle = TL.toStrict qtitle
                          , questionContent = TL.toStrict qcontent
                          , answerWikiEntity = TL.toStrict wikiAnswer
                          , answerImdbEntity = TL.toStrict imdbAnswer
                          , answerTexts = fmap TL.toStrict [answerText1, answerText2, answerText3]
                          }
      | TL.all isSpace x = Nothing
      | otherwise = error $ "readHazyMemoryLine: "++fname++" ("++show lineNo++"): Unrecognized line in "++fname++": "++TL.unpack x
      where
        readError :: String -> TL.Read.Reader a -> TL.Text -> a
        readError place reader str =
          case reader str of
            Left err -> error $ "readHazyMemoryLine: "++fname++" ("++show lineNo++"): Error parsing "++place++": "++err++": "++TL.unpack str
            Right (x,_) -> x



helpDescr :: PP.Doc
helpDescr =
    "Convert HazyMemory dataset to TREC CAR cbor and qrels."


opts :: Parser (IO ())
opts = subparser
    $  cmd "import-qrels"   importQrels'
    <>  cmd "import-cbor"   importCbor'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    pagesFile = option str (short 'A' <> long "articles" <> help "articles file" <> metavar "ANNOTATIONS FILE")
    inputQrelsFile = argument str (help "dbpedia qrels file" <> metavar "QRELS")
    inputRawDataFile = argument str (help "hazy memory tsv file" <> metavar "TSV")
    inputRunsFile = argument str (help "dbpedia run file" <> metavar "RUN")
    outputFile = option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    importQrels' =
        importQrels <$> (many inputRawDataFile) <*> pagesFile <*> outputFile
    importCbor' =
        importCbor <$> (many inputRawDataFile) <*> outputFile


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

    defaultCantParseHandler dbPediaEntityId = error ("Can't parse Wikipedia entity id \"" ++ (show dbPediaEntityId) ++ "\"")  Nothing
    defaultCantFindHandler cleanDbpediaEntityName =  Debug.trace ("Can't find entity \"" ++show cleanDbpediaEntityName++" \" in TREC CAR. Skipping ") $ Nothing

    silentCantParseHandler dbPediaEntityId = Debug.trace ("Warning: Can't parse DBpedia entity id \"" ++ (show dbPediaEntityId) ++ "\"" ) ((packPageName (T.unpack dbPediaEntityId)))
    silentCantFindHandler cleanDbpediaEntityName =  Debug.trace ("Warning: Can't find entity \"" ++show cleanDbpediaEntityName++" \" in TREC CAR ") $ Just (dummyPageIdSet cleanDbpediaEntityName)

    dummyPageIdSet :: PageName -> S.Set PageId
    dummyPageIdSet name =
         S.singleton $ packPageId ("undefined:" ++ T.unpack (T.replace " " "_" (T.pack (unpackPageName name))))

    importQrels :: [FilePath] -> FilePath -> FilePath -> IO()
    importQrels inFiles articlesFile outputFile = do

        inData <- readHazyMemoryFiles inFiles
               :: IO [HazyMemoryLine]


        articlesBundle <- CAR.openPageBundle articlesFile
        let transFormEntity' = transformEntity defaultCantParseHandler defaultCantFindHandler articlesBundle

            outQrels = [ (QF.Entry ( queryId hazyLn ) (unwrapPageId entity) QF.Relevant)
                       | hazyLn <- inData
                       , entity <- transFormEntity' (answerWikiEntity hazyLn)
                       ]
        QF.writeQRel outputFile $ filterDuplicateQrels outQrels
      where

        unwrapPageId = T.pack . CAR.unpackPageId

        filterDuplicateQrels :: [QF.Entry QF.QueryId  QF.DocumentName QF.IsRelevant] ->  [QF.Entry QF.QueryId  QF.DocumentName QF.IsRelevant]
        filterDuplicateQrels qrelEntries =
            HM.elems
            $ HM.fromListWith chooseHigher
            [ ((QF.queryId entry, QF.documentName entry), entry) |  entry <- qrelEntries]

        chooseHigher entry1 entry2 =
           if QF.relevance entry1 >= QF.relevance entry2 then
                entry1
           else
                entry2
--

    importCbor :: [FilePath] -> FilePath -> IO()
    importCbor inFiles outputFile = do

        inData <- readHazyMemoryFiles inFiles
               :: IO [HazyMemoryLine]
        let outPages = fmap toPage inData
            releaseName = "hazy-v1.0"
            siteProv = SiteProvenance { provSiteId = "hazymemory"
                                  , language = Language "en-us"
                                  , sourceName = "hazymemory"
                                  , siteComments = []
                                  }
            prov = Provenance { siteProvenances = [siteProv]
                          , dataReleaseName = releaseName
                          , comments = []
                          , transforms = []
                          }
        writeCarFile outputFile prov outPages

      where
        toPage :: HazyMemoryLine -> Stub
        toPage HazyMemoryLine{..} =
            Stub { stubName = pageName
                 , stubPageId = packPageId $ T.unpack queryId
                 , stubType = ArticlePage
                 , stubMetadata = emptyPageMetadata
                 , stubSkeleton = [mkParagraph questionContent, mkSection questionContent]
                 }
          where
            pageName = packPageName $ T.unpack questionTitle



mkParagraph :: T.Text -> PageSkeleton
mkParagraph text = Para $ Paragraph (paraBodiesToId bodies) bodies
  where bodies = [ParaText text]

mkSection :: T.Text -> PageSkeleton
mkSection text = Section heading (sectionHeadingToId heading) []
  where heading = SectionHeading text



parseEntity :: T.Text -> Maybe T.Text
parseEntity s = do
    s' <- T.stripPrefix "https://en.wikipedia.org/wiki/" $ T.strip s
    return $ T.replace "_" " " s'

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)