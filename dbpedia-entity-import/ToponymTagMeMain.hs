{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

import Data.Monoid hiding (All, Any)
import Control.Monad
import System.Environment
import Control.Exception
import Servant.Client

import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Text.PrettyPrint.ANSI.Leijen ((<$$>))
import Data.Maybe
import Data.Char
import Data.Void
import Data.Foldable
import Control.Monad (void)
import Options.Applicative
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as S
import Numeric.Log

import CAR.Types.AST as CAR
import CAR.ToolVersion
import CAR.Types
import qualified SimplIR.Format.QRel as QF
import qualified SimplIR.Format.TrecRunFile as RF
import CAR.AnnotationsFile as CAR
import qualified Debug.Trace as Debug

import Data.Semigroup hiding (option)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as Aeson
import GHC.Generics
import System.FilePath

import ToponymGroundTruthParser
import TagMe

data PubmedDocument = PubmedDocument { content :: T.Text
                                     , filename :: T.Text
                                     }
        deriving (Aeson.ToJSON, Aeson.FromJSON, Generic)

data PubmedAnnotations = PubmedAnnotations { doc :: PubmedDocument
                                           , annotations :: [Annotation]
                                           }
        deriving (Aeson.ToJSON, Aeson.FromJSON, Generic)

data ToponymWrapper = ToponymWrapper { list :: [PubmedAnnotations]}
        deriving (Aeson.ToJSON, Aeson.FromJSON, Generic)


-- Naive Bayes types
type Pos = Int
type Neg = Int
data NBTuple = NBTuple { intPositives, intNegatives :: !Int }

aPos, aNeg :: NBTuple
aPos = NBTuple 1 0
aNeg = NBTuple 0 1

instance Monoid NBTuple where
    mempty = NBTuple 0 0
instance Semigroup NBTuple where
    NBTuple a b <> NBTuple c d = NBTuple (a+c) (b+d)



data NBLogTuple = NBLogTuple { positives, negatives :: !(Log Double) }
data NBModel = NBModel { totals :: NBLogTuple
                       , stats :: HM.HashMap T.Text NBLogTuple}

mkNaiveBayesModel :: NBTuple -> HM.HashMap T.Text NBTuple -> NBModel
mkNaiveBayesModel t s =
    NBModel { totals = toLog t, stats = fmap toLog s}
  where toLog :: NBTuple -> NBLogTuple
        toLog (NBTuple p n) = NBLogTuple (realToFrac p) (realToFrac n)



nbLikelihood :: NBModel -> [T.Text] -> Log Double
nbLikelihood NBModel{..} feats =
    let !featSet = S.fromList feats

    in toRatio totals * product [ toRatio x
                               | f <- feats
                               , let Just x = f `HM.lookup` stats
                               ]
                     * product [ toFlipRatio x
                               | (f,x) <- HM.toList stats
                               , not $ f `S.member` featSet
                               ]
  where
    toRatio :: NBLogTuple -> Log Double
    toRatio (NBLogTuple pos neg) =
        pos / neg

    toFlipRatio :: NBLogTuple -> Log Double
    toFlipRatio (NBLogTuple pos neg) =
        neg / pos



readPubmedFiles :: [FilePath] -> IO [PubmedDocument]
readPubmedFiles (f1:rest) = do
    d1 <- readPubmedFile f1
    drest <- readPubmedFiles rest
    return $ d1:drest
readPubmedFiles [] = do
    pure []


readPubmedFile :: FilePath -> IO PubmedDocument
readPubmedFile fname  = do
    text <- T.readFile fname
    return PubmedDocument { content = text
                          , filename = T.pack $ takeBaseName fname
                          }


writePubmedAnnotations :: FilePath -> [PubmedAnnotations] -> IO ()
writePubmedAnnotations fname outData =
    BSL.writeFile fname $ Aeson.encode $ ToponymWrapper outData

readPubmedAnnotations :: FilePath -> IO [PubmedAnnotations]
readPubmedAnnotations fname = do
    contents <- BSL.readFile fname
    return $ list
           $ fromJust $ Aeson.decode
           $ contents

tagData :: TagMe.TagMeEnv -> TagMe.Token -> Int -> Int -> PubmedDocument -> IO [TagMe.Annotation]
tagData env tagMeToken maxLen overlapLen document = do
    let txt = content document
    anns <- sequence
            [ do putStrLn $ (show (filename document)) <> " " <> (show i) <> ": " <> (show (T.take 10 t)) <> "... "
                 anns <- entityLinkAnnotationsConf env tagMeToken t tagMeOptions
                 return [ ann { start = (start ann) + i*maxLen
                              , end = (end ann) + i*maxLen}
                        | ann <- anns
                        ]
            | (i, t) <- zip [0..] $ overlapChunks maxLen overlapLen txt
            ]
    return $ mconcat anns




overlapChunks :: Int -> Int -> T.Text -> [T.Text]
overlapChunks k o text =
    [ substring start (start+k+o) text
    | start <- [0, k .. T.length text]
    ]
  where
    substring:: Int -> Int -> T.Text -> T.Text
    substring start end text =
        T.take (end-start) $ T.drop start text


tagMeOptions :: TagMeOptions
tagMeOptions = TagMeOptions { inclAbstract = False
                            , inclCategories = True
                            , isTweet = False
                            , isLongText = True
                            , language = langEn
                            }




helpDescr :: PP.Doc
helpDescr =
    "Convert PubMed documents to TagMe annotations."


annotatePubMed :: [FilePath] -> FilePath -> Int -> Int -> Int -> IO()
annotatePubMed inFiles outputFile maxLen overlapLen httpTimeout = do
    tagMeToken <- Token . T.pack <$> getEnv "TAG_ME_TOKEN"
    env <- mkTagMeEnv httpTimeout

    inData <- readPubmedFiles inFiles
           :: IO [PubmedDocument]
    annotatedInData <- sequence [ handle (handler doc) $
                                      do anns <- tagData env tagMeToken maxLen overlapLen doc
                                         return $ Just $ PubmedAnnotations {doc = doc, annotations = anns}
                                | doc <- inData
                                ]
                       :: IO [Maybe PubmedAnnotations]

    writePubmedAnnotations outputFile $ catMaybes annotatedInData
  where
    handler :: PubmedDocument -> ClientError -> IO (Maybe PubmedAnnotations)
    handler PubmedDocument{filename = fname} e = do
        putStrLn $ "TagmeServerError: "<> show fname <>  " : "<> show e
        return Nothing


placePatterns :: [T.Text]
placePatterns = ["place", "capital", "province" , "nations", "countries", "territories", "territory", "geography", "continent"]

predictToponyms :: FilePath -> FilePath -> FilePath -> [FilePath] -> IO ()
predictToponyms trainInFile predictInFile outputFile groundTruthFiles = do
    trainData <- readPubmedAnnotations trainInFile
    groundTruthData <- loadGroundTruthHashMap groundTruthFiles
    let model = trainNaive (isPositiveData groundTruthData) trainData
    predictData <- readPubmedAnnotations predictInFile
    writePubmedAnnotations outputFile $ catMaybes $ fmap (onlyPlaces model) predictData
  where
    onlyPlaces :: NBModel -> PubmedAnnotations -> Maybe PubmedAnnotations
    onlyPlaces model (pub@PubmedAnnotations {annotations = annotations }) =
        let annotation' = filter (onlyPlaceAnnotations model) annotations
        in if null annotation' then Nothing
           else Just $ pub { annotations = annotation'}

    onlyPlaceAnnotations model = (predictNaive model)

    onlyPlaceAnnotationsHeuristic :: Annotation -> Bool
    onlyPlaceAnnotationsHeuristic Annotation{..} =
        let Just categories = dbpediaCategories
            placeCats = [ cat
                        | cat <- categories
                        , pat <- placePatterns
                        , pat `T.isInfixOf` (T.toLower cat)
                        ]
--             !x =  Debug.traceShow (title, placeCats) $ placeCats
        in (not $ null $ placeCats) && (spot /= "et" && spot /= "al")

    trainNaive :: ( T.Text -> Annotation -> Bool) -> [PubmedAnnotations] -> NBModel
    trainNaive isPositive trainData =
        let totals :: NBTuple
            !totals = foldMap (\(isPos, _ ) -> if isPos then aPos else aNeg) trainData'
            perCatCounts :: HM.HashMap T.Text NBTuple
            !perCatCounts =
                HM.fromListWith (<>)
                $ [ (cat, counts)
                  | (isPos, Annotation{dbpediaCategories = Just categories}) <- trainData'
                  , cat <- categories
                  , let counts = if isPos then aPos else aNeg
                  ]
        in mkNaiveBayesModel totals perCatCounts
      where trainData' =
                [ (isPos, ann)
                | PubmedAnnotations { doc =  PubmedDocument {filename = pubmedFilePath }, annotations =  anns} <- trainData
                , ann <- anns
                , let isPos = isPositive pubmedFilePath ann
                ]
    predictNaive :: NBModel -> Annotation -> Bool
    predictNaive model Annotation{dbpediaCategories = Just categories} =
        let score = nbLikelihood model categories
        in score > 0.6


        -- todo get ground truth pos/neg
    isPositiveData :: HM.HashMap T.Text ([Offsets], [Offsets]) ->  T.Text -> Annotation -> Bool
    isPositiveData groundTruthData docname Annotation{..} =
        case docname `HM.lookup` groundTruthData of
          Just (posOffsets, negOffsets) -> not $ null $ filter ((start, end) `offsetsIntersect`) posOffsets
          Nothing -> False
      where offsetsIntersect :: Offsets -> Offsets -> Bool
            offsetsIntersect (s1, e1)  (s2, e2) =
                if s1 > s2 && s1 < e2 then True
                else if s2 > s1 && s2 < e1 then True
                   else False
opts :: Parser (IO ())
opts = subparser
    $  cmd "annotate"   annotatePubMed'
    <> cmd "predict"   predictToponyms'
  where
    cmd name action = command name (info (helper <*> action) fullDesc)
    inputRawDataFile = argument str (help "pubmed text file" <> metavar "TXT")
    maxLen = option auto (short 'l' <> long "max-len" <> help "max length of text to submit to TagMe" <> metavar "L")
    overlapLen = option auto (short 'L' <> long "overlap-len" <> help "length of overlaps of text to submit to TagMe" <> metavar "O")
    outputFile = option str (short 'o' <> long "output" <> metavar "FILE" <> help "Output file")
    httpTimeout = option auto (short 't' <> long "timeout" <> metavar "SECONDS" <> help "Timeout for HTTP requests")
    trainInFile = option str (short 'T' <> long "train" <> metavar "JSON" <> help "Training annotations (in JSON format)")
    predictInFile = option str (short 'P' <> long "predict" <> metavar "JSON" <> help "Prediction annotations (in JSON format)")
    groundTruthFiles = many <$> option str (short 'g' <> long "ground-truth" <> metavar "ANN" <> help "Ground truth in (*.ann format)")
    annotatePubMed' =
        annotatePubMed <$> (many inputRawDataFile) <*> outputFile <*> maxLen <*> overlapLen <*> httpTimeout

    predictToponyms' =
        predictToponyms <$> trainInFile <*> predictInFile <*> outputFile <*> groundTruthFiles

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)