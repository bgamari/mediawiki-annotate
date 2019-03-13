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
import Data.List
import Data.Ord
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

import qualified Data.SVM as SVM
import qualified Data.IntMap.Strict as IntMap
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
import EvalConfusionMatrix

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




tagText :: TagMe.TagMeEnv -> TagMe.Token -> Int -> Int -> T.Text -> IO [TagMe.Annotation]
tagText env tagMeToken maxLen overlapLen txt = do
    anns <- sequence
            [ do anns <- entityLinkAnnotationsConf env tagMeToken t tagMeOptions
                 return [ ann { start = (start ann) + i*maxLen
                              , end = (end ann) + i*maxLen}
                        | ann <- anns
                        ]
            | (i, t) <- zip [0..] $ overlapChunks maxLen overlapLen txt
            ]
    return $ mconcat anns



tagData :: TagMe.TagMeEnv -> TagMe.Token -> Int -> Int -> PubmedDocument -> IO [TagMe.Annotation]
tagData env tagMeToken maxLen overlapLen document =
    tagText env tagMeToken maxLen overlapLen (content document)


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

data TrainData = TrainData { vocabulary :: HM.HashMap T.Text Int
                           , allTrainData :: SVM.Problem
                           , balancedTrainData :: SVM.Problem
                           , numPos :: Int
                           , posBalancedTrainData :: SVM.Problem
                           , negBalancedTrainData :: SVM.Problem
                           }



predictToponyms :: FilePath -> FilePath -> FilePath -> FilePath -> [FilePath] -> IO ()
predictToponyms trainInFile validateInFile predictInFile outputFile groundTruthFiles = do
    print "Loading data..."
    trainData <- readPubmedAnnotations trainInFile
    validateData <- readPubmedAnnotations validateInFile
    groundTruthData <- loadGroundTruthHashMap groundTruthFiles

    print "Tuning Svm..."
    let allCategories = pubMedDataToAllFeatures trainData
    tunedSvmParam <- tuneSvmParam (isPositiveData groundTruthData) allCategories trainData validateData
    print $ "Tuning result = "<> show tunedSvmParam

    print "Tuning Svm..."
    svmModel <- trainSvm (isPositiveData groundTruthData) allCategories tunedSvmParam trainData

    print "Loading predict data..."
    predictData <- readPubmedAnnotations predictInFile

    print "Predicting with Svm..."
    preds <- mapM (predictToponymsSvm svmModel allCategories) predictData
    writePubmedAnnotations outputFile $ catMaybes $ preds
    print "Done"
  where

    predictToponymsSvm :: SVM.Model -> HM.HashMap T.Text Int -> PubmedAnnotations -> IO (Maybe PubmedAnnotations)
    predictToponymsSvm  model allCategories (pub@PubmedAnnotations {annotations = annotations }) = do
        annotation' <- filterM (onlySvmPlaceAnnotations model allCategories) annotations
        return $ if null annotation' then Nothing
                 else Just $ pub { annotations = annotation'}

    onlySvmPlaceAnnotations  :: SVM.Model -> HM.HashMap T.Text Int -> Annotation -> IO Bool
    onlySvmPlaceAnnotations model allCategories ann = do
        let vector = annToSvmVector allCategories ann
        result <- SVM.predict model vector
        return $ result >0.0

    onlyPlaceAnnotationsHeuristic :: Annotation -> Bool
    onlyPlaceAnnotationsHeuristic Annotation{..} =
        let Just categories = dbpediaCategories
            placeCats = [ cat
                        | cat <- categories
                        , pat <- placePatterns
                        , pat `T.isInfixOf` (T.toLower cat)
                        ]
        in (not $ null $ placeCats) && (spot /= "et" && spot /= "al")


-- --------------------------------
    enumerateFeatures :: Annotation -> [T.Text]
    enumerateFeatures Annotation{dbpediaCategories = Just categories} =
        [ w
        | cat <- categories
        , w <- T.words cat
        ] ++
        [ cat
        | cat <- categories
        ]

    enumerateFeatures Annotation{dbpediaCategories = Nothing} = []

    -- | produce allCategories data
    pubMedDataToAllFeatures :: [PubmedAnnotations] -> HM.HashMap T.Text Int
    pubMedDataToAllFeatures trainData =
            let allCategories = let catList = S.toList $ S.fromList
                                              $ [ feat
                                                | PubmedAnnotations {annotations =  anns} <- trainData
                                                , ann <- anns
                                                , feat <- enumerateFeatures ann
                                                ]
                                in HM.fromList $ zip catList [1..]
            in allCategories


    annToSvmVector :: HM.HashMap T.Text Int -> Annotation -> IntMap.IntMap Double
    annToSvmVector allCategories ann =
        let feats = IntMap.fromList
                      $ [ (fIdx, 1.0)
                        | feat <- enumerateFeatures ann
                        , Just fIdx <- pure $ feat `HM.lookup` allCategories
                        ]
        in feats


    pubmedDataToSvmTrainData :: ( T.Text -> Annotation -> Bool) -> HM.HashMap T.Text Int-> [PubmedAnnotations] -> TrainData
    pubmedDataToSvmTrainData isPositive allCategories trainData =
            let
                trainData'' =
                    [ (labelTo isPos, annToSvmVector allCategories ann)
                    | (isPos, ann) <- trainData'
                    ]
                numPosTrain = length $ filter fst trainData'
                (posTrainData'', negTrainData_) = partition (\(x,_)-> x>0) trainData''
                negTrainData'' =  take numPosTrain negTrainData_
                trainData''' = posTrainData'' <>  negTrainData''
            in TrainData {vocabulary = allCategories, allTrainData = trainData'', balancedTrainData=trainData''', numPos=numPosTrain, posBalancedTrainData=posTrainData'', negBalancedTrainData=negTrainData''}
      where trainData' =
                [ (isPos, ann)
                | PubmedAnnotations { doc =  PubmedDocument {filename = pubmedFilePath }, annotations =  anns} <- trainData
                , ann <- anns
                , let isPos = isPositive pubmedFilePath ann
                ]
            labelTo :: Bool -> Double
            labelTo True =  1.0
            labelTo False = -1.0

-- --------------------------------

    trainSvm :: ( T.Text -> Annotation -> Bool) -> HM.HashMap T.Text Int -> Double -> [PubmedAnnotations] -> IO SVM.Model
    trainSvm isPositive allCategories svmParam trainData  = do
        let TrainData {vocabulary = _, allTrainData = trainData'', balancedTrainData=trainData''', numPos=_, posBalancedTrainData=_, negBalancedTrainData=_}
               = pubmedDataToSvmTrainData isPositive allCategories trainData
        svmModel <- SVM.train (SVM.CSvc svmParam) SVM.Linear $ trainData'''
        return $ svmModel


    tuneSvmParam :: ( T.Text -> Annotation -> Bool) -> HM.HashMap T.Text Int -> [PubmedAnnotations] -> [PubmedAnnotations] -> IO Double
    tuneSvmParam isPositive allCategories trainData validateData = do
        let TrainData {vocabulary = _, allTrainData = trainData'', balancedTrainData=trainData''', numPos=_, posBalancedTrainData=_, negBalancedTrainData=_}
               = pubmedDataToSvmTrainData isPositive allCategories trainData
            TrainData {vocabulary = _, allTrainData = _, balancedTrainData=_, numPos=numPosTrain, posBalancedTrainData=posTrainData'', negBalancedTrainData=negTrainData''}
               = pubmedDataToSvmTrainData isPositive allCategories validateData
        criteria <- forM [0.001, 0.01, 0.05, 0.1, 0.2, 0.3, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
          (\c -> do

            svmModel <- SVM.train (SVM.CSvc c) SVM.Linear $ trainData'''
            posPred <- mapM (SVM.predict svmModel) $ fmap snd $ posTrainData''
            negPred <- mapM (SVM.predict svmModel)  $ fmap snd $ negTrainData''
            criterion <- debugOutput c posPred negPred
                         :: IO Double
            return (criterion,c)
          )
          :: IO [(Double, Double)]
        return $ snd $ maximumBy fstThenInvSnd criteria
      where fstThenInvSnd :: (Double,Double) -> (Double,Double) -> Ordering
            fstThenInvSnd (s1,c1) (s2,c2) = if s1==s2 then (compare (-c1) (-c2)) else (compare s1 s2)

            debugOutput :: Double -> [Double] -> [Double] -> IO Double
            debugOutput c posPred negPred = do
                    let confusion = (Confusion {
                                        tp= length $ filter (>0) posPred
                                      , fn = length $ filter (<=0) posPred
                                      , tn= length $ filter (<=0) negPred
                                      , fp= length $ filter (>0) negPred
                                      })
                    putStrLn $ "C="<>show c <>" f1="<> show (f1 confusion) <> " " <> show confusion <> " pos="<> show (avg posPred) <>"  neg="<> show (avg negPred)
                    return $ f1 confusion

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

avg :: [Double] -> Double
avg list = 1/ realToFrac (length list) * (Data.List.sum list)



-- ---- Command Line interface ----

helpDescr :: PP.Doc
helpDescr =
    "SemEval Toponym track 19 - TagMe baseline \n" <>
    "annotate: Convert PubMed documents to TagMe annotations.\n" <>
    "predict: Filter TagMe Annotation using SVM (training included in this step) "

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
    validateInFile = option str (short 'V' <> long "validate" <> metavar "JSON" <> help "Validation annotations (in JSON format)")
    predictInFile = option str (short 'P' <> long "predict" <> metavar "JSON" <> help "Prediction annotations (in JSON format)")
    groundTruthFiles = many $ argument str (metavar "ANN" <> help "Ground truth in (*.ann format)")


    annotatePubMed' =
        annotatePubMed <$> (many inputRawDataFile) <*> outputFile <*> maxLen <*> overlapLen <*> httpTimeout

    predictToponyms' =
        predictToponyms <$> trainInFile <*> validateInFile <*> predictInFile <*> outputFile <*> groundTruthFiles

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)