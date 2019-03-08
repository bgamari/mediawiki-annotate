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
                 deriving (Show)

aPos, aNeg :: NBTuple
aPos = NBTuple 1 0
aNeg = NBTuple 0 1

instance Monoid NBTuple where
    mempty = NBTuple 0 0
instance Semigroup NBTuple where
    NBTuple a b <> NBTuple c d = NBTuple (a+c) (b+d)



data NBLogTuple = NBLogTuple { positives, negatives :: !(Log Double) }
                 deriving (Show)
data NBModel = NBModel { totals :: NBLogTuple
                       , stats :: HM.HashMap T.Text NBLogTuple}
                 deriving (Show)

mkNaiveBayesModel :: NBTuple -> HM.HashMap T.Text NBTuple -> NBModel
mkNaiveBayesModel t s =
    NBModel { totals = toLog t, stats = fmap toLog s}
  where toLog :: NBTuple -> NBLogTuple
        toLog (NBTuple p n) = NBLogTuple (realToFrac (p+1)) (realToFrac (n+1))



nbLikelihood :: NaiveBayesMode -> NBModel -> [T.Text] -> Log Double
nbLikelihood nbMode NBModel{..} feats
  | null feats = 0.0
  | not $ any (`HM.member` stats) feats = 0.0
  | otherwise =


    let !featSet = S.fromList feats
        likelihood =
            case nbMode of
              NBFull ->
                   toRatio (NBLogTuple 1 1) totals *
                      product [ toRatio totals x
                             | f <- feats
                             , Just x <- pure $ f `HM.lookup` stats
                             ]
                   * product [ toFlipRatio totals x
                             | (f,x) <- HM.toList stats
                             , not $ f `S.member` featSet
                             ]
              NBNoClass ->
                   product [ toRatio totals x
                             | f <- feats
                             , Just x <- pure $ f `HM.lookup` stats
                             ]
                   * product [ toFlipRatio totals x
                             | (f,x) <- HM.toList stats
                             , not $ f `S.member` featSet
                             ]


              NBNoNegFeats ->
                      product [ toRatio totals x
                             | f <- feats
                             , Just x <- pure $ f `HM.lookup` stats
                             ]

    in checkNan $ likelihood
  where
    toRatio :: NBLogTuple -> NBLogTuple -> Log Double
    toRatio (NBLogTuple normPos normNeg) (NBLogTuple pos neg) =
        if pos == 0 then 1e-5 else
        if neg == 0 then 1  else
        if normPos == 0  then 1 else
            -- else (pos/normPos) / (neg/normNeg)
            pos * normNeg / (normPos * neg)
    toFlipRatio :: NBLogTuple -> NBLogTuple  -> Log Double
    toFlipRatio totals stats =
        toRatio (flip totals) (flip stats)
      where flip (NBLogTuple p n) = NBLogTuple n p

    checkNan :: Log Double -> Log Double
    checkNan score =
        if isInfinite $ ln score then
            error $ "Model score is infinite. Features: "<> show feats
        else if isNaN $ ln score then
            error $ "Model score is Nan. Features: "<> show feats
        else score



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

data Confusion = Confusion {tp::Int, tn :: Int, fp :: Int, fn::Int}
f1 ::Confusion -> Double
f1 Confusion{..} =
    let prec = (realToFrac tp)/(realToFrac (tp + fp))
        recall = (realToFrac tp)/(realToFrac (tp + fn))
    in  2 * prec * recall / (prec + recall)


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

predictToponyms :: FilePath -> FilePath -> FilePath -> FilePath -> [FilePath] -> NaiveBayesMode -> IO ()
predictToponyms trainInFile validateInFile predictInFile outputFile groundTruthFiles naiveBayesMode = do
--     let scoreThresh'' :: Log Double
--         scoreThresh'' = realToFrac scoreThresh'
    print "Loading data..."
    trainData <- readPubmedAnnotations trainInFile
    validateData <- readPubmedAnnotations validateInFile
    groundTruthData <- loadGroundTruthHashMap groundTruthFiles
--     groundTruthValData <- loadGroundTruthHashMap groundTruthFiles -- load validation ground truth

    print "Training Svm..."
    (allCategories, svmModel) <- trainSvm (isPositiveData groundTruthData) trainData
    SVM.saveModel svmModel "toponym.model.svm"

--     print "Training Naive Bayes ..."
--     let model = trainNaive (isPositiveData groundTruthData) trainData
--     print "Training Naive Bayes Threshold..."
--     let scoreThresh = trainThresh (isPositiveData groundTruthData) model validateData     -- todo load validation data from file

    print "Loading predict data..."
    predictData <- readPubmedAnnotations predictInFile
--     print "Predicting with Naive Bayes..."
--     let preds = fmap (predictToponyms model scoreThresh) predictData
    print "Predicting with Svm..."
    preds <- mapM (predictToponymsSvm svmModel allCategories) predictData
    writePubmedAnnotations outputFile $ catMaybes $ preds
    print "Done"
  where
    predictToponymsNaiveBayes :: NBModel -> Log Double -> PubmedAnnotations -> Maybe PubmedAnnotations
    predictToponymsNaiveBayes model scoreThresh (pub@PubmedAnnotations {annotations = annotations }) =
        let annotation' = filter (onlyPlaceAnnotations model scoreThresh) annotations
        in if null annotation' then Nothing
           else Just $ pub { annotations = annotation'}

    predictToponymsSvm :: SVM.Model -> HM.HashMap T.Text Int -> PubmedAnnotations -> IO (Maybe PubmedAnnotations)
    predictToponymsSvm  model allCategories (pub@PubmedAnnotations {annotations = annotations }) = do
        annotation' <- filterM (onlySvmPlaceAnnotations model allCategories) annotations
        return $ if null annotation' then Nothing
                 else Just $ pub { annotations = annotation'}

    onlyPlaceAnnotations model scoreThresh ann = (predictNaive naiveBayesMode  model ann > scoreThresh)

    onlySvmPlaceAnnotations  :: SVM.Model -> HM.HashMap T.Text Int -> Annotation -> IO Bool
    onlySvmPlaceAnnotations model allCategories ann = do
        let vector = annToSvmVector allCategories ann
        result <- SVM.predict model vector
        return $ Debug.trace (show result) $ result >0.0

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

    annToSvmVector :: HM.HashMap T.Text Int -> Annotation -> IntMap.IntMap Double
    annToSvmVector allCategories Annotation{dbpediaCategories = Just categories} =
        let catSet = S.fromList categories
            feats = IntMap.fromList
                      $ [ (fIdx, 1.0)
                        | cat <- categories
                        , w <- T.words cat
                        , let Just fIdx = w `HM.lookup` allCategories
                        ]
        in feats

    trainSvm:: ( T.Text -> Annotation -> Bool) -> [PubmedAnnotations] -> IO (HM.HashMap T.Text Int,  SVM.Model)
    trainSvm isPositive trainData = do
        let allCategories = let catList = S.toList $ S.fromList
                                          $ [w
                                          | (isPos, Annotation{dbpediaCategories = Just categories}) <- trainData'
                                          , cat <- categories
                                          , w <- T.words cat
                                          ]
                            in HM.fromList $ zip catList [1..]
            trainData =
                [ (labelTo isPos, annToSvmVector allCategories ann)
                | (isPos, ann) <- trainData'
                ]
            numPosTrain = length $ filter fst trainData'
            trainData'' = filter (\(x,_)-> x>0) trainData
            !bla = Debug.trace (unlines $ fmap show trainData'') $ 0

        cvResult <-  SVM.crossValidate (SVM.OneClassSvm 0.1) SVM.Linear trainData'' 5
        let !bla1 =  Debug.trace ("crossvalidate" <> show cvResult) $ 0
        svmModel <- SVM.train (SVM.OneClassSvm 0.1) SVM.Linear $ trainData''
        return $ (allCategories, svmModel)
      where trainData' =
                [ (isPos, ann)
                | PubmedAnnotations { doc =  PubmedDocument {filename = pubmedFilePath }, annotations =  anns} <- trainData
                , ann <- anns
                , let isPos = isPositive pubmedFilePath ann
                ]
            labelTo :: Bool -> Double
            labelTo True =  1.0
            labelTo False = -1.0


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

    trainThresh  :: ( T.Text -> Annotation -> Bool) -> NBModel -> [PubmedAnnotations] -> Log Double
    trainThresh isPositive model validateData =
            let predictions = [ (score, isPos)
                              | PubmedAnnotations {doc = PubmedDocument {filename = fname}, annotations = annotations} <- validateData
                              , ann <- annotations
                              , let score =  predictNaive naiveBayesMode model ann
                              , let isPos = isPositive fname ann
                              ]
                predictions' = sortOn (Down. fst) predictions
--                 thresh50 = scoreThresHalfRecall predictions'
--                 threshGaussian = scoreHalfGaussian predictions'
--                 threshMedian = scoreHalfMedian predictions'
                threshF1 = scoreThreshMaxF1 predictions'

                preds :: [(Confusion, (Log Double, Bool))]
                !preds = rankQuality predictions'
                xxx :: Log Double -> (Log Double, Double)
                xxx thresh =
                    let ((conf, (_, _)) : _) = dropWhile (\(_, (score, _)) -> score > thresh) preds
                    in (thresh, f1 conf)
            in Debug.trace ("threshF1="<> show (xxx threshF1) )
--                        <> " thresh50="<> show (xxx thresh50)
--                        <> " threshGaussian="<> show (xxx threshGaussian)
--                        <> " threshMedian="<> show (xxx threshMedian)  )
                       $ threshF1
      where
                scoreThresHalfRecall predictions' =
                    let numPos = length $ filter snd predictions'
                        predictCountAccumPos :: [(Int, (Log Double, Bool))]
                        x:: (Int, [(Int, (Log Double, Bool))])
                        x@(_, predictCountAccumPos) =
                           mapAccumL f 0 predictions'
                            where f :: Int -> (Log Double, Bool) -> (Int, (Int, (Log Double, Bool)))
                                  f count x@(_, isPos) = (count', (count', x))
                                    where
                                      count'
                                        | isPos = count+1
                                        | otherwise = count
                        scoreThresh :: Log Double
                        (_, (scoreThresh, _)) = head $ dropWhile (\(c,_) -> c < numPos `div` 2) predictCountAccumPos
                    in scoreThresh

                scoreThreshMaxF1 predictions' =
                    let numPos = length $ filter snd predictions'
                        numTotal = length predictions'

                        predictCountAccumPos :: [(Double, (Log Double, Bool))]
                        x:: (Confusion, [(Double, (Log Double, Bool))])
                        x@(_, predictCountAccumPos) =
                           mapAccumL f Confusion{tp =0, fn=0, fp=numPos, tn=(numTotal-numPos)} predictions'
                            where f :: Confusion -> (Log Double, Bool) -> (Confusion, (Double, (Log Double, Bool)))
                                  f conf@Confusion{} x@(_, isPos) = (conf', (f1 conf', x))
                                    where
                                      conf'
                                        | isPos =     conf{ tp= (tp conf)+1, fp= (fp conf)-1}
                                        | otherwise = conf{ fn= (fn conf)+1, tn= (tn conf)-1}
                        scoreThresh :: Log Double
                        (_, (scoreThresh, _)) = maximumBy (comparing fst) predictCountAccumPos
                    in scoreThresh

                rankQuality :: [(Log Double, Bool)] -> [(Confusion, (Log Double, Bool))]
                rankQuality predictions' =
                    let numPos = length $ filter snd predictions'
                        numTotal = length predictions'

                        predictCountAccumPos :: [(Confusion, (Log Double, Bool))]
                        x:: (Confusion, [(Confusion, (Log Double, Bool))])
                        x@(_, predictCountAccumPos) =
                           mapAccumL f Confusion{tp =0, fn=0, fp=numPos, tn=(numTotal-numPos)} predictions'
                            where f :: Confusion -> (Log Double, Bool) -> (Confusion, (Confusion, (Log Double, Bool)))
                                  f conf@Confusion{} x@(_, isPos) = (conf', (conf', x))
                                    where
                                      conf'
                                        | isPos =     conf{ tp= (tp conf)+1, fp= (fp conf)-1}
                                        | otherwise = conf{ fn= (fn conf)+1, tn= (tn conf)-1}
                    in  predictCountAccumPos

                scoreHalfGaussian predictions' =
                    let (poss, negs) = partition snd predictions'
                    in (avg poss) + (avg negs) /2

                  where avg list = 1/ realToFrac (length list) * (Numeric.Log.sum $ fmap fst list)

                scoreHalfMedian predictions' =
                    let (poss, negs) = partition snd predictions'
                    in (med poss) + (med negs) /2

                  where med list =
                          let (medScore, _) = last $ take (length list `div` 2) list
                          in medScore


    predictNaive :: NaiveBayesMode -> NBModel -> Annotation -> Log Double
    predictNaive naiveBayesMode model Annotation{dbpediaCategories = Just categories} =
        let score = nbLikelihood naiveBayesMode model categories
        in Debug.trace (show $ ln score)$ score
--         in score

    isPositiveData :: HM.HashMap T.Text ([Offsets], [Offsets]) ->  T.Text -> Annotation -> Bool
    isPositiveData groundTruthData docname Annotation{..} =
        case docname `HM.lookup` groundTruthData of
          Just (posOffsets, negOffsets) -> let res = not $ null $ filter ((start, end) `offsetsIntersect`) posOffsets
                                           in Debug.trace (show docname <> show res) res
                                           --in Debug.trace ("posOffsets" <> show posOffsets <> "\n searching "<> show (start, end)) res
          Nothing -> False
      where offsetsIntersect :: Offsets -> Offsets -> Bool
            offsetsIntersect (s1, e1)  (s2, e2) =
                if s1 > s2 && s1 < e2 then (Debug.trace "isPositive" True)
                else if s2 > s1 && s2 < e1 then (Debug.trace "isPositive" True)
                   else False

data NaiveBayesMode =  NBFull | NBNoClass | NBNoNegFeats


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


    naiveBayesMode = flag' NBFull ( long "nb-full" <> help "full naive bayes likelihood")
                       <|> flag' NBNoClass ( long "nb-no-class" <> help "naive bayes likelihood without class prior")
                       <|> flag' NBNoNegFeats ( long "nb-no-neg-feats" <> help "naive bayes likelihood without class prior and without negative feature information")


--     scoreThresh = option auto (long "score-thresh" <> metavar "DOUBLE" <> help "Minimum threshold for positive place prediction")
    annotatePubMed' =
        annotatePubMed <$> (many inputRawDataFile) <*> outputFile <*> maxLen <*> overlapLen <*> httpTimeout

    predictToponyms' =
        predictToponyms <$> trainInFile <*> validateInFile <*> predictInFile <*> outputFile <*> groundTruthFiles <*> naiveBayesMode

main :: IO ()
main = join $ execParser' 1 (helper <*> opts) (progDescDoc $ Just helpDescr)