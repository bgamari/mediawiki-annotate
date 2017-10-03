import Data.HashMap.Strict as HM
import System.Environment
import Data.Time.Clock
import AssessmentEval

main = do
    fileA:fileB:[] <- getArgs

    let dateA = dateFromFilepath fileA
    a <- readAssessments fileA

    let dateB = dateFromFilepath fileB
    b <- readAssessments fileB

    let new = b `HM.difference` a
        dt = dateB `diffUTCTime` dateA
        rate :: Double
        rate = realToFrac dt / realToFrac (HM.size new)
    putStrLn $ "new assessments: " ++ show (HM.size new)
    putStrLn $ "time: " ++ show dt
    putStrLn $ "seconds per assessment: " ++ show rate

