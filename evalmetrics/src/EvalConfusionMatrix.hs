{-# LANGUAGE RecordWildCards #-}

module EvalConfusionMatrix
    ( Confusion(..)
    , f1
    , precision
    , recall
    )
where


data Confusion = Confusion {tp::Int, tn :: Int, fp :: Int, fn::Int}
                    deriving (Show)
f1 ::Confusion -> Double
f1 confusion =
    let prec' = precision confusion
        recall' = recall confusion
    in  2 * prec' * recall' / (prec' + recall')

precision :: Confusion -> Double
precision Confusion{..} =
    (realToFrac tp)/(realToFrac (tp + fp))

recall :: Confusion -> Double
recall Confusion{..} =
    (realToFrac tp)/(realToFrac (tp + fn))
