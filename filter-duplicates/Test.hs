{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Numeric
import Data.Foldable
import Data.Bits
import Test.QuickCheck hiding ((.&.))
import CAR.FilterDuplicates.Word1024

newtype N = N {getN :: Integer}
          deriving (Eq, Ord, Num, Enum, Bits)

instance Show N where
    showsPrec _ (N n) = showHex n

instance Bounded N where
    minBound = 0
    maxBound = (1 `shiftL` 1024) - 1

instance Arbitrary N where
    arbitrary = (.&. maxBound) . N <$> arbitrary

instance Arbitrary Word1024 where
    arbitrary = toWord1024 <$> arbitrary

toWord1024 :: N -> Word1024
toWord1024 = integerToWord1024 . getN

fromWord1024 :: Word1024 -> N
fromWord1024 = N . word1024ToInteger

main = quickCheck $ conjoin
    [ counterexample "roundtrip" propRoundtrip
    , counterexample "ors" propOr
    , counterexample "ands" propAnd
    ]

propRoundtrip :: N -> Bool
propRoundtrip n =
    word1024ToInteger (toWord1024 n) == getN n

propOr :: [N] -> Property
propOr ns =
    let res = foldl' (.|.) 0 ns
    in fromWord1024 (orWord1024s (map toWord1024 ns)) === res

propAnd :: [N] -> Property
propAnd ns =
    let res = foldl' (.&.) maxBound ns
    in fromWord1024 (andWord1024s (map toWord1024 ns)) === res
