module ArbitraryLocalTime where

import Data.Fixed
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Test.QuickCheck

-- This implements @Arbitrary@ for a valid @LocalTime@ and its parts directly.
-- It's not clear why QuickCheck doesn't have this out of the box; maybe because
-- there are multiple possible implementations and it's better to implement it
-- on a more specific newtype?
-- https://blog.ploeh.dk/2019/09/02/naming-newtypes-for-quickcheck-arbitraries/
instance Arbitrary Day where
  arbitrary = do
    year <- chooseInteger (1990, 2030)
    month <- chooseInt (1, 12)
    day <- chooseInt (1, 31)
    return $ fromGregorian year month day

instance Arbitrary TimeOfDay where
  arbitrary = do
    hour <- chooseInt (0, 23)
    minute <- chooseInt (0, 59)
    intSecond <- chooseInteger (0, 60)
    -- TODO extract to `Arbitrary Second`
    let second = MkFixed (intSecond * resolution (0 :: Pico)) :: Pico
    return $ TimeOfDay hour minute second

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary NominalDiffTime where
  arbitrary = secondsToNominalDiffTime . MkFixed <$> arbitrary
