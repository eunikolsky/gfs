module ArbitraryLocalTime where

import GFS

import Control.Applicative
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

-- |Chooses an arbitrary integer second in the given @range@.
chooseSecond :: (Int, Int) -> Gen Pico
chooseSecond range = do
    -- TODO extract to `Arbitrary Second`?
    intSecond <- chooseInt range
    return . MkFixed $ (fromIntegral intSecond) * (resolution (0 :: Pico))

instance Arbitrary TimeOfDay where
  arbitrary = do
    hour <- chooseInt (0, 23)
    minute <- chooseInt (0, 59)
    second <- chooseSecond (0, 60)
    return $ TimeOfDay hour minute second

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

instance Arbitrary NominalDiffTime where
  arbitrary = secondsToNominalDiffTime . MkFixed <$> arbitrary

-- TODO derived by compiler?
instance Arbitrary Period where
  arbitrary = Period <$> liftA2 (,) arbitrary arbitrary
