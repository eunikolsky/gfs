module RangedInput where

import ArbitraryLocalTime
import GFS

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Word
import Test.QuickCheck

data RangeInput = RangeInput
  { rangeMaxTime :: LocalTime
  , rangeTimes :: [LocalTime]
  }
  deriving Show

-- |Data type defining all the inputs for the @cleanup@ function. The reason for
-- |the separate type is we need to generate an initial offset and a number of
-- |ranges, then generate arbitrary times inside every range, and I'm not sure
-- |it's possible to do that with @property@. Plus it seems to be a good idea
-- |anyway.
data RangedInput = RangedInput
  { riNow :: LocalTime
  , riPeriod :: Period
  --, riNumRanges :: Positive Word8
  , riRanges :: NonEmptyList RangeInput
  }
  deriving Show

nominalHour = 60 * 60

-- |Returns the number of seconds for the given number of hours.
hours :: Int -> Int
hours = (* nominalHour)

-- |Returns the number of seconds for the given number of weeks.
weeks :: Int -> Int
weeks = (* nominalWeek)
  where nominalWeek = nominalHour * 24 * 7

instance Arbitrary RangedInput where
  arbitrary = do
    now <- arbitrary
    offsetFrom <- chooseSecond (hours 1, weeks 4)
    numRanges <- chooseInt (1, 5)

    let
      -- note: this always generates an @offsetTo@ that is an integer multiplier
      -- away from @offsetFrom@
      offsetTo = offsetFrom * (fromIntegral numRanges + 1)
      range = RangeInput (LocalTime (ModifiedJulianDay 0) midnight) []
    return $ RangedInput
      now
      (secondsToNominalDiffTime offsetFrom, secondsToNominalDiffTime offsetTo)
      (NonEmpty $ replicate numRanges range)
