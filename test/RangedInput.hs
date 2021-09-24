{-# LANGUAGE TypeApplications #-}

module RangedInput where

import ArbitraryLocalTime
import GFS

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Test.QuickCheck

-- |Data type defining all the inputs for the @cleanup@ function. The reason for
-- |the separate type is we need to generate an initial offset and a number of
-- |ranges, then generate arbitrary times inside every range, and I'm not sure
-- |it's possible to do that with @property@. Plus it seems to be a good idea
-- |anyway.
data RangedInput = RangedInput
  { riNow :: LocalTime
  , riPeriod :: Period
  , riTimes :: SortedList LocalTime
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
    --now <- arbitrary
    let now = LocalTime (fromGregorian 2000 01 01) midnight
    offsetFrom <- chooseSecond (hours 1, weeks 4)
    offsetToMultiplier <- choose @Float (1.1, 4.9)

    -- offsetTo is always bigger than offsetFrom
    let offsetTo = offsetFrom * realToFrac offsetToMultiplier

    -- range: [now - offsetTo * 1.2; now]
    timeOffset <- choose @Float (-1.2, 0)
    let time = addLocalTime (secondsToNominalDiffTime $ offsetTo * realToFrac timeOffset) now

    return $ RangedInput
      now
      (secondsToNominalDiffTime offsetFrom, secondsToNominalDiffTime offsetTo)
      (Sorted [time])
