{-# LANGUAGE TypeApplications #-}

module RangedInput where

import ArbitraryLocalTime
import GFS

import Control.Arrow ((>>>))
import Data.Fixed
import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Test.QuickCheck

-- An attempt to clarify the tuples' values in the generators below. A poor
-- replacement for a proper data type, but different generators return a
-- different detalization of data and combining all the different variants
-- into one type would make the test code more complicated.
type Now = LocalTime
type Newest = LocalTime
type Times = SortedList LocalTime

-- |Generates an arbitrary input for `cleanup` such that the times are outside
-- |the generated period (within certain bounds).
arbitraryInputOutsideOfRange :: Gen (Now, Newest, Period, Times)
arbitraryInputOutsideOfRange = do
  -- TODO a fixed date is easier for development
  let now = LocalTime (fromGregorian 2000 01 01) midnight
      newest = addLocalTime (secondsToNominalDiffTime (-1 :: Pico)) now
  offsetFrom <- chooseSecond (hours 1, weeks 1)
  offsetToMultiplier <- choose @Float (1.1, 4.9)

  -- offsetTo is always bigger than offsetFrom
  let offsetTo = offsetFrom * realToFrac offsetToMultiplier

  -- range: [now - offsetTo * 2; now - offsetTo] ∪ [now - offsetFrom; now]
  timeOffset <- oneof
    [ (* offsetTo) . realToFrac <$> choose @Float (-2, -1)
    , (* offsetFrom) . realToFrac <$> choose @Float (-1, 0)
    ]
  let time = addLocalTime (secondsToNominalDiffTime timeOffset) now

  return
    ( now
    , newest
    , ( PrettyTimeInterval $ secondsToNominalDiffTime offsetFrom
      , PrettyTimeInterval $ secondsToNominalDiffTime offsetTo
      )
    , (Sorted [time])
    )

type NumSubperiods = Int

-- |Generates an arbitrary input for `cleanup` such that there is at least
-- |one time in each of the subperiods of the period.
arbitraryInputWithinRange :: Gen (Now, Newest, Period, NumSubperiods, Times)
arbitraryInputWithinRange = do
  -- TODO a fixed date is easier for development; start from a random value and shrink towards 2000-01-01?
  let now = LocalTime (fromGregorian 2000 01 01) midnight
      newest = addLocalTime (secondsToNominalDiffTime (-1 :: Pico)) now
  --offsetFrom <- chooseInt (hours 1, weeks 1)
  offsetFrom <- hours <$> chooseInt (1, 24)
  numSubperiods <- chooseInt (1, 10)

  -- offsetTo is always bigger than offsetFrom
  let offsetTo = offsetFrom * (numSubperiods + 1)

  arbitraryOffsets <- vectorOf numSubperiods (choose (offsetFrom, offsetTo))
  let times = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate <$> arbitraryOffsets

  return
    ( now
    , newest
    , ( PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds $ offsetFrom
      , PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds $ offsetTo
      )
    , numSubperiods
    , (Sorted $ sort times)
    )

intToSeconds :: Int -> Pico
intToSeconds = MkFixed . (* (resolution (0 :: Pico))) . fromIntegral

instance Arbitrary PrettyTimeInterval where
  arbitrary = PrettyTimeInterval <$> arbitrary

nominalHour = 60 * 60

-- |Returns the number of seconds for the given number of hours.
hours :: Int -> Int
hours = (* nominalHour)

-- |Returns the number of seconds for the given number of weeks.
weeks :: Int -> Int
weeks = (* nominalWeek)
  where nominalWeek = nominalHour * 24 * 7

-- |Returns a list of adjacent pairs from the source list.
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs xs = zip xs (tail xs)
