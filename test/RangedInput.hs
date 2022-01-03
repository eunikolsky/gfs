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

-- |Generates an arbitrary input for `cleanup` such that the number of times
-- |matches the number of subperiods in the period.
arbitraryInputWithinRange :: Gen (Now, Newest, Period, NumSubperiods, Times)
-- TODO join `Period` and `NumSubperiods` into a logically single type?
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
  -- TODO add base `offsetFrom`? (see `arbitraryInputWithinRangeSubperiods`)
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

type NewestTimes = Times

-- |Generates an arbitrary input for `cleanup` such that each subperiod has at
-- |least two times; the newest times in each one are _also_ returned separately
-- |for the ease of writing tests.
-- Sample output:
-- `(2000-01-01 00:00:00,1999-12-31 23:59:59,(1 d,5 d),Sorted {getSorted = [1999-12-27 15:53:04,1999-12-28 00:50:13,1999-12-29 20:36:03,1999-12-30 14:32:27]},Sorted {getSorted = [1999-12-27 15:53:04,1999-12-27 16:53:56,1999-12-27 18:52:44,1999-12-27 19:24:03,1999-12-27 21:07:00,1999-12-27 22:50:25,1999-12-28 00:50:13,1999-12-28 00:55:15,1999-12-28 02:22:39,1999-12-28 08:52:45,1999-12-28 13:19:33,1999-12-29 20:36:03,1999-12-29 23:19:06,1999-12-30 14:32:27,1999-12-30 14:55:48,1999-12-30 17:32:40,1999-12-30 18:40:20,1999-12-30 19:25:54,1999-12-30 19:58:30,1999-12-30 21:17:13,1999-12-30 23:45:06]})`
arbitraryInputWithinRangeSubperiods :: Gen (Now, Newest, Period, NewestTimes, Times)
arbitraryInputWithinRangeSubperiods = do
  let now = LocalTime (fromGregorian 2000 01 01) midnight
      newest = addLocalTime (secondsToNominalDiffTime (-1 :: Pico)) now
  offsetFrom <- pure $ hours 2 -- <$> choose (1, 24)
  numSubperiods <- pure 5 -- choose (1, 10)

  let offsetTo = offsetFrom * (numSubperiods + 1)
      generateOffsets :: Int -> Gen (Int, [Int])
      generateOffsets numSubperiod = do
        -- note: both offsets are shifted relative to `offsetFrom` in order not to start from `now`,
        -- that's where the extra `+ 1` comes from
        let (subperiodFrom, subperiodTo) = (offsetFrom * (numSubperiod + 1), offsetFrom * (numSubperiod + 1 + 1))
        newestOffset <- choose (subperiodFrom, subperiodTo)
        numOffsets <- chooseInt (1, 2)
        offsets <- vectorOf numOffsets $ choose (newestOffset, subperiodTo)
        pure (newestOffset, offsets)

  generatedOffsets <- traverse generateOffsets [0..numSubperiods-1]
  let (newestOffsets, offsets) = sequence $ (\(newestTime, times) -> ([newestTime], times)) <$> generatedOffsets
      newestTimes = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate <$> newestOffsets
      times = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate <$> concat offsets

  pure
    ( now
    , newest
    , ( PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds $ offsetFrom
      , PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds $ offsetTo
      )
    , (Sorted $ sort newestTimes)
    , (Sorted . sort $ times ++ newestTimes)
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
