{-# LANGUAGE TypeApplications #-}

module RangedInput where

import ArbitraryLocalTime
import GFS

import Control.Arrow ((>>>))
import Data.Coerce (coerce)
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

type WithNow a = (Now, Newest, a)

class Num a => NumSubperiods a where
  ceiling_ :: a -> Int
  choose_ :: Gen a
  zeroList :: a -> [a]

instance NumSubperiods Int where
  ceiling_ = id
  choose_ = chooseInt (1, 10)
  zeroList x = [0..x]

instance NumSubperiods Float where
  ceiling_ = ceiling
  choose_ = choose (1.0, 10.0)
  zeroList = floatList

type BaseTestData a = WithNow (Period, Times, a, NewestTimes)

arbitraryNow :: (Now -> Gen a) -> Gen (WithNow a)
arbitraryNow f = do
  -- TODO a fixed date is easier for development; start from a random value and shrink towards 2000-01-01?
  let now = LocalTime (fromGregorian 2000 01 01) midnight
      newest = addLocalTime (secondsToNominalDiffTime (-1 :: Pico)) now

  x <- f now

  pure (now, newest, x)

newtype Offset = Offset Int
newtype NewestOffset = NewestOffset Int

arbitraryBaseTestData :: NumSubperiods a => Gen a -> (Int -> Int -> a -> Gen ([Offset], [NewestOffset])) -> Gen (BaseTestData a)
arbitraryBaseTestData genNumSubperiods genOffsets = arbitraryNow $ \now -> do
  offsetFrom <- hours <$> chooseInt (1, 24) -- chooseSecond (hours 1, weeks 1)
  numSubperiods <- genNumSubperiods
  -- offsetTo is always bigger than offsetFrom
  let offsetTo = ceiling_ $ fromIntegral offsetFrom * (numSubperiods + 1)

  (offsets, newestOffsets) <- genOffsets offsetFrom offsetTo numSubperiods

  let times = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate <$> coerce offsets
      newestTimes = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate <$> coerce newestOffsets

  pure
    ( ( PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds $ offsetFrom
      , PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds $ offsetTo
      )
    , (Sorted $ sort times)
    , numSubperiods
    , (Sorted $ sort newestTimes)
    )

-- |Generates an arbitrary input for `cleanup` such that the times are outside
-- |the generated period (within certain bounds).
arbitraryInputOutsideOfRange :: Gen (BaseTestData Float)
arbitraryInputOutsideOfRange = arbitraryBaseTestData
  (choose @Float (1.1, 4.9))
  $ \offsetFrom offsetTo _ -> do
    -- range: [now - offsetTo * 2; now - offsetTo) ∪ (now - offsetFrom; now]
    offsets <- listOf1 $ ceiling @_ @Int <$> oneof
      [ (* (fromIntegral offsetTo)) <$> choose @Float (1.001, 2.0)
      , (* (fromIntegral offsetFrom)) <$> choose @Float (0.0, 0.999)
      ]
    pure (coerce offsets, [])

-- |Generates an arbitrary input for `cleanup` such that the number of times
-- |matches the number of subperiods in the period.
arbitraryInputWithinRange :: Gen (BaseTestData Int)
-- TODO join `Period` and `NumSubperiods` into a logically single type?
arbitraryInputWithinRange = arbitraryBaseTestData
  (chooseInt (1, 10))
  $ \offsetFrom offsetTo numSubperiods -> do
    offsets <- vectorOf numSubperiods (choose (offsetFrom, offsetTo))
    pure (coerce offsets, [])

type NewestTimes = Times

-- |Generates an arbitrary input for `cleanup` such that each subperiod has at
-- |least two times; the newest times in each one are _also_ returned separately
-- |for the ease of writing tests.
-- Sample output:
-- `(2000-01-01 00:00:00,1999-12-31 23:59:59,(1 d,5 d),Sorted {getSorted = [1999-12-27 15:53:04,1999-12-28 00:50:13,1999-12-29 20:36:03,1999-12-30 14:32:27]},Sorted {getSorted = [1999-12-27 15:53:04,1999-12-27 16:53:56,1999-12-27 18:52:44,1999-12-27 19:24:03,1999-12-27 21:07:00,1999-12-27 22:50:25,1999-12-28 00:50:13,1999-12-28 00:55:15,1999-12-28 02:22:39,1999-12-28 08:52:45,1999-12-28 13:19:33,1999-12-29 20:36:03,1999-12-29 23:19:06,1999-12-30 14:32:27,1999-12-30 14:55:48,1999-12-30 17:32:40,1999-12-30 18:40:20,1999-12-30 19:25:54,1999-12-30 19:58:30,1999-12-30 21:17:13,1999-12-30 23:45:06]})`
arbitraryInputWithinRangeSubperiods :: NumSubperiods a => Gen (BaseTestData a)
arbitraryInputWithinRangeSubperiods = arbitraryBaseTestData
  choose_
  $ \offsetFrom offsetTo numSubperiods -> do
      generatedOffsets <- traverse (generateOffsets offsetFrom) . adjacentPairs $ zeroList numSubperiods
      let (newestOffsets, offsets) = sequence $ (\(newestTime, times) -> ([newestTime], times)) <$> generatedOffsets

      pure (concat offsets ++ coerce newestOffsets, newestOffsets)

      where
        --generateOffsets :: Int -> (Float, Float) -> Gen (NewestOffset, [Offset])
        generateOffsets offsetFrom (numSubperiodFrom, numSubperiodTo) = do
          -- note: both offsets are shifted relative to `offsetFrom` in order not to start from `now`,
          -- that's where the extra `+ 1` comes from
          let subperiodFrom = ceiling_ $ fromIntegral offsetFrom * (numSubperiodFrom + 1)
              subperiodTo = ceiling_ $ fromIntegral offsetFrom * (numSubperiodTo + 1)
          newestOffset <- choose (subperiodFrom + 1, subperiodTo)
          numOffsets <- chooseInt (1, 2)
          offsets <- fmap (fmap Offset) . vectorOf numOffsets $ choose (newestOffset, subperiodTo)
          pure (NewestOffset newestOffset, offsets)

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

-- |Generates a list of integer values starting at `0` and ending with the given values.
-- |E.g. `floatList 3.5 = [0.0, 1.0, 2.0, 3.0, 3.5]`.
floatList :: Float -> [Float]
floatList x = avoidDuplicateX $ (fromIntegral <$> [0,1..floor x]) ++ [x]
  -- for a case when `x` is an integral value (`x.0`)
  where avoidDuplicateX = nub
