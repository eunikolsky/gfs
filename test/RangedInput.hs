{-# LANGUAGE TypeApplications #-}

module RangedInput where

import ArbitraryLocalTime
import GFS

import Control.Arrow ((>>>))
import Control.Monad (join)
import Data.Fixed
import Data.Functor.Identity (Identity(..))
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

import Control.Monad.Loops (unfoldrM)
import Test.QuickCheck

-- An attempt to clarify the tuples' values in the generators below. A poor
-- replacement for a proper data type, but different generators return a
-- different detalization of data and combining all the different variants
-- into one type would make the test code more complicated.
type Now = LocalTime

newtype Times = Times { unTimes :: SortedList LocalTime }
  deriving Show
newtype NewestTimes = NewestTimes { unNewestTimes :: SortedList LocalTime }
  deriving Show

type WithNow a = (Now, a)

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

type PeriodInfo f a = {-NumSubperiods a =>-} (Offsets, f a)

type BaseTestData f a = WithNow (PeriodInfo f a, Times, NewestTimes)

arbitraryNow :: (Now -> Gen a) -> Gen (WithNow a)
arbitraryNow f = do
  -- TODO a fixed date is easier for development; start from a random value and shrink towards 2000-01-01?
  let now = LocalTime (fromGregorian 2000 01 01) midnight

  x <- f now

  pure (now, x)

newtype Offset = Offset { unOffset :: Int }
newtype NewestOffset = NewestOffset { unNewestOffset :: Int }

arbitraryBaseTestData :: NumSubperiods a => Gen a -> (Offset -> Offset -> a -> Gen ([Offset], [NewestOffset])) -> Gen (BaseTestData Identity a)
arbitraryBaseTestData genNumSubperiods genOffsets = arbitraryNow $ \now -> do
  offsetFrom <- Offset . hours <$> chooseInt (1, 24) -- chooseSecond (hours 1, weeks 1)
  numSubperiods <- genNumSubperiods
  -- offsetTo is always bigger than offsetFrom
  let offsetTo = Offset . ceiling_ $ fromIntegral (unOffset offsetFrom) * (numSubperiods + 1)

  (offsets, newestOffsets) <- genOffsets offsetFrom offsetTo numSubperiods

  let times = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate . unOffset <$> offsets
      newestTimes = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate . unNewestOffset <$> newestOffsets

  pure
    ( ( Offsets $ PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds . unOffset <$> NE.fromList [offsetFrom, offsetTo]
      , Identity numSubperiods
      )
    , (Times . Sorted $ sort times)
    , (NewestTimes . Sorted $ sort newestTimes)
    )

-- |Generates an arbitrary input for `cleanup` such that the times are outside
-- |the generated period (within certain bounds).
arbitraryInputOutsideOfRange :: Gen (BaseTestData Identity Float)
arbitraryInputOutsideOfRange = arbitraryBaseTestData
  (choose @Float (1.1, 4.9))
  $ \(Offset offsetFrom) (Offset offsetTo) _ -> do
    -- range: [now - offsetTo * 2; now - offsetTo) ∪ (now - offsetFrom; now]
    offsets <- listOf1 $ Offset . ceiling @_ @Int <$> oneof
      [ (* (fromIntegral offsetTo)) <$> choose @Float (1.001, 2.0)
      , (* (fromIntegral offsetFrom)) <$> choose @Float (0.0, 0.999)
      ]
    pure (offsets, [])

-- |Generates an arbitrary input for `cleanup` such that the number of times
-- |matches the number of subperiods in the period.
arbitraryInputWithinRange :: Gen (BaseTestData Identity Int)
arbitraryInputWithinRange = arbitraryBaseTestData
  (chooseInt (1, 10))
  $ \(Offset offsetFrom) (Offset offsetTo) numSubperiods -> do
    offsets <- vectorOf numSubperiods (Offset <$> choose (offsetFrom + 1, offsetTo - 1))
    pure (offsets, [])

-- |Generates an arbitrary input for `cleanup` such that each subperiod has at
-- |least two times; the newest times in each one are _also_ returned separately
-- |for the ease of writing tests.
-- Sample output:
-- `(2000-01-01 00:00:00,1999-12-31 23:59:59,(1 d,5 d),Sorted {getSorted = [1999-12-27 15:53:04,1999-12-28 00:50:13,1999-12-29 20:36:03,1999-12-30 14:32:27]},Sorted {getSorted = [1999-12-27 15:53:04,1999-12-27 16:53:56,1999-12-27 18:52:44,1999-12-27 19:24:03,1999-12-27 21:07:00,1999-12-27 22:50:25,1999-12-28 00:50:13,1999-12-28 00:55:15,1999-12-28 02:22:39,1999-12-28 08:52:45,1999-12-28 13:19:33,1999-12-29 20:36:03,1999-12-29 23:19:06,1999-12-30 14:32:27,1999-12-30 14:55:48,1999-12-30 17:32:40,1999-12-30 18:40:20,1999-12-30 19:25:54,1999-12-30 19:58:30,1999-12-30 21:17:13,1999-12-30 23:45:06]})`
arbitraryInputWithinRangeSubperiods :: NumSubperiods a => Gen (BaseTestData Identity a)
arbitraryInputWithinRangeSubperiods = arbitraryBaseTestData
  choose_
  $ \offsetFrom _ numSubperiods -> do
      generatedOffsets <- traverse (arbitraryOffsets offsetFrom) . adjacentPairs $ zeroList numSubperiods
      let (newestOffsets, offsets) = sequence $ (\(newestTime, times) -> ([newestTime], times)) <$> generatedOffsets

      pure (concat offsets, newestOffsets)

-- |Generates an arbitrary input for `cleanup` with one period starting at `now`
-- |and multiple times there.
arbitraryInputWithinRangeFromNow :: Gen (BaseTestData Identity Int)
arbitraryInputWithinRangeFromNow = arbitraryNow $ \now -> do
  let offsetFrom = 0
      numSubperiods = 1
  offsetTo <- hours <$> chooseInt (1, 24)

  (offsets, newestOffsets) <- do
      newestOffset <- choose (offsetFrom + 1, offsetTo - 1)
      numOffsets <- chooseInt (1, 10)
      offsets <- vectorOf numOffsets $ choose (newestOffset + 1, offsetTo - 1)

      pure (offsets ++ [newestOffset], [newestOffset])

  let times = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate <$> offsets
      newestTimes = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate <$> newestOffsets

  pure
    ( ( Offsets $ PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds <$> NE.fromList [offsetFrom, offsetTo]
      , Identity numSubperiods
      )
    , (Times . Sorted $ sort times)
    , (NewestTimes . Sorted $ sort newestTimes)
    )

-- |Generates arbitrary time offsets (newest offsets are also included in all
-- |offsets) based on the initial offset of a period and the number of subperiod
-- |range in that period.
--
-- Example:
--  * `offsetFrom = hours 4`
--  * `numSubperiod = (3, 3.5)`
--  * => returns offsets in `(hours (4 * (3 + 1)), hours (4 * (3.5 + 1))) =
--       = (hours 16, hours 18)`
--
-- Note: the extra `+ 1` comes from the fact that the subperiod itself is shifted
-- from "zero" (i.e., "now") by `offsetFrom`, and we need to generate the
-- offsets relative to `offsetFrom`, not "zero".
arbitraryOffsets :: NumSubperiods a => Offset -> (a, a) -> Gen (NewestOffset, [Offset])
arbitraryOffsets (Offset offsetFrom) (numSubperiodFrom, numSubperiodTo) = do
  let subperiodFrom = ceiling_ $ fromIntegral offsetFrom * (numSubperiodFrom + 1)
      subperiodTo = ceiling_ $ fromIntegral offsetFrom * (numSubperiodTo + 1)
  newestOffset <- choose (subperiodFrom + 1, subperiodTo - 1)
  numOffsets <- chooseInt (1, 2)
  arbitraryNewerOffsets <- vectorOf numOffsets $ choose (newestOffset + 1, subperiodTo - 1)
  let offsets = newestOffset : arbitraryNewerOffsets
  pure (NewestOffset newestOffset, Offset <$> offsets)

-- |A control parameter for `arbitraryMultiPeriodBaseTestData` that defines
-- |whether all periods will have times or some will be without times.
-- |This type is much more self-explanatory than a `Bool`.
data MultiPeriodTimesQuantifier
  = AllPeriodsHaveTimes
  | SomePeriodsHaveTimes

quantifierSkipGeneratingOffsets :: MultiPeriodTimesQuantifier -> Gen Bool
quantifierSkipGeneratingOffsets AllPeriodsHaveTimes = pure False
quantifierSkipGeneratingOffsets SomePeriodsHaveTimes = chooseBool

-- |Generates an arbitrary input for `cleanup` consisting of multiple,
-- |properly-aligned periods, and the `times` and `newestTimes` inside
-- |the generated periods. `quantifier` controls whether all the periods
-- |have times.
arbitraryMultiPeriodBaseTestData :: MultiPeriodTimesQuantifier -> Gen (BaseTestData [] Float)
arbitraryMultiPeriodBaseTestData quantifier = arbitraryNow $ \now -> do
  numPeriods <- chooseInt (1, 4)
  offsetFrom <- Offset . hours <$> chooseInt (1, 10)
  infos <- unfoldrM nextPeriod (offsetFrom, numPeriods)
  let periods = (\(x, _, _) -> x) <$> infos
      newestOffsets = concatMap (\(_, x, _) -> x) infos
      offsets = concatMap (\(_, _, x) -> x) infos

  let times = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate . unOffset <$> offsets
      newestTimes = flip addLocalTime now . secondsToNominalDiffTime . intToSeconds . negate . unNewestOffset <$> newestOffsets
      periodOffsets = NE.fromList $ fst <$> periods
      initialOffsetFrom = PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds . unOffset $ offsetFrom
      periodInfo = (Offsets . (initialOffsetFrom NE.<|) . join . fmap unOffsets $ periodOffsets, runIdentity . snd <$> periods)

  pure (periodInfo, Times . Sorted $ sort times, NewestTimes . Sorted $ sort newestTimes)

  where
    nextPeriod :: (Offset, Int) -> Gen (Maybe ((PeriodInfo Identity Float, [NewestOffset], [Offset]), (Offset, Int)))
    nextPeriod (offsetFrom, numPeriods) =
      if numPeriods == 0
      then pure Nothing
      else do
        (offsetTo, info) <- generatePeriod offsetFrom
        let nextOffsetFrom = offsetTo
        pure . Just $ (info, (nextOffsetFrom, numPeriods - 1))

    generatePeriod :: Offset -> Gen (Offset, (PeriodInfo Identity Float, [NewestOffset], [Offset]))
    generatePeriod oFrom@(Offset offsetFrom) = do
      numSubperiods <- choose_
      let offsetTo = ceiling_ $ fromIntegral offsetFrom * (numSubperiods + 1)

      skipGeneratingOffsets <- quantifierSkipGeneratingOffsets quantifier

      (newestOffsets, offsets) <- if skipGeneratingOffsets
        then pure ([], [])
        else do
          generatedOffsets <- traverse (arbitraryOffsets oFrom) . adjacentPairs $ zeroList numSubperiods
          pure . sequence $ (\(newestTime, times) -> ([newestTime], times)) <$> generatedOffsets

      pure
        ( Offset offsetTo
        , ( ( Offsets $ PrettyTimeInterval . secondsToNominalDiffTime . intToSeconds <$> NE.fromList [offsetTo]
            , Identity numSubperiods
          )
          , newestOffsets
          , concat offsets
          )
        )


chooseBool :: Gen Bool
chooseBool = chooseEnum (minBound, maxBound)

intToSeconds :: Int -> Pico
intToSeconds = MkFixed . (* (resolution (0 :: Pico))) . fromIntegral

--instance Arbitrary PrettyTimeInterval where
  --arbitrary = PrettyTimeInterval <$> arbitrary

instance Arbitrary Offsets where
  arbitrary = do
    offsets <- arbitrary
    pure . Offsets . NE.fromList . sort . fmap PrettyTimeInterval . getNonEmpty $ offsets

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
