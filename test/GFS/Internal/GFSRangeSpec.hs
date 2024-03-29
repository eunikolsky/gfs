module GFS.Internal.GFSRangeSpec where

import GFS.Internal.Checkpoints
import GFS.Internal.GFSRange
import GFS.Internal.TimeInterval

import GFS.Internal.ALocalTime

import Control.Exception (assert)
import Data.List (sort)
import Data.Maybe
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck hiding (scale)
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
  let hour = mkTimeIntervalHours 1
      day = mkTimeIntervalDays 1
      month = mkTimeIntervalMonths 1
      year = mkTimeIntervalYears 1
      hourly = GFSRange hour day
      daily = GFSRange day month
      monthly = GFSRange month year
      sampleRanges = mkGFSRanges hourly [daily, monthly]

  describe "applyRange" $ do
    context "produces correct times (examples)" $ do
      it "basic example" $
        let now = read "2023-08-20 23:30:00" :: LocalTime
            startTime = read "2023-08-10 20:45:00"
            range = GFSRange
              { rStep = mkTimeIntervalMonths 1
              , rLimit = mkTimeIntervalMonths 3
              }
            endTime = read "2022-01-01 00:00:00"
            expected = fmap read
              [ "2023-05-20 23:30:00"
              , "2023-06-10 20:45:00"
              , "2023-07-10 20:45:00"
              ]
            actual = applyRange endTime range now startTime
        in actual `shouldBe` expected

      it "the last day of a month going through February" $
        let now = read "2023-03-31 12:00:00" :: LocalTime
            startTime = now
            range = GFSRange
              { rStep = mkTimeIntervalMonths 1
              , rLimit = mkTimeIntervalMonths 3
              }
            endTime = read "2022-01-01 00:00:00"
            expected = fmap read
              [ "2022-12-31 12:00:00"
              , "2023-01-31 12:00:00"
              , "2023-02-28 12:00:00"
              ]
            actual = applyRange endTime range now startTime
        in actual `shouldBe` expected

    it "doesn't include start time" $
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        pure . counterexample (mconcat
          [ "now: ", show now
          , "\nstart time: ", show startTime
          ]) $ startTime `notElem` applyRange endTime range now startTime

    it "includes end time from now" $
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        let actual = applyRange endTime range now startTime
        pure . counterexample (mconcat
          [ "endTime: " <> show endTime
          , "\nactual: " <> show actual
          ]) $ endTime `elem` actual

    it "returns the correct number of times" $
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        let actual = applyRange endTime range now startTime
            (expectedCount, expectedTimes) = countExpectedTimes (endTime, startTime) (rStep range)
            actualCount = length actual
        pure . counterexample (mconcat
          [ "now: ", show now
          , "\nstart time: ", show startTime
          , "\n(end time: ", show endTime, ")"
          , "\n(expected times: ", show expectedTimes, ")"
          , "\nexpected count: ", show expectedCount
          , "\nactual: ", show actual
          , "\nactual count: ", show actualCount
          ]) $ actualCount == expectedCount

    it "returns sorted times" $
      -- strictly speaking, this currently tests that `Checkpoints` (or
      -- `unCheckpoints`) produces a sorted list; however, since `Checkpoints`
      -- is a part of the `applyRange` functionality, it doesn't matter where
      -- sorting happens (except for which tests fail if sorting is incorrect)
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        let actual = applyRange endTime range now startTime
        pure $ actual == sort actual

    it "produces all times between end time and start time" $
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        let actual = applyRange endTime range now startTime
            acceptedRange = (endTime, startTime)
        pure . counterexample (mconcat
          [ "start time: ", show startTime
          , "\naccepted range: ", show acceptedRange
          , "\nactual: ", show actual
          ]) $ all (`between` acceptedRange) actual

    it "produces nothing when end time is bigger than start time" $ do
      let chooseStartTimeBeforeEndTime :: GFSRange -> LocalTime -> Gen (LocalTime, LocalTime)
          chooseStartTimeBeforeEndTime range now = do
            let endTime = getEndTime range now
            offset <- realToFrac . getPositive <$> (arbitrary :: Gen (Positive Integer))
            pure (addLocalTime (negate offset) endTime, endTime)

      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTimeBeforeEndTime range now
        let actual = applyRange endTime range now startTime
        -- TODO deduplicate this `counterexample $ mconcat …` pattern
        pure . counterexample (mconcat
          [ "end time: ", show endTime
          , "\nactual: ", show actual
          ]) $ actual == []

  describe "applyRanges" $ do
    context "produces correct times (examples)" $ do
      it "basic example" $
        let now = read "2023-11-19 22:00:00"
            expected = mkCheckpoints
              now
              $ fmap read
              -- now's midnight
              [ "2023-11-19 00:00:00"
              -- hourly
              , "2023-11-18 23:00:00"
              , "2023-11-18 22:00:00"
              , "2023-11-18 21:00:00"
              , "2023-11-18 20:00:00"
              , "2023-11-18 19:00:00"
              , "2023-11-18 18:00:00"
              , "2023-11-18 17:00:00"
              , "2023-11-18 16:00:00"
              , "2023-11-18 15:00:00"
              , "2023-11-18 14:00:00"
              , "2023-11-18 13:00:00"
              , "2023-11-18 12:00:00"
              , "2023-11-18 11:00:00"
              , "2023-11-18 10:00:00"
              , "2023-11-18 09:00:00"
              , "2023-11-18 08:00:00"
              , "2023-11-18 07:00:00"
              , "2023-11-18 06:00:00"
              , "2023-11-18 05:00:00"
              , "2023-11-18 04:00:00"
              , "2023-11-18 03:00:00"
              , "2023-11-18 02:00:00"
              , "2023-11-18 01:00:00"
              , "2023-11-18 00:00:00"
              -- daily
              , "2023-11-17 00:00:00"
              , "2023-11-16 00:00:00"
              , "2023-11-15 00:00:00"
              , "2023-11-14 00:00:00"
              , "2023-11-13 00:00:00"
              , "2023-11-12 00:00:00"
              , "2023-11-11 00:00:00"
              , "2023-11-10 00:00:00"
              , "2023-11-09 00:00:00"
              , "2023-11-08 00:00:00"
              , "2023-11-07 00:00:00"
              , "2023-11-06 00:00:00"
              , "2023-11-05 00:00:00"
              , "2023-11-04 00:00:00"
              , "2023-11-03 00:00:00"
              , "2023-11-02 00:00:00"
              , "2023-11-01 00:00:00"
              , "2023-10-31 00:00:00"
              , "2023-10-30 00:00:00"
              , "2023-10-29 00:00:00"
              , "2023-10-28 00:00:00"
              , "2023-10-27 00:00:00"
              , "2023-10-26 00:00:00"
              , "2023-10-25 00:00:00"
              , "2023-10-24 00:00:00"
              , "2023-10-23 00:00:00"
              , "2023-10-22 00:00:00"
              , "2023-10-21 00:00:00"
              , "2023-10-20 00:00:00"
              , "2023-10-19 00:00:00"
              -- monthly
              , "2023-09-19 00:00:00"
              , "2023-08-19 00:00:00"
              , "2023-07-19 00:00:00"
              , "2023-06-19 00:00:00"
              , "2023-05-19 00:00:00"
              , "2023-04-19 00:00:00"
              , "2023-03-19 00:00:00"
              , "2023-02-19 00:00:00"
              , "2023-01-19 00:00:00"
              , "2022-12-19 00:00:00"
              , "2022-11-19 00:00:00"
              ]
        in applyRanges sampleRanges now `shouldBe` expected

    it "includes now" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        now `elem` unCheckpoints (applyRanges ranges now)

    it "includes now's midnight" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        now{localTimeOfDay = midnight} `elem` unCheckpoints (applyRanges ranges now)

    it "includes end time of the last range from now's midnight" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let endTime = getEndTime (NE.last $ unGFSRanges ranges) now{localTimeOfDay = midnight}
            actual = NE.toList (unCheckpoints $ applyRanges ranges now)
        in counterexample (mconcat
          [ "end time: ", show endTime
          , "\nactual: ", show actual
          ]) $ endTime `elem` actual

    it "returns sorted times" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let actualTimes = unCheckpoints $ applyRanges ranges now
        in actualTimes == NE.sort actualTimes

    it "returns unique times" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let actualTimes = unCheckpoints $ applyRanges ranges now
        in actualTimes == NE.nub actualTimes

    it "produces all times between end time and now's midnight" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let endTime = getEndTime (NE.last $ unGFSRanges ranges) now{localTimeOfDay = midnight}
            actual = applyRanges ranges now
            acceptedRange = (endTime, now)
            failing = filter (not . flip between acceptedRange) (NE.toList . unCheckpoints $ actual)
        in counterexample (mconcat
          [ "accepted range: ", show acceptedRange
          , "\nactual: ", show actual
          , "\n failing: ", show failing
          ]) $ null failing

  describe "Show GFSRange instance (examples)" $ do
    it "shows step and limits separated by colon" $
      let step = mkTimeIntervalDays 1
          limit = mkTimeIntervalYears 1
          range = GFSRange step limit
      in show range `shouldBe` "1d:1y"

  describe "Show GFSRanges instance (examples)" $ do
    it "shows ranges separated by comma" $
      show sampleRanges `shouldBe` "1h:1d,1d:1m,1m:1y"

-- TODO this function repeats the production code; avoid this somehow?
getEndTime :: GFSRange -> LocalTime -> LocalTime
getEndTime (GFSRange _ limit) = subTimeInterval limit

between :: (Ord a, HasCallStack) => a -> (a, a) -> Bool
between x (lowest, highest) = assert (highest > lowest) $ x >= lowest && x <= highest

countExpectedTimes :: (LocalTime, LocalTime) -> TimeInterval -> (Int, [LocalTime])
countExpectedTimes (from, to) step =
  let times = from : (safeTail . takeWhile (> from) $ (`subTimeInterval` to) . (`scaleTimeInterval` step) <$> [0..])
      -- TODO in theory, counting times backwards should be the same as counting forward,
      -- but the test sometimes fails with this line (seed 284373677); there is an extra
      -- time present in the test data, which is exactly 2 days away from start time; the
      -- test doesn't fail when the number of hours is less than about 20; the reason might
      -- be that adding many hours changes days, in turn changing months, and then the
      -- calendar diffs get messed up
      -- times = to : (safeTail . takeWhile (< to) $ (`addTimeInterval` from) . (`scaleTimeInterval` step) <$> [0..])
  in (length times, times)

safeTail :: [a] -> [a]
safeTail = fromMaybe [] . fmap NE.tail . NE.nonEmpty

chooseTimeBetween :: (LocalTime, LocalTime) -> Gen LocalTime
chooseTimeBetween (from, to) = do
  let diff = to `diffLocalTime` from
  fromOffset <- fromInteger <$> chooseInteger (0, floor diff - 1)
  pure $ addLocalTime fromOffset from

chooseStartTime :: GFSRange -> LocalTime -> Gen (LocalTime, LocalTime)
chooseStartTime range now = do
  let endTime = getEndTime range now
  startTime <- chooseTimeBetween (endTime, now)
  pure (startTime, endTime)

newtype AGFSRange = AGFSRange { unAGFSRange :: GFSRange }
  deriving Show

instance Arbitrary AGFSRange where
  arbitrary = do
    useHours <- chooseAny
    step <- if useHours
      then
        let dayInHours = 24
        in mkTimeIntervalHours <$> chooseBoundedIntegral (1, dayInHours)
      else
        mkTimeIntervalMonths <$> chooseBoundedIntegral (1, 10)

    -- note: so that the max hours is one week; otherwise it's possible that
    -- an interval with a few months and a lot of hours is actually larger than
    -- an interval with many months and a few hours
    scale <- chooseBoundedIntegral (2, 7)
    let limit = scaleTimeInterval scale step
    pure . AGFSRange $ GFSRange step limit

newtype AGFSRanges = AGFSRanges GFSRanges
  deriving Show

instance Arbitrary AGFSRanges where
  arbitrary = do
    let arbitraryRange = unAGFSRange <$> arbitrary
    one <- arbitraryRange
    ranges <- listOf1 arbitraryRange
    pure . AGFSRanges $ mkGFSRanges one ranges
