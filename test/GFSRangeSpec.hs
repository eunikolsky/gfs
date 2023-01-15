module GFSRangeSpec where

import Checkpoints
import GFSRange
import TimeInterval

import ALocalTime

import Control.Exception (assert)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck hiding (scale)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set

spec :: Spec
spec = do
  describe "applyRange" $ do
    context "produces correct times (examples)" $ do
      it "basic example" $
        let now = read "2023-08-20 23:30:00" :: LocalTime
            startTime = read "2023-08-10 20:45:00"
            range = GFSRange
              { rStep = mkTimeInterval 1 2
              , rLimit = mkTimeInterval 3 10
              }
            endTime = read "2023-05-20 13:30:00"
            times = fmap read ["2023-07-10 18:45:00", "2023-06-10 16:45:00"]
            expected = mkCheckpoints endTime times
            actual = applyRange range now startTime
        in actual `shouldBe` expected

      it "the last day of a month going through February" $
        let now = read "2023-03-31 12:00:00" :: LocalTime
            startTime = now
            range = GFSRange
              { rStep = mkTimeInterval 1 0
              , rLimit = mkTimeInterval 3 0
              }
            endTime = read "2022-12-31 12:00:00"
            times = fmap read ["2023-02-28 12:00:00", "2023-01-31 12:00:00"]
            expected = mkCheckpoints endTime times
            actual = applyRange range now startTime
        in actual `shouldBe` expected

    it "doesn't include start time" $
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, _) <- chooseStartTime range now
        pure . counterexample (mconcat
          [ "now: ", show now
          , "\nstart time: ", show startTime
          ]) $ startTime `notElem` unCheckpoints (applyRange range now startTime)

    it "includes end time from now" $
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        let actual = applyRange range now startTime
        pure . counterexample (mconcat
          [ "endTime: " <> show endTime
          , "\nactual: " <> show actual
          ]) $ endTime `elem` NE.toList (unCheckpoints actual)

    it "returns the correct number of times" $
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        let actual = applyRange range now startTime
            (expectedCount, expectedTimes) = countExpectedTimes (endTime, startTime) (rStep range)
            actualCount = NE.length (unCheckpoints actual)
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
        (startTime, _) <- chooseStartTime range now
        let actual = applyRange range now startTime
            actualTimes = unCheckpoints actual
        pure $ actualTimes == NE.sort actualTimes

    it "produces all times between end time and start time" $ do
      property $ \(AGFSRange range) (ALocalTime now) -> do
        (startTime, endTime) <- chooseStartTime range now
        let actual = applyRange range now startTime
            acceptedRange = (endTime, startTime)
        pure . counterexample (mconcat
          [ "start time: ", show startTime
          , "\naccepted range: ", show acceptedRange
          , "\nactual: ", show actual
          ]) $ all (`between` acceptedRange) (NE.toList . unCheckpoints $ actual)

  describe "applyRanges" $ do
    it "includes now" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        now `elem` unCheckpoints (applyRanges ranges now)

    it "includes end time of the last range from now" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let endTime = getEndTime (NE.last $ unGFSRanges ranges) now
        in endTime `elem` NE.toList (unCheckpoints $ applyRanges ranges now)

    it "returns sorted times" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let actualTimes = unCheckpoints $ applyRanges ranges now
        in actualTimes == NE.sort actualTimes

    it "returns unique times" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let actualTimes = unCheckpoints $ applyRanges ranges now
        in actualTimes == NE.nub actualTimes

    it "produces all times between end time and now" $
      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let endTime = getEndTime (NE.last $ unGFSRanges ranges) now
            actual = applyRanges ranges now
            acceptedRange = (endTime, now)
            failing = filter (not . flip between acceptedRange) (NE.toList . unCheckpoints $ actual)
        in counterexample (mconcat
          [ "accepted range: ", show acceptedRange
          , "\nactual: ", show actual
          , "\n failing: ", show failing
          ]) $ null failing

    it "contains times from each range in order" $ do
      let timesFromRanges :: Maybe (NonEmpty GFSRange) -> LocalTime -> LocalTime -> [LocalTime]
          timesFromRanges Nothing _ _ = []
          timesFromRanges (Just (range :| ranges)) now startTime =
            let endTime = getEndTime range now
                times = unCheckpoints $ applyRange range now startTime
                newStartTime = NE.head times
            in endTime : NE.toList times ++ timesFromRanges (NE.nonEmpty ranges) now newStartTime

      property $ \(AGFSRanges ranges) (ALocalTime now) ->
        let expected = Set.fromList $ timesFromRanges (Just $ unGFSRanges ranges) now now
            actual = Set.fromList . NE.toList . unCheckpoints $ applyRanges ranges now
        in counterexample (mconcat
          [ "expected: ", show expected
          , "\nactual: ", show actual
          ]) $ expected `Set.isSubsetOf` actual

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
    months <- chooseInt (0, 10)
    let dayInHours = 24
    hours <- chooseInt (0, dayInHours)
    let hours' = if months == 0 && hours == 0 then 1 else hours
    let step = mkTimeInterval months hours'
    -- note: so that the max hours is one week; otherwise it's possible that
    -- an interval with a few months and a lot of hours is actually larger than
    -- an interval with many months and a few hours
    scale <- chooseInt (2, 7)
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
