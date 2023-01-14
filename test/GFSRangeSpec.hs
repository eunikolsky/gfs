module GFSRangeSpec where

import Checkpoints
import GFSRange
import TimeInterval

import ALocalTime

import Data.Maybe
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck hiding (scale)
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
  describe "applyRange" $ do
    it "doesn't include start time" $
      property $ \(AGFSRange range) -> do
        (now, time) <- chooseNowAndStartTime
        pure . counterexample (mconcat
          [ "now: ", show now
          , "\nstart time: ", show time
          ]) $ time `notElem` unCheckpoints (applyRange range now time)

    it "includes end time from now" $
      property $ \(AGFSRange range) -> do
        (now, time) <- chooseNowAndStartTime
        let endTime = getEndTime range now
            actual = applyRange range now time
        pure . counterexample (mconcat
          [ "endTime: " <> show endTime
          , "\nactual: " <> show actual
          ]) $ endTime `elem` NE.toList (unCheckpoints actual)

    it "returns the correct number of times" $
      property $ \(AGFSRange range) -> do
        (now, startTime) <- chooseNowAndStartTime
        let endTime = getEndTime range now
            actual = applyRange range now startTime
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
      property $ \(AGFSRange range) -> do
        (now, startTime) <- chooseNowAndStartTime
        let actual = applyRange range now startTime
            actualTimes = unCheckpoints actual
        pure $ actualTimes == NE.sort actualTimes

-- TODO this function repeats the production code; avoid this somehow?
getEndTime :: GFSRange -> LocalTime -> LocalTime
getEndTime (GFSRange _ limit) = subTimeInterval limit

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

chooseNowAndStartTime :: Gen (LocalTime, LocalTime)
chooseNowAndStartTime = do
  now <- unALocalTime <$> arbitrary
  startTimeOffset <- realToFrac <$> chooseInteger (1, 1_000_000)
  let startTime = addLocalTime (negate startTimeOffset) now
  pure (now, startTime)

newtype AGFSRange = AGFSRange GFSRange
  deriving Show

instance Arbitrary AGFSRange where
  arbitrary = do
    months <- chooseInt (0, 10)
    let weekInHours = 24 * 7
    hours <- chooseInt (0, weekInHours)
    let step = mkTimeInterval months hours
    scale <- chooseInt (2, 5)
    let limit = scaleTimeInterval scale step
    pure . AGFSRange $ GFSRange step limit
