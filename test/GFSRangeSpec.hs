module GFSRangeSpec where

import Checkpoints
import GFSRange

import ALocalTime

import Data.Maybe
import Data.Time.Calendar
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

-- TODO this function repeats the production code; avoid this somehow?
getEndTime :: GFSRange -> LocalTime -> LocalTime
getEndTime (GFSRange _ limit) = addDiffTime (scaleCalendarDiffTime (-1) limit)

addDiffTime :: CalendarDiffTime -> LocalTime -> LocalTime
addDiffTime (CalendarDiffTime diffMonths diffTime) t =
  let time = addLocalTime diffTime t
   in time { localDay = addGregorianMonthsClip diffMonths (localDay time) }

countExpectedTimes :: (LocalTime, LocalTime) -> CalendarDiffTime -> (Int, [LocalTime])
countExpectedTimes (from, to) step =
  let backwardsStep = scaleCalendarDiffTime (-1) step
      times = from : (fromMaybe [] . fmap NE.tail . NE.nonEmpty . takeWhile (> from) $ iterate (addDiffTime backwardsStep) to)
      -- FIXME in theory, counting times backwards should be the same as counting forward,
      -- but the test sometimes fails with this line (seed 692022403):
      -- times = to : (fromMaybe [] . fmap NE.tail . NE.nonEmpty . takeWhile (< to) $ iterate (addDiffTime step) from)
  in (length times, times)

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
    months <- chooseInteger (0, 10)
    let week = 60 * 60 * 24 * 7
    time <- realToFrac <$> chooseInteger (0, week)
    let step = CalendarDiffTime months time
    scale <- chooseInteger (2, 5)
    let limit = scaleCalendarDiffTime scale step
    pure . AGFSRange $ GFSRange step limit
