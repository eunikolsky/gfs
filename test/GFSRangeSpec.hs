module GFSRangeSpec where

import Checkpoints
import GFSRange

import ALocalTime

import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck hiding (scale)
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
  describe "applyRange" $ do
    it "doesn't include start time" $
      property $ \(AGFSRange range) (ALocalTime now) (ALocalTime time) ->
        time `notElem` unCheckpoints (applyRange range now time)

    it "includes end time from now at the end" $
      property $ \(AGFSRange range) (ALocalTime now) (ALocalTime time) ->
        let endTime = getEndTime range now
            actual = applyRange range now time
        in counterexample (mconcat
          [ "endTime: " <> show endTime
          , "\nactual: " <> show actual
          ]) $ NE.last (unCheckpoints actual) == endTime

-- TODO this function repeats the production code; avoid this somehow?
getEndTime :: GFSRange -> LocalTime -> LocalTime
getEndTime (GFSRange _ (CalendarDiffTime limitMonths limitTime)) t =
  let time = addLocalTime (negate limitTime) t
      negativeMonths = negate limitMonths
  in time { localDay = addGregorianMonthsClip negativeMonths (localDay time) }

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
