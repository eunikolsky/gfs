module GFSRangeSpec where

import GFSRange

import ALocalTime

import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck hiding (scale)

spec :: Spec
spec = do
  describe "applyRange" $ do
    it "doesn't include start time" $
      property $ \(AGFSRange range) (ALocalTime time) ->
        time `notElem` applyRange range time

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
