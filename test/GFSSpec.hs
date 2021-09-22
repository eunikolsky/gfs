module GFSSpec where

import ArbitraryLocalTime
import GFS

import Data.List
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Word
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "cleanup" $ do
    it "returns nothing for empty input" $
      property $ \period now ->
        cleanup period [] now == []

    it "never cleans up the newest time" $ do
      property $ \period times now ->
        let input = sort . getNonEmpty $ times
            -- this is safe because @times@ is a @NonEmptyList LocalTime@
            newest = last input
            cleanedUp = cleanup period input now
        in newest `notElem` cleanedUp

    it "never cleans up times in the future" $ do
      property $ \period times now ->
        let input = sort . getNonEmpty $ times
            cleanedUp = cleanup period input now
            isFutureDate = (> 0) . (`diffLocalTime` now)
        in all (not . isFutureDate) cleanedUp

    it "is idempotent (cleaning up twice with the same period is the same as cleaning up once)" $ do
      -- TODO apply multiple times
      property $ \period times now ->
        let input = sort . getNonEmpty $ times
            cleanedUp = cleanup period input now
            rest = input \\ cleanedUp
            cleanedUpTwice = cleanup period rest now
        in cleanedUp == cleanedUpTwice

    it "does not create cleanup times" $ do
      property $ \period times now ->
        let input = sort . getNonEmpty $ times
            cleanedUp = cleanup period input now
        in null $ cleanedUp \\ input

    it "cleans up such that there are no remaining elements closer than the period" $ do
      property $ \period times now ->
        let input = sort . getNonEmpty $ times
            remaining = input \\ cleanup period input now
            remainingPairs = zip remaining (tail remaining)
        in all (\(time, nextTime) -> diffLocalTime nextTime time > period) remainingPairs

data RangeInput = RangeInput
  { rangeMaxTime :: LocalTime
  , rangeTimes :: [LocalTime]
  }
  deriving Show

-- |Data type defining all the inputs for the @cleanup@ function. The reason for
-- |the separate type is we need to generate an initial offset and a number of
-- |ranges, then generate arbitrary times inside every range, and I'm not sure
-- |it's possible to do that with @property@. Plus it seems to be a good idea
-- |anyway.
data RangedInput = RangedInput
  { riNow :: LocalTime
  , riPeriod :: Period
  --, riNumRanges :: Positive Word8
  , riRanges :: NonEmptyList RangeInput
  }
  deriving Show

instance Arbitrary RangedInput where
  arbitrary = do
    let numRanges = 6
        range = RangeInput (LocalTime (ModifiedJulianDay 0) midnight) []
    return $ RangedInput
      (LocalTime (ModifiedJulianDay 0) midnight)
      (nominalDay, (numRanges + 1) * nominalDay)
      (NonEmpty [range])
