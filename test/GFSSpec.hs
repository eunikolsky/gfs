module GFSSpec where

import ArbitraryLocalTime
import GFS
import RangedInput

import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck hiding (within)

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

    -- TODO does this property makes sense now?
    --it "cleans up such that there are no remaining elements closer than the period" $ do

    it "cleans up items outside of the specified range" $ do
      property $ \rangedInput ->
        let now = riNow rangedInput
            range = riPeriod rangedInput
            input = getSorted . riTimes $ rangedInput
            cleanedUp = cleanup range input now
        in sort cleanedUp == input
