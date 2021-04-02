module GFSSpec where

import ArbitraryLocalTime
import GFS

import Data.List
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "cleanup" $ do
    it "returns nothing for empty input" $
      property $ \period ->
        cleanup period [] == []

    it "never cleans up the newest time" $ do
      property $ \period times ->
        let input = sort . getNonEmpty $ times
            -- this is safe because @times@ is a @NonEmptyList LocalTime@
            newest = last input
            cleanedUp = cleanup period input
        in newest `notElem` cleanedUp

    it "is idempotent (cleaning up twice with the same period is the same as cleaning up once)" $ do
      -- TODO apply multiple times
      property $ \period times ->
        let input = sort . getNonEmpty $ times
            cleanedUp = cleanup period input
            rest = input \\ cleanedUp
            cleanedUpTwice = cleanup period rest
        in cleanedUp == cleanedUpTwice

    it "does not create cleanup times" $ do
      property $ \period times ->
        let input = sort . getNonEmpty $ times
            cleanedUp = cleanup period input
        in null $ cleanedUp \\ input

    it "cleans up such that there are no remaining elements closer than the period" $ do
      property $ \period times ->
        let input = sort . getNonEmpty $ times
            remaining = input \\ cleanup period input
            remainingPairs = zip remaining (tail remaining)
        in all (\(time, nextTime) -> diffLocalTime nextTime time > period) remainingPairs
