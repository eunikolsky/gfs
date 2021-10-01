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
            restAgain = rest \\ cleanup period rest now
        in counterexample (concat ["rest: ", show rest, "; restAgain: ", show restAgain])
          $ rest == restAgain

    it "does not create cleanup times" $ do
      property $ \period times now ->
        let input = sort . getNonEmpty $ times
            cleanedUp = cleanup period input now
        in null $ cleanedUp \\ input

    -- TODO does this property makes sense now?
    --it "cleans up such that there are no remaining elements closer than the period" $ do

    it "cleans up items outside of the specified range (with exceptions)" $ do
      property $ forAll arbitraryInputOutsideOfRange $ \(now, newest, range, times) ->
        let inputTimes = getSorted times
            -- we always have to separately add a newest time that is never removed
            input = inputTimes ++ [newest]
            cleanedUp = cleanup range input now

            actual = sort cleanedUp
            expected = inputTimes
            description = concat ["Actual cleaned up: ", show actual, "; expected: ", show inputTimes]
        in counterexample (intercalate "\n" [describePeriod range now, description])
          $ actual == expected

    it "leaves as many times as there are subperiods with times" $ do
      property $ forAll arbitraryInputWithinRange $ \(now, newest, range, numSubperiods, times) ->
        let inputTimes = getSorted times
            -- we always have to separately add a newest time that is never removed
            input = inputTimes ++ [newest]
            rest = input \\ (cleanup range input now ++ [newest])

            numberOfItems = concat ["Actual left items: ", show (length rest), "; expected: ", show numSubperiods]
        in counterexample (intercalate "\n" [describePeriod range now, numberOfItems])
          $ length rest == numSubperiods

-- |Describes @period@ as times relative to @now@.
describePeriod :: Period -> LocalTime -> String
describePeriod (PrettyTimeInterval offsetFrom, PrettyTimeInterval offsetTo) now
  = concat ["Period: from ", from, " to ", to]
  where
    from = show $ addLocalTime (-offsetTo) now
    to = show $ addLocalTime (-offsetFrom) now
