{-# LANGUAGE TypeApplications #-}

module GFSSpec where

import ArbitraryLocalTime
import GFS
import RangedInput

import Data.Functor.Identity (Identity(..))
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
      property $ forAll arbitraryInputOutsideOfRange $ \(now, newest, (Identity (range, _), times, _)) ->
        let inputTimes = getSorted times
            -- we always have to separately add a newest time that is never removed
            input = inputTimes ++ [newest]
            cleanedUp = cleanup range input now

            actual = sort cleanedUp
            expected = inputTimes
            description = concat ["Actual cleaned up: ", show actual, "; expected: ", show inputTimes]
        in counterexample (intercalate "\n" [describePeriod range now, description])
          $ actual == expected

    it "leaves no more times than there are subperiods with times" $ do
      property $ forAll arbitraryInputWithinRange $ \(now, newest, (Identity (range, numSubperiods), times, _)) ->
        let inputTimes = getSorted times
            -- we always have to separately add a newest time that is never removed
            input = inputTimes ++ [newest]
            rest = input \\ (cleanup range input now ++ [newest])

            numberOfItems = concat ["Actual left items: ", show (length rest), "; expected: ", show numSubperiods]
        in counterexample (intercalate "\n" [describePeriod range now, numberOfItems])
          -- the reason for `<=` instead of `==` is there may not be a single time
          -- within every subperiod
          $ length rest <= ceiling_ numSubperiods

    it "leaves only the newest time in every subperiod" $ do
      property $ forAll (arbitraryInputWithinRangeSubperiods @Float) $ \(now, newest, (Identity (range, _), times, newestTimes)) ->
        let inputTimes = getSorted times
            input = inputTimes ++ [newest]
            rest = input \\ (cleanup range input now ++ [newest])

            description = concat ["Actual left: ", show rest, "; expected: ", show $ getSorted newestTimes]
        in counterexample description $ rest == getSorted newestTimes

    it "removes no more than two times when `now` shifts forward by `offsetFrom`" $ do
      -- note: the max allowed removed times after the shift in this property
      -- is two because the number of subperiods is a floating-point number;
      -- if that number is an integer (i.e., all subperiods are of the same
      -- duration), then the max allowed removed times is only one!
      property $ forAll (arbitraryInputWithinRangeSubperiods @Float) $ \(now, newest, (Identity (range, _), times, newestTimes)) ->
        let inputTimes = getSorted times
            input = inputTimes ++ [newest]
            rest = input \\ cleanup range input now

            shiftedNow = unPrettyTimeInterval (fst range) `addLocalTime` now
            shiftedRest = rest \\ cleanup range rest shiftedNow

            numExtraRemovedTimes = length rest - length shiftedRest
            description = concat
              [ "First rest (", show (length rest)
              , "): ", show rest
              , "; second rest (", show (length shiftedRest)
              , "): ", show shiftedRest
              ]
        in counterexample description $ numExtraRemovedTimes <= 2

    it "removes no more than one time when `now` shifts forward by `offsetFrom` and all subperiods are equal" $ do
      property $ forAll (arbitraryInputWithinRangeSubperiods @Int) $ \(now, newest, (Identity (range, _), times, newestTimes)) ->
        let inputTimes = getSorted times
            input = inputTimes ++ [newest]
            rest = input \\ cleanup range input now

            shiftedNow = unPrettyTimeInterval (fst range) `addLocalTime` now
            shiftedRest = rest \\ cleanup range rest shiftedNow

            numExtraRemovedTimes = length rest - length shiftedRest
            description = concat
              [ "First rest (", show (length rest)
              , "): ", show rest
              , "; second rest (", show (length shiftedRest)
              , "): ", show shiftedRest
              ]
        in counterexample description $ numExtraRemovedTimes <= 1

-- |Describes @period@ as times relative to @now@.
describePeriod :: Period -> LocalTime -> String
describePeriod (PrettyTimeInterval offsetFrom, PrettyTimeInterval offsetTo) now
  = concat ["Period: from ", from, " to ", to]
  where
    from = show $ addLocalTime (-offsetTo) now
    to = show $ addLocalTime (-offsetFrom) now
