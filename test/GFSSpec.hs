{-# LANGUAGE TypeApplications #-}

module GFSSpec where

import ArbitraryLocalTime
import GFS
import RangedInput

import Control.Monad.Writer.Strict (runWriter)
import Data.Functor.Identity (Identity(..))
import Data.List
import qualified Data.List.NonEmpty as NE
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

    it "cleans up items outside of the specified range" $ do
      property $ forAll arbitraryInputOutsideOfRange $ \(now, ((range, _), times, _)) ->
        let input = getSorted . unTimes $ times
            cleanedUp = cleanup range input now

            actual = sort cleanedUp
            expected = input
            description = concat ["Actual cleaned up: ", show actual, "; expected: ", show input]
        in counterexample (intercalate "\n" [describeOffsets range now, description])
          $ actual == expected

    it "leaves no more times than there are subperiods with times" $ do
      property $ forAll arbitraryInputWithinRange $ \(now, ((range, Identity numSubperiods), times, _)) ->
        let input = getSorted . unTimes $ times
            rest = input \\ cleanup range input now

            numberOfItems = concat ["Actual left items: ", show (length rest), "; expected: ", show numSubperiods]
        in counterexample (intercalate "\n" [describeOffsets range now, numberOfItems])
          -- the reason for `<=` instead of `==` is there may not be a single time
          -- within every subperiod
          $ length rest <= ceiling_ numSubperiods

    it "leaves only the newest time in every subperiod" $ do
      property $ forAll (arbitraryInputWithinRangeSubperiods @Float) $ \(now, ((range, _), times, newestTimes)) ->
        let input = getSorted . unTimes $ times
            rest = input \\ cleanup range input now

            description = concat ["Actual left: ", show rest, "; expected: ", show . getSorted . unNewestTimes $ newestTimes]
        in counterexample description $ rest == getSorted (unNewestTimes newestTimes)

    it "removes no more than two times when `now` shifts forward by `offsetFrom`" $ do
      -- note: the max allowed removed times after the shift in this property
      -- is two because the number of subperiods is a floating-point number;
      -- if that number is an integer (i.e., all subperiods are of the same
      -- duration), then the max allowed removed times is only one!
      property $ forAll (arbitraryInputWithinRangeSubperiods @Float) $ \(now, ((range, _), times, newestTimes)) ->
        let input = getSorted . unTimes $ times
            rest = input \\ cleanup range input now

            shiftedNow = unPrettyTimeInterval (NE.head $ unOffsets range) `addLocalTime` now
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
      property $ forAll (arbitraryInputWithinRangeSubperiods @Int) $ \(now, ((range, _), times, newestTimes)) ->
        let input = getSorted . unTimes $ times
            rest = input \\ cleanup range input now

            shiftedNow = unPrettyTimeInterval (NE.head $ unOffsets range) `addLocalTime` now
            shiftedRest = rest \\ cleanup range rest shiftedNow

            numExtraRemovedTimes = length rest - length shiftedRest
            description = concat
              [ "First rest (", show (length rest)
              , "): ", show rest
              , "; second rest (", show (length shiftedRest)
              , "): ", show shiftedRest
              ]
        in counterexample description $ numExtraRemovedTimes <= 1

    it "leaves only the newest time in every subperiod of every period" $ do
      prop_leavesOnlyNewestTimes AllPeriodsHaveTimes

    it "correctly skips periods without any times" $ do
      prop_leavesOnlyNewestTimes SomePeriodsHaveTimes

    it "leaves only the newest time for a period starting at now" $ do
      property $ forAll arbitraryInputWithinRangeFromNow $ \(now, ((range, _), times, newestTimes)) ->
        let input = getSorted . unTimes $ times
            rest = input \\ cleanup range input now

            description = concat ["Actual left: ", show rest, "; expected: ", show . getSorted . unNewestTimes $ newestTimes]
        in counterexample description $ rest == getSorted (unNewestTimes newestTimes)

prop_leavesOnlyNewestTimes :: MultiPeriodTimesQuantifier -> Property
prop_leavesOnlyNewestTimes quantifier =
  property $ forAll (arbitraryMultiPeriodBaseTestData quantifier) $ \(now, ((ranges, _), times, newestTimes)) ->
    let input = getSorted . unTimes $ times
        (cleaned, log) = runWriter $ cleanup_ ranges input now
        rest = input \\ cleaned

        description = intercalate "\n" $ concat ["Actual left: ", show rest, "; expected: ", show . getSorted . unNewestTimes $ newestTimes] : log
    in counterexample description $ rest == getSorted (unNewestTimes newestTimes)

-- |Describes @offsets@ as times relative to @now@.
describeOffsets :: Offsets -> LocalTime -> String
describeOffsets (Offsets offsets) now
  = "Offsets: " ++ (intercalate "," . NE.toList $ show . flip addLocalTime now . negate . unPrettyTimeInterval <$> NE.reverse offsets)
