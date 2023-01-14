module TimeIntervalSpec where

import TimeInterval

import ALocalTime

import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "addTimeInterval" $
    it "round-trips with subTimeInterval within a few days" $
      {- the round-trip isn't perfect because of the irregular number of days in months:

         ATimeInterval I6M6H
         ALocalTime {unALocalTime = 2023-12-31 03:36:00}
         added time: 2024-06-30 09:36:00
         actual: 2023-12-30 03:36:00
         (time - actual) days: 1

        Randomized with seed 492797633
       -}
      property $ \(ATimeInterval interval) (ALocalTime time) ->
        let added = addTimeInterval interval time
            actual = subTimeInterval interval added
            dayDiff = diffDays (localDay time) (localDay actual)
            -- the max days difference in months: Jan 31 - Feb 28
            maxDayDiff = 31 - 28
        in counterexample (mconcat
          [ "added time: ", show added
          , "\nactual: ", show actual
          , "\n(time - actual) days: ", show dayDiff
          ]) $ (localTimeOfDay actual) == (localTimeOfDay time)
            && dayDiff <= maxDayDiff

  describe "Ord instance" $ do
    let chooseIncreasingInts = do
          int0 <- arbitrary :: Gen Int
          offset <- getPositive <$> arbitrary
          pure (int0, int0 + offset)

    it "compares months first" $
      property $ \hour ->
        forAll chooseIncreasingInts $ \(month0, month1) ->
          mkTimeInterval month0 hour < mkTimeInterval month1 hour

    it "compares hours second" $
      property $ \(Positive month) ->
        forAll chooseIncreasingInts $ \(hour0, hour1) ->
          mkTimeInterval month hour0 < mkTimeInterval month hour1

newtype ATimeInterval = ATimeInterval TimeInterval
  deriving Show

instance Arbitrary ATimeInterval where
  arbitrary = do
    months <- getNonNegative <$> arbitrary
    hours <- getNonNegative <$> arbitrary
    pure . ATimeInterval $ mkTimeInterval months hours
