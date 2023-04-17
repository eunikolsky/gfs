module GFS.Internal.TimeIntervalSpec where

import GFS.Internal.TimeInterval

import GFS.Internal.ALocalTime

import Control.Monad
import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "addTimeInterval" $ do
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

    it "adding bigger interval produces bigger result" $
      property $ \(ALocalTime t) ->
        forAll chooseIncreasingIntervals $ \(ti0, ti1) ->
          addTimeInterval ti1 t > addTimeInterval ti0 t

    it "uses end days if possible (example)" $
      let t0430 = read "2023-04-30 00:00:00"
          t0530 = read "2023-05-30 00:00:00"
          ti1month25hours = mkTimeInterval 1 25
      in do
        -- when adding hours first: "2023-06-01 01:00:00"
        addTimeInterval ti1month25hours t0430 `shouldBe` read "2023-05-31 01:00:00"
        -- when adding hours first: "2023-06-30 01:00:00"
        addTimeInterval ti1month25hours t0530 `shouldBe` read "2023-07-01 01:00:00"

  describe "subTimeInterval" $ do
    it "subtracting bigger interval produces smaller result" $
      property $ \(ALocalTime t) ->
        forAll chooseIncreasingIntervals $ \(ti0, ti1) ->
          subTimeInterval ti1 t < subTimeInterval ti0 t

    it "uses end days if possible (example)" $
      let t0430 = read "2023-04-30 00:00:00"
          t0531 = read "2023-05-31 00:00:00"
          ti1month25hours = mkTimeInterval 1 25
      in do
        -- when subtracting months first: "2023-03-28 23:00:00"
        subTimeInterval ti1month25hours t0430 `shouldBe` read "2023-03-28 23:00:00"
        -- when subtracting months first: "2023-04-28 23:00:00"
        subTimeInterval ti1month25hours t0531 `shouldBe` read "2023-04-29 23:00:00"

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

  describe "Show instance (examples)" $ do
    let mkTimeIntervalHours = mkTimeInterval 0
        mkTimeIntervalDays = mkTimeIntervalHours . (* 24)
        mkTimeIntervalWeeks = mkTimeIntervalDays . (* 7)

    it "shows hours under a day" $
      -- this looping is done inside the test so that it doesn't produce a lot
      -- of test case output
      forM_ [1..23] $ \h ->
        show (mkTimeIntervalHours h) `shouldBe` (show h <> "h")

    it "shows days under a week" $
      forM_ [1..6] $ \d ->
        show (mkTimeIntervalDays d) `shouldBe` (show d <> "d")

    it "shows hours over a day" $
      forM_ [25, 26, 47, 49] $ \h ->
        show (mkTimeIntervalHours h) `shouldBe` (show h <> "h")

    it "preferes integral days over hours" $
      forM_ [1, 2, 5, 10] $ \d ->
        show (mkTimeIntervalHours $ d * 24) `shouldBe` (show d <> "d")

    it "shows weeks" $
      forM_ [1..6] $ \w ->
        show (mkTimeIntervalWeeks w) `shouldBe` (show w <> "w")

chooseIncreasingIntervals :: Gen (TimeInterval, TimeInterval)
chooseIncreasingIntervals = do
  months <- chooseInt (0, 24)
  hours <- chooseInt (0, 72)
  (mOffset, hOffset) <- (,) <$> chooseInt (1, 24) <*> chooseInt (1, 72)
  pure (mkTimeInterval months hours
      , mkTimeInterval (months + mOffset) (hours + hOffset))

newtype ATimeInterval = ATimeInterval TimeInterval
  deriving Show

instance Arbitrary ATimeInterval where
  arbitrary = do
    months <- getNonNegative <$> arbitrary
    hours <- getNonNegative <$> arbitrary
    pure . ATimeInterval $ mkTimeInterval months hours
