module GFS.Internal.TimeIntervalSpec where

import GFS.Internal.TimeInterval

import GFS.Internal.ALocalTime

import Control.Monad
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "subTimeInterval" $ do
    it "subtracting bigger interval produces smaller result" $
      property $ \(ALocalTime t) ->
        forAll chooseIncreasingIntervals $ \(ti0, ti1) ->
          subTimeInterval ti1 t < subTimeInterval ti0 t

    it "uses end days when subtracting months (example)" $
      let t0430 = read "2023-04-30 00:00:00"
          t0531 = read "2023-05-31 00:00:00"
          tiMonth = mkTimeIntervalMonths 1
      in do
        subTimeInterval tiMonth t0430 `shouldBe` read "2023-03-30 00:00:00"
        subTimeInterval tiMonth t0531 `shouldBe` read "2023-04-30 00:00:00"

    it "uses end days when subtracting months (example)" $
      let t0501 = read "2023-05-01 00:00:00"
          t0601 = read "2023-06-01 00:00:00"
          tiDay = mkTimeIntervalHours 24
      in do
        subTimeInterval tiDay t0501 `shouldBe` read "2023-04-30 00:00:00"
        subTimeInterval tiDay t0601 `shouldBe` read "2023-05-31 00:00:00"

  describe "Ord instance" $ do
    let chooseIncreasingInts = do
          -- bounds are used here in order to avoid overflows of `Hours` and
          -- `Months`; big values aren't practical anyway
          int0 <- chooseBoundedIntegral (0, 10000)
          offset <- chooseBoundedIntegral (1, 10000)
          pure (int0, int0 + offset)

    it "compares months" $
      property . forAll chooseIncreasingInts $ \(month0, month1) ->
        mkTimeIntervalMonths month0 < mkTimeIntervalMonths month1

    it "compares hours" $
      property . forAll chooseIncreasingInts $ \(hour0, hour1) ->
        mkTimeIntervalHours hour0 < mkTimeIntervalHours hour1

    prop "positive hours are always smaller than positive months" $
      \(Positive hours) (Positive months) ->
        mkTimeIntervalHours hours < mkTimeIntervalMonths months

  describe "Show instance (examples)" $ do
    let mkTimeIntervalDays = mkTimeIntervalHours . (* 24)
        mkTimeIntervalWeeks = mkTimeIntervalDays . (* 7)
        mkTimeIntervalYears = mkTimeIntervalMonths . (* 12)

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

    it "shows days over a week" $
      forM_ [8, 9, 13, 15] $ \d ->
        show (mkTimeIntervalDays d) `shouldBe` (show d <> "d")

    it "preferes integral weeks over days" $
      forM_ [1, 2, 5, 10] $ \w ->
        show (mkTimeIntervalHours $ w * 7 * 24) `shouldBe` (show w <> "w")

    it "shows months under a year" $
      forM_ [1..11] $ \m ->
        show (mkTimeIntervalMonths m) `shouldBe` (show m <> "m")

    it "shows years" $
      forM_ [1, 2, 5, 10] $ \y ->
        show (mkTimeIntervalYears y) `shouldBe` (show y <> "y")

    it "shows months over a year" $
      forM_ [13, 14, 23, 25] $ \m ->
        show (mkTimeIntervalMonths m) `shouldBe` (show m <> "m")

    it "preferes integral years over months" $
      forM_ [1, 2, 5, 10] $ \y ->
        show (mkTimeIntervalMonths $ y * 12) `shouldBe` (show y <> "y")

chooseIncreasingIntervals :: Gen (TimeInterval, TimeInterval)
chooseIncreasingIntervals = do
  useHours <- chooseAny
  if useHours
    then do
      hours <- chooseBoundedIntegral (0, 72)
      offset <- chooseBoundedIntegral (1, 72)
      pure (mkTimeIntervalHours hours, mkTimeIntervalHours $ hours + offset)
    else do
      months <- chooseBoundedIntegral (0, 24)
      offset <- chooseBoundedIntegral (1, 24)
      pure (mkTimeIntervalMonths months, mkTimeIntervalMonths $ months + offset)
