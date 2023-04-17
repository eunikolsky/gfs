module GFS.Internal.TimeIntervalSpec where

import GFS.Internal.TimeInterval

import GFS.Internal.ALocalTime

import Control.Monad
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
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
          -- bounds are used here in order to avoid overflows of `Hours` and
          -- `Months`; big values aren't practical anyway
          int0 <- chooseBoundedIntegral (0, 10000)
          offset <- chooseBoundedIntegral (1, 10000)
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

    it "shows days over a week" $
      forM_ [8, 9, 13, 15] $ \d ->
        show (mkTimeIntervalDays d) `shouldBe` (show d <> "d")

    it "preferes integral weeks over days" $
      forM_ [1, 2, 5, 10] $ \w ->
        show (mkTimeIntervalHours $ w * 7 * 24) `shouldBe` (show w <> "w")

chooseIncreasingIntervals :: Gen (TimeInterval, TimeInterval)
chooseIncreasingIntervals = do
  months <- chooseBoundedIntegral (0, 24)
  hours <- chooseBoundedIntegral (0, 72)
  (mOffset, hOffset) <- (,) <$> chooseBoundedIntegral (1, 24) <*> chooseBoundedIntegral (1, 72)
  pure (mkTimeInterval months hours
      , mkTimeInterval (months + mOffset) (hours + hOffset))
