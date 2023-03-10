module GFS.Internal.TimeListSpec where

import GFS.Internal.ALocalTime
import GFS.Internal.TimeList

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "keepNewestTime" $ do
    it "returns nothing for empty times" $
      let noTimes = mkTimeList []
      in keepNewestTime noTimes == noTimes

    it "removes the newest time" $
      property . forAll chooseTimeList $ \(times, withoutNewest) ->
        let actual = keepNewestTime times
        in counterexample ("actual: " <> show actual) $ actual == withoutNewest

chooseTimeList :: Gen (TimeList, TimeList)
chooseTimeList = do
  times <- fmap (fmap (TimeItem "") . sort) . listOf1 $ unALocalTime <$> arbitrary
  let withoutNewest = reverse . tail . reverse $ times
  pure (mkTimeList times, mkTimeList withoutNewest)
