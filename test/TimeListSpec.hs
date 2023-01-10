module TimeListSpec where

import ALocalTime
import TimeList

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
  times <- fmap sort . listOf1 $ unALocalTime <$> arbitrary
  let withoutNewest = reverse . tail . reverse $ times
  pure (mkTimeList times, mkTimeList withoutNewest)
