module GFSSpec where

import ArbitraryLocalTime
import GFS

import Data.List
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "cleanup" $ do
    it "returns nothing for empty input" $
      cleanup [] `shouldBe` []

    it "never cleans up the newest time" $ do
      property $ \times ->
        let input = sort . getNonEmpty $ times
            -- this is safe because @times@ is a @NonEmptyList LocalTime@
            newest = last input
            cleanedUp = cleanup input
        in newest `notElem` cleanedUp
