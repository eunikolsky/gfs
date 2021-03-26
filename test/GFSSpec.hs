module GFSSpec where

import GFS

import Data.Fixed (Fixed(..))
import Data.List
import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

-- This implements @Arbitrary@ for a valid @LocalTime@ and its parts directly.
-- It's not clear why QuickCheck doesn't have this out of the box; maybe because
-- there are multiple possible implementations and it's better to implement it
-- on a more specific newtype?
-- https://blog.ploeh.dk/2019/09/02/naming-newtypes-for-quickcheck-arbitraries/
instance Arbitrary Day where
  arbitrary = fmap ModifiedJulianDay arbitrary

instance Arbitrary TimeOfDay where
  arbitrary = do
    hour <- choose (0, 23)
    minute <- choose (0, 59)
    second <- fmap MkFixed arbitrary
    return $ TimeOfDay hour minute second

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary

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
