module RangeParserSpec (spec) where

import GFS.Internal.GFSRange
import GFS.Internal.TimeInterval
import RangeParser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  let mkTimeIntervalDays = mkTimeIntervalHours . (* 24)

  describe "parseRanges" $ do
    it "should fail on empty input" $
      shouldBeLeft $ parseRanges ""

    prop "parses hours and days" $ \(Positive h) (Positive d) -> do
      let input = show h <> "h:" <> show d <> "d"
      parseRanges input `shouldParseSingleRange`
        GFSRange (mkTimeIntervalHours h) (mkTimeIntervalDays d)

shouldBeLeft :: Show b => Either a b -> Expectation
shouldBeLeft (Left _) = pure ()
shouldBeLeft (Right x) = expectationFailure $ "Expected Left, but got Right " <> show x

shouldParseSingleRange :: Either String GFSRanges -> GFSRange -> Expectation
shouldParseSingleRange result range = result `shouldBe` Right ranges
  where ranges = mkGFSRanges range []
