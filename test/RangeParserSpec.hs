module RangeParserSpec (spec) where

import GFS.Internal.GFSRange
import GFS.Internal.TimeInterval
import RangeParser
import Test.Hspec

spec :: Spec
spec = parallel $ do
  let mkTimeIntervalDays = mkTimeIntervalHours . (* 24)

  describe "parseRanges" $ do
    it "should fail on empty input" $
      shouldBeLeft $ parseRanges ""

    it "parses hours and days" $
      parseRanges "1h:1d" `shouldParseSingleRange`
        GFSRange (mkTimeIntervalHours 1) (mkTimeIntervalDays 1)

shouldBeLeft :: Show b => Either a b -> Expectation
shouldBeLeft (Left _) = pure ()
shouldBeLeft (Right x) = expectationFailure $ "Expected Left, but got Right " <> show x

shouldParseSingleRange :: Either String GFSRanges -> GFSRange -> Expectation
shouldParseSingleRange result range = result `shouldBe` Right ranges
  where ranges = mkGFSRanges range []
