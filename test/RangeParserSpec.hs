module RangeParserSpec (spec) where

import RangeParser
import Test.Hspec

spec :: Spec
spec = parallel $
  describe "parseRanges" $ do
    it "should fail on empty input" $
      shouldBeLeft $ parseRanges ""

shouldBeLeft :: Show b => Either a b -> IO ()
shouldBeLeft (Left _) = pure ()
shouldBeLeft (Right x) = expectationFailure $ "Expected Left, but got Right " <> show x
