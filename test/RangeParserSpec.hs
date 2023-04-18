module RangeParserSpec (spec) where

import GFS.Internal.GFSRange
import GFS.Internal.TimeInterval
import RangeParser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = parallel $ do
  describe "parseRanges" $ do
    it "should fail on empty input" $
      shouldBeLeft $ parseRanges ""

    prop "parses a single range" $ \(InputRange input expected) -> do
      parseRanges input `shouldParseSingleRange` expected

    prop "parses multiple ranges" $ \(InputRanges input expected) -> do
      parseRanges input `shouldParse` expected

-- | An arbitrary input `GFSRange` as a string and how it should be parsed.
data InputRange = InputRange
  { _irInput :: !String
  , irExpectedRange :: GFSRange
  }

instance Arbitrary InputRange where
  arbitrary = do
    let arbitraryInterval =
          elements [mkTimeIntervalHours, mkTimeIntervalDays, mkTimeIntervalWeeks, mkTimeIntervalMonths, mkTimeIntervalYears]
          <*> (getPositive <$> arbitrary)

    rStep <- arbitraryInterval
    rLimit <- arbitraryInterval

    let range = GFSRange { rStep, rLimit }
    pure $ InputRange (show range) range

instance Show InputRange where
  show (InputRange s range) = show range <> " (" <> s <> ")"

-- | Arbitrary input `GFSRanges` as a string and how they should be parsed.
data InputRanges = InputRanges String GFSRanges
  deriving Show

instance Arbitrary InputRanges where
  arbitrary = do
    first <- irExpectedRange <$> arbitrary
    rest <- getNonEmpty . fmap irExpectedRange <$> arbitrary
    let ranges = mkGFSRanges first rest
    pure $ InputRanges (show ranges) ranges

shouldBeLeft :: Show b => Either a b -> Expectation
shouldBeLeft (Left _) = pure ()
shouldBeLeft (Right x) = expectationFailure $ "Expected Left, but got Right " <> show x

shouldParse :: Either String GFSRanges -> GFSRanges -> Expectation
shouldParse result ranges = result `shouldBe` Right ranges

shouldParseSingleRange :: Either String GFSRanges -> GFSRange -> Expectation
shouldParseSingleRange result range = result `shouldParse` ranges
  where ranges = mkGFSRanges range []
