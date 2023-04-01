module InputParserSpec where

import GFS (TimeItem(..))
import InputParser

import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = do
  describe "parseTimes" $ do
    let format = "%Y-%m-%d_%H_%M_%S"

    it "parses times based on format" $ do
      let actual = parseTimes format
            [ "1999-12-31_00_00_00"
            , "2023-07-13_12_31_49"
            , "2000-01-01_23_59_59"
            ]

          expected = fmap createTime
            [ (1999, 12, 31, 00, 00, 00)
            , (2023, 07, 13, 12, 31, 49)
            , (2000, 01, 01, 23, 59, 59)
            ]

      fmap (fmap itTime) actual `shouldBe` Right expected

    it "returns empty list for empty input" $
      parseTimes format [] `shouldBe` Right []

    it "accepts non-format words in format" $ do
      let actual = parseTimes "computer_name %Y%m%d-%H%M%S.ext"
            [ "computer_name 19991231-000000.ext"
            , "computer_name 20230713-123149.ext"
            , "computer_name 20000101-235959.ext"
            ]

          expected = fmap createTime
            [ (1999, 12, 31, 00, 00, 00)
            , (2023, 07, 13, 12, 31, 49)
            , (2000, 01, 01, 23, 59, 59)
            ]

      fmap (fmap itTime) actual `shouldBe` Right expected

    it "stores the original string in the item" $ do
      let strings =
            [ "computer_name 19991231-000000.ext"
            , "computer_name 20230713-123149.ext"
            , "computer_name 20000101-235959.ext"
            ]
          actual = parseTimes "computer_name %Y%m%d-%H%M%S.ext" strings

          expected = strings

      fmap (fmap itStr) actual `shouldBe` Right expected

    it "returns an error for an empty string" $ do
      parseTimes format [""] `shouldBe` Left (InvalidTime "")

    it "returns the first encountered error" $ do
      parseTimes format
        [ "2023-07-13_12_31_49"
        , "foobar"
        , ""
        ] `shouldBe` Left (InvalidTime "foobar")

    prop "skips leading chars until it can parse times" $ \(NonEmpty pairs) -> do
      let strings = (\(UnicodeString prefix, LocalTimeWithIntSeconds localTime) -> T.pack $
            prefix <> formatTime defaultTimeLocale (T.unpack format) localTime)
            <$> pairs
          actual = parseTimes format strings
          expected = getLocalTimeWithIntSeconds . snd <$> pairs
      fmap itTime <$> actual `shouldBe` Right expected

-- | `LocalTime` with integer seconds because the default `format` in tests
-- doesn't parse milliseconds (should it?).
newtype LocalTimeWithIntSeconds = LocalTimeWithIntSeconds { getLocalTimeWithIntSeconds :: LocalTime }
  deriving Show

mkLocalTimeWithIntSeconds :: LocalTime -> LocalTimeWithIntSeconds
mkLocalTimeWithIntSeconds t@(LocalTime { localTimeOfDay }) =
    LocalTimeWithIntSeconds $ t { localTimeOfDay = intSecTimeOfDay localTimeOfDay }

intSecTimeOfDay :: TimeOfDay -> TimeOfDay
intSecTimeOfDay t@(TimeOfDay { todSec }) = t { todSec = fromIntegral @Int $ floor todSec }

instance Arbitrary LocalTimeWithIntSeconds where
  arbitrary = mkLocalTimeWithIntSeconds <$> arbitrary
  shrink (LocalTimeWithIntSeconds t) = mkLocalTimeWithIntSeconds <$> shrink t

createTime :: (Int, Int, Int, Int, Int, Int) -> LocalTime
createTime (y, m, d, h, mi, s) = LocalTime
  { localDay = fromGregorian (fromIntegral y) m d
  , localTimeOfDay = fromJust $ makeTimeOfDayValid h mi (realToFrac s)
  }
