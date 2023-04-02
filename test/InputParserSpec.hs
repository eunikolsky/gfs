module InputParserSpec where

import GFS (TimeItem(..))
import InputParser hiding (parseTimes)
import InputParser qualified (parseTimes)

import Data.Char
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Time ()

spec :: Spec
spec = parallel $ do
  describe "parseTimes" $ do
    let format = "%Y-%m-%d_%H_%M_%S"

    context "exact match" $ do
      let parseTimes = InputParser.parseTimes ExactMatch

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

      prop "doesn't skip leading chars" $ \(NonEmpty pairs) -> do
        let strings = (\(NonEmptyUnicodeString prefix, LocalTimeWithIntSeconds localTime) -> T.pack $
              prefix <> formatTime defaultTimeLocale (T.unpack format) localTime)
              <$> pairs
            actual = parseTimes format strings
            expected = InvalidTime $ head strings
        fmap itTime <$> actual `shouldBe` Left expected

      prop "doesn't skip trailing chars" $ \(NonEmpty pairs) -> do
        let strings = (\(NonEmptyUnicodeString suffix, LocalTimeWithIntSeconds localTime) -> T.pack $
              formatTime defaultTimeLocale (T.unpack format) localTime <> suffix)
              <$> pairs
            actual = parseTimes format strings
            expected = InvalidTime $ head strings
        fmap itTime <$> actual `shouldBe` Left expected

    context "lenient match" $ do
      let parseTimes = InputParser.parseTimes LenientMatch

      prop "skips leading chars until it can parse times" $ \(NonEmpty pairs) -> do
        let strings = (\(prefix, LocalTimeWithIntSeconds localTime) -> T.pack $
              getSkippablePrefix prefix <> formatTime defaultTimeLocale (T.unpack format) localTime)
              <$> pairs
            actual = parseTimes format strings
            expected = getLocalTimeWithIntSeconds . snd <$> pairs
        fmap itTime <$> actual `shouldBe` Right expected

      prop "skips trailing chars until it can parse times" $ \(NonEmpty pairs) -> do
        let strings = (\(UnicodeString suffix, LocalTimeWithIntSeconds localTime) -> T.pack $
              formatTime defaultTimeLocale (T.unpack format) localTime <> suffix)
              <$> pairs
            actual = parseTimes format strings
            expected = getLocalTimeWithIntSeconds . snd <$> pairs
        fmap itTime <$> actual `shouldBe` Right expected

-- | A unicode string that can't end with a digit. Found because the
-- "skips leading chars until it can parse times" property discovered this
-- failing case with seed `401874497`:
--
--   Falsifiable (after 78 tests and 17 shrinks):
--     NonEmpty {getNonEmpty = [(UnicodeString {getUnicodeString = "1"},LocalTimeWithIntSeconds {getLocalTimeWithIntSeconds = 1858-11-17 00:00:00})]}
--   expected: Right [1858-11-17 00:00:00]
--    but got: Right [11858-11-17 00:00:00]
--
-- If we accept a prefix ending with a digit, then there is no general easy way
-- to distinguish year `11858` from `1858`. We could limit a year to 4 digits…
newtype SkippablePrefix = SkippablePrefix { getSkippablePrefix :: String }
  deriving newtype Show

mkSkippablePrefix :: String -> SkippablePrefix
mkSkippablePrefix s =
  let maybeLastCharIsDigit = fmap isDigit . listToMaybe $ reverse s
  in if fromMaybe False maybeLastCharIsDigit
      -- it's unexpected that `discard` works in a pure function, outside of `Gen`
      then discard
      else SkippablePrefix s

instance Arbitrary SkippablePrefix where
  arbitrary = mkSkippablePrefix . getUnicodeString <$> arbitrary
  shrink (SkippablePrefix p) = mkSkippablePrefix <$> shrink p

newtype NonEmptyUnicodeString = NonEmptyUnicodeString { getNonEmptyUnicodeString :: String }
  deriving newtype Show

mkNonEmptyUnicodeString :: String -> NonEmptyUnicodeString
mkNonEmptyUnicodeString s
  | null s = discard
  | otherwise = NonEmptyUnicodeString s

instance Arbitrary NonEmptyUnicodeString where
  arbitrary = mkNonEmptyUnicodeString . getUnicodeString <$> arbitrary
  shrink = fmap mkNonEmptyUnicodeString . shrink . getNonEmptyUnicodeString

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
