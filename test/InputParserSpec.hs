module InputParserSpec where

import InputParser

import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec

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

      actual `shouldBe` Right expected

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

      actual `shouldBe` Right expected

    it "returns an error for an empty string" $ do
      parseTimes format [""] `shouldBe` Left (InvalidTime "")

    it "returns the first encountered error" $ do
      parseTimes format
        [ "2023-07-13_12_31_49"
        , "foobar"
        , ""
        ] `shouldBe` Left (InvalidTime "foobar")

createTime :: (Int, Int, Int, Int, Int, Int) -> LocalTime
createTime (y, m, d, h, mi, s) = LocalTime
  { localDay = fromGregorian (fromIntegral y) m d
  , localTimeOfDay = fromJust $ makeTimeOfDayValid h mi (realToFrac s)
  }
