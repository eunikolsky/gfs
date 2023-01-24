module InputParserSpec where

import InputParser

import Data.Maybe
import Data.Time.Calendar
import Data.Time.LocalTime
import Test.Hspec

spec :: Spec
spec = do
  describe "parseTimes" $ do
    it "parses times based on format" $ do
      let actual = parseTimes "%Y-%m-%d_%H_%M_%S"
            [ "1999-12-31_00_00_00"
            , "2023-07-13_12_31_49"
            , "2000-01-01_23_59_59"
            ]

          expected = fmap createTime
            [ (1999, 12, 31, 00, 00, 00)
            , (2023, 07, 13, 12, 31, 49)
            , (2000, 01, 01, 23, 59, 59)
            ]

      actual `shouldBe` expected

createTime :: (Int, Int, Int, Int, Int, Int) -> LocalTime
createTime (y, m, d, h, mi, s) = LocalTime
  { localDay = fromGregorian (fromIntegral y) m d
  , localTimeOfDay = fromJust $ makeTimeOfDayValid h mi (realToFrac s)
  }
