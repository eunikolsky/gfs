module RangedInput where

import GFS

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Word
import Test.QuickCheck

data RangeInput = RangeInput
  { rangeMaxTime :: LocalTime
  , rangeTimes :: [LocalTime]
  }
  deriving Show

-- |Data type defining all the inputs for the @cleanup@ function. The reason for
-- |the separate type is we need to generate an initial offset and a number of
-- |ranges, then generate arbitrary times inside every range, and I'm not sure
-- |it's possible to do that with @property@. Plus it seems to be a good idea
-- |anyway.
data RangedInput = RangedInput
  { riNow :: LocalTime
  , riPeriod :: Period
  --, riNumRanges :: Positive Word8
  , riRanges :: NonEmptyList RangeInput
  }
  deriving Show

instance Arbitrary RangedInput where
  arbitrary = do
    let numRanges = 6
        range = RangeInput (LocalTime (ModifiedJulianDay 0) midnight) []
    return $ RangedInput
      (LocalTime (ModifiedJulianDay 0) midnight)
      (nominalDay, (numRanges + 1) * nominalDay)
      (NonEmpty [range])
