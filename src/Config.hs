module Config
  ( Action(..)
  , Config(..)
  , defaultRanges
  ) where

import GFS (GFSRange(..), GFSRanges, TimeInterval, mkGFSRanges, mkTimeInterval)

import Data.Text (Text)
import Prelude hiding (until)
import InputParser (FormatMatch)

data Action
  = RunGFS Config
  | ShowVersion

data Config = Config
  { cfgTimeFormat :: Text
  , cfgGFSRanges :: GFSRanges
  , cfgVerbose :: Bool
  , cfgFormatMatch :: FormatMatch
  }

-- | The default set of ranges for the GFS algorithm, based on TimeMachine's rules.
defaultRanges :: GFSRanges
defaultRanges = mkGFSRanges hourlyForDay
  [ dailyForMonth
  , weeklyForYear
  , monthlyUntilFourYears
  , yearlyUntilThirtyTwoYears
  ]
  where
    hourlyForDay = hour `until` day
    dailyForMonth = day `until` month
    weeklyForYear = week `until` year
    monthlyUntilFourYears = month `until` years 4
    yearlyUntilThirtyTwoYears = year `until` years 32

    hour = mkTimeInterval 0 1
    day = days 1
    week = days 7
    month = months 1
    year = years 1

    days = mkTimeInterval 0 . (* 24)
    months = flip mkTimeInterval 0
    years = months . (* 12)

until :: TimeInterval -> TimeInterval -> GFSRange
until = GFSRange
