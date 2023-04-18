module Config
  ( Action(..)
  , Config(..)
  , defaultRanges
  ) where

import GFS

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

    hour = mkTimeIntervalHours 1
    day = mkTimeIntervalDays 1
    week = mkTimeIntervalWeeks 1
    month = mkTimeIntervalMonths 1
    year = mkTimeIntervalYears 1

    years = mkTimeIntervalYears

until :: TimeInterval -> TimeInterval -> GFSRange
until = GFSRange
