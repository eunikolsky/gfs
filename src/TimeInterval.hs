module TimeInterval
  ( TimeInterval -- not exporting the constructor
  , addTimeInterval
  , mkTimeInterval
  , scaleTimeInterval
  , subTimeInterval
  ) where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime

-- | Duration of time for the GFS algorithm with the minimum resolution
-- of one hour — this will be the minimum step for the default settings,
-- and this makes test data easier to understand.
data TimeInterval = TimeInterval
  { tiMonths :: Int
  , tiHours :: Int
  }

instance Show TimeInterval where
  show (TimeInterval months hours) = mconcat ["I", show months, "M", show hours, "H"]

mkTimeInterval :: Int -> Int -> TimeInterval
mkTimeInterval months hours = TimeInterval { tiMonths = months, tiHours = hours }

addTimeInterval :: TimeInterval -> LocalTime -> LocalTime
addTimeInterval (TimeInterval months hours) = addMonths . addHours
  where
    diffTime :: NominalDiffTime
    diffTime = realToFrac $ hours * secondsInHour
    addHours = addLocalTime diffTime
    addMonths time = time
      { localDay = addGregorianMonthsClip (fromIntegral months) (localDay time)
      }

subTimeInterval :: TimeInterval -> LocalTime -> LocalTime
subTimeInterval int = addTimeInterval (scaleTimeInterval (-1) int)

scaleTimeInterval :: Int -> TimeInterval -> TimeInterval
scaleTimeInterval x (TimeInterval months hours) = TimeInterval
  { tiMonths = months * x
  , tiHours = hours * x
  }

secondsInHour :: Int
secondsInHour = 60 * 60
