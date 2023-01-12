module GFSRange
  ( GFSRange(..)
  , applyRange
  ) where

import Data.Time.Calendar
import Data.Time.LocalTime

-- | A single range for the GFS algorithm; it's used to calculate checkpoints relative
-- to "now" by going back in time by `rStep` from a start time as many times as
-- necessary until the `rLimit` from `"now".
-- `CalendarDiffTime` (instead of `NominalDiffTime`) is required to support
-- calendar-based time calculations.
data GFSRange = GFSRange
  { rStep :: CalendarDiffTime
  , rLimit :: CalendarDiffTime
  }
  deriving Show

type StartTime = LocalTime
type Now = LocalTime

applyRange :: GFSRange -> Now -> StartTime -> [LocalTime]
applyRange range now _ = [endTime range now]

endTime :: GFSRange -> Now -> LocalTime
endTime (GFSRange _ limit) = subtractMonths . subtractTime
  where
    subtractTime = addLocalTime (negate $ ctTime limit)
    diffMonths = scaleCalendarDiffDays (-1) $ calendarMonths limit
    subtractMonths time = time
      { localDay = addGregorianDurationClip diffMonths (localDay time)
      }

calendarMonths :: CalendarDiffTime -> CalendarDiffDays
calendarMonths (CalendarDiffTime months _) = CalendarDiffDays months 0
