module GFSRange
  ( GFSRange(..)
  , applyRange
  ) where

import Checkpoints

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

applyRange :: GFSRange -> Now -> StartTime -> Checkpoints
applyRange range@(GFSRange step _) now startTime = mkCheckpoints endTime times
  where
    endTime = getEndTime range now
    times = safeTail . takeWhile (> endTime) $ iterate (subCalendarDiffTime step) startTime

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

subCalendarDiffTime :: CalendarDiffTime -> LocalTime -> LocalTime
subCalendarDiffTime diff@(CalendarDiffTime _ diffTime) = subtractMonths . subtractTime
  where
    subtractTime = addLocalTime (negate diffTime)
    diffMonths = scaleCalendarDiffDays (-1) $ calendarMonths diff
    subtractMonths time = time
      { localDay = addGregorianDurationClip diffMonths (localDay time)
      }

getEndTime :: GFSRange -> Now -> LocalTime
getEndTime (GFSRange _ limit) = subCalendarDiffTime limit

calendarMonths :: CalendarDiffTime -> CalendarDiffDays
calendarMonths (CalendarDiffTime months _) = CalendarDiffDays months 0
