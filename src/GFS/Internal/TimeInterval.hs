module GFS.Internal.TimeInterval
  ( TimeInterval -- not exporting the constructor
  , addHours
  , addMonths
  , addTimeInterval
  , mkTimeInterval
  , scaleTimeInterval
  , showTimeInterval
  , subTimeInterval
  ) where

import Data.Text (Text)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import qualified Data.Text as T

-- | Duration of time for the GFS algorithm with the minimum resolution
-- of one hour — this will be the minimum step for the default settings,
-- and this makes test data easier to understand.
-- Storing the number of months separately from the number of hours is required
-- to support calendar-based time calculations, similarly to `CalendarDiffTime`.
data TimeInterval = TimeInterval
  { tiMonths :: !Int
  , tiHours :: !Int
  }
  -- TODO is `Ord` even make sense here? since `tiHours` is unlimited, it may
  -- make one interval bigger than another with fewer `tiMonths`; multiplying
  -- months by a constant in addition to hours isn't valid for the same reason
  -- that we're using calendar-based calculations — it may mess up time
  -- calculations
  deriving (Eq, Ord)

instance Show TimeInterval where
  show (TimeInterval months hours) = mconcat ["I", show months, "M", show hours, "H"]

mkTimeInterval :: Int -> Int -> TimeInterval
mkTimeInterval months hours = TimeInterval { tiMonths = months, tiHours = hours }

addTimeInterval :: TimeInterval -> LocalTime -> LocalTime
addTimeInterval ti = addHours ti . addMonths ti

addHours :: TimeInterval -> LocalTime -> LocalTime
addHours (TimeInterval _ hours) = addLocalTime diffTime
  where
    diffTime :: NominalDiffTime
    diffTime = realToFrac $ hours * secondsInHour

addMonths :: TimeInterval -> LocalTime -> LocalTime
addMonths (TimeInterval months _) time = time
  { localDay = addGregorianMonthsClip (fromIntegral months) (localDay time)
  }

subTimeInterval :: TimeInterval -> LocalTime -> LocalTime
subTimeInterval ti = addMonths negativeTi . addHours negativeTi
  where negativeTi = scaleTimeInterval (-1) ti

scaleTimeInterval :: Int -> TimeInterval -> TimeInterval
scaleTimeInterval x (TimeInterval months hours) = TimeInterval
  { tiMonths = months * x
  , tiHours = hours * x
  }

secondsInHour :: Int
secondsInHour = 60 * 60

showTimeInterval :: TimeInterval -> Text
showTimeInterval (TimeInterval months hours) = mconcat
  [ T.pack $ show months
  , " months"
  , T.pack $ show hours
  , " hours"
  ]
