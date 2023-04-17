module GFS.Internal.TimeInterval
  ( TimeInterval -- not exporting the constructor
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
import Data.Word

-- 16-bit value because it should be possible to represent 4 weeks (672 hours)
type Hours = Word16
-- 16-bit value because an 8-bit month could only go up to 21 years
type Months = Word16
type Days = Word16
type Weeks = Word16

-- | Duration of time for the GFS algorithm with the minimum resolution
-- of one hour — this will be the minimum step for the default settings,
-- and this makes test data easier to understand.
-- Storing the number of months separately from the number of hours is required
-- to support calendar-based time calculations, similarly to `CalendarDiffTime`.
data TimeInterval = TimeInterval
  { tiMonths :: !Months
  , tiHours :: !Hours
  }
  -- TODO is `Ord` even make sense here? since `tiHours` is unlimited, it may
  -- make one interval bigger than another with fewer `tiMonths`; multiplying
  -- months by a constant in addition to hours isn't valid for the same reason
  -- that we're using calendar-based calculations — it may mess up time
  -- calculations
  deriving (Eq, Ord)

instance Show TimeInterval where
  show (TimeInterval _ hours) = case getDays hours of
    Just days -> case getWeeks days of
      Just weeks -> show weeks <> "w"
      Nothing -> show days <> "d"
    Nothing -> show hours <> "h"

getDays :: Hours -> Maybe Days
getDays = getIntegralComponent 24

getWeeks :: Days -> Maybe Weeks
getWeeks = getIntegralComponent 7

getIntegralComponent :: Integral a => a -> a -> Maybe a
getIntegralComponent k d = let (intComponent, rest) = d `divMod` k
  in if rest == 0 then Just intComponent else Nothing

mkTimeInterval :: Months -> Hours -> TimeInterval
mkTimeInterval months hours = TimeInterval { tiMonths = months, tiHours = hours }

subTimeInterval :: TimeInterval -> LocalTime -> LocalTime
subTimeInterval ti = subMonths ti . subHours ti

subHours :: TimeInterval -> LocalTime -> LocalTime
subHours (TimeInterval _ hours) = addLocalTime diffTime
  where
    diffTime :: NominalDiffTime
    diffTime = realToFrac . negate @Int $ fromIntegral hours * secondsInHour

subMonths :: TimeInterval -> LocalTime -> LocalTime
subMonths (TimeInterval months _) time = time
  { localDay = addGregorianMonthsClip (fromIntegral . negate @Int $ fromIntegral months) (localDay time)
  }

scaleTimeInterval :: Word16 -> TimeInterval -> TimeInterval
scaleTimeInterval x (TimeInterval months hours) = TimeInterval
  { tiMonths = months * fromIntegral x
  , tiHours = hours * fromIntegral x
  }

secondsInHour :: Num a => a
secondsInHour = 60 * 60

showTimeInterval :: TimeInterval -> Text
showTimeInterval (TimeInterval months hours) = mconcat
  [ T.pack $ show months
  , " months, "
  , T.pack $ show hours
  , " hours"
  ]
