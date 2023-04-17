module GFS.Internal.TimeInterval
  ( TimeInterval -- not exporting the constructor
  , mkTimeIntervalHours
  , mkTimeIntervalMonths
  , scaleTimeInterval
  , showTimeInterval
  , subTimeInterval
  ) where

import Data.Text (Text)
import Data.Time.Calendar
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
-- of one hour — this is the minimum step for the default settings,
-- and this makes test data easier to understand. This type allows to store
-- either N hours (or days/weeks), or N months (or years) — this makes the
-- system simpler.
--
-- The number of months can't be represented in a number of hours because of the
-- variable months length. The number of months is required to support
-- calendar-based time calculations, similarly to `CalendarDiffTime`.
data TimeInterval
  = Hours !Hours
  | Months !Months
  -- TODO is `Ord` even make sense here? since `Hours` is unlimited, it may
  -- make one interval bigger than another with fewer `Months`; multiplying
  -- months by a constant in addition to hours isn't valid for the same reason
  -- that we're using calendar-based calculations — it may mess up time
  -- calculations
  deriving (Eq, Ord)

instance Show TimeInterval where
  show (Hours hours) = case getDays hours of
    Just days -> case getWeeks days of
      Just weeks -> show weeks <> "w"
      Nothing -> show days <> "d"
    Nothing -> show hours <> "h"
  show (Months months) = show months <> "m"

getDays :: Hours -> Maybe Days
getDays = getIntegralComponent 24

getWeeks :: Days -> Maybe Weeks
getWeeks = getIntegralComponent 7

getIntegralComponent :: Integral a => a -> a -> Maybe a
getIntegralComponent k d = let (intComponent, rest) = d `divMod` k
  in if rest == 0 then Just intComponent else Nothing

mkTimeIntervalHours :: Hours -> TimeInterval
mkTimeIntervalHours = Hours

mkTimeIntervalMonths :: Months -> TimeInterval
mkTimeIntervalMonths = Months

subTimeInterval :: TimeInterval -> LocalTime -> LocalTime
subTimeInterval (Hours hours) time = addLocalTime diffTime time
  where diffTime = realToFrac . negate @Int $ fromIntegral hours * secondsInHour
subTimeInterval (Months months) time = time
  { localDay = addGregorianMonthsClip (fromIntegral . negate @Int $ fromIntegral months) (localDay time)
  }

scaleTimeInterval :: Word16 -> TimeInterval -> TimeInterval
scaleTimeInterval x (Hours h) = Hours $ h * x
scaleTimeInterval x (Months m) = Months $ m * x

secondsInHour :: Num a => a
secondsInHour = 60 * 60

showTimeInterval :: TimeInterval -> Text
showTimeInterval (Months months) = T.pack (show months) <> " months"
showTimeInterval (Hours hours) = T.pack (show hours) <> " hours"
