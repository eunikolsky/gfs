module GFS
    ( Period
    , PrettyTimeInterval(..)
    , cleanup
    ) where

import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime

-- |Wrapper for @NominalDiffTime@ that @show@s the value in a human-friendlier
-- |way than just seconds, e.g. "1 w 2 h 10 s".
-- |Warning: seconds are always displayed as integers.
newtype PrettyTimeInterval = PrettyTimeInterval NominalDiffTime

instance Show PrettyTimeInterval where
  show (PrettyTimeInterval diffTime) = formatUnits
    where
      formatUnits
        = ("0" `ifEmpty`)
        . intercalate " "
        . map (\(amount, unit) -> concat [show amount, " ", unit])
        . filter ((/= 0) . fst)
        . flip zip ["week", "d", "h", "min", "s"]
        . reverse
        . snd
        . foldl' (\(secondsLeft, unitAmounts) unit ->
            let (unitAmount, fewerSeconds) = secondsLeft `divMod` unit
            in (fewerSeconds, unitAmount:unitAmounts))
          (seconds, [])
        $ [week, day, hour, minute, second]

      seconds = truncate . nominalDiffTimeToSeconds $ diffTime

      second = 1
      minute = 60 * second
      hour = 60 * minute
      day = 24 * hour
      week = 7 * day

      ifEmpty :: [a] -> [a] -> [a]
      ifEmpty def [] = def
      ifEmpty _ xs = xs

-- TODO it's somewhat strange to force a pretty time interval instead of a
-- regular one…
type OffsetFrom = PrettyTimeInterval
type OffsetTo = PrettyTimeInterval

-- FIXME clarify the terms "range" vs "period"! and the direction OffsetFrom <-> OffsetTo!

-- |A @period@ is a segment of time which counts back from "now".
-- |@OffsetFrom@ must be less than @OffsetTo@ because the offsets here are
-- |positive even though they represent negative offsets from "now".
-- |
-- |E.g.: @(1d, 7d)@ means a range of @[now - 7d … now - 1d]@.
type Period = (OffsetFrom, OffsetTo)

-- |The main cleanup function that takes a sorted list of @times@ and returns
-- |the ones that should be removed to satisfy the requirements of the GFS
-- |algorithm. The order of returned cleaned up times is not specified.
-- |
-- |An example of how @period@ works: if @period@ is @(1d, 6.5d)@, then:
-- |* the time segment of @[now-6.5d; now-1d]@ is considered;
-- |* it is split into sub-periods based on the @offsetFrom@ value (@1d@ here):
-- |  @[(now-2d, now-1d), (now-3d, now-2d), …, (now-6d, now-5d), (now-6.5d, now-6d)]@;
-- |* in each period, only the newest time is left.
-- |
-- |__Note__: the most recent time is never cleaned up.
-- |__Note__: times that are in the future (bigger than @now@) are never removed
-- |(for safety).
-- |
-- |__Assumption__: @times@ is sorted in the ascending order (oldest to newest).
cleanup :: Period -> [LocalTime] -> LocalTime -> [LocalTime]
cleanup (PrettyTimeInterval from, PrettyTimeInterval to) times now
  = consider . leaveNewest . takeWhile (before now) $ times
  where
    numSubperiods = (subtract 1) . ceiling . nominalDiffTimeToSeconds $ to - from

    -- Any of the input times here can be cleaned up.
    consider times = outsideOfPeriod times ++ insidePeriod times
      where
        inside :: LocalTime -> Bool
        inside time = time >= addLocalTime (-to) now && time <= addLocalTime (-from) now
        outsideOfPeriod = filter (not . inside)
        insidePeriod = leaveAtMost numSubperiods . filter inside

    before = flip (<=)
    leaveNewest = dropLast
    leaveAtMost = drop

-- |Returns the list without the last element.
dropLast :: [a] -> [a]
dropLast [] = []
dropLast xs = take (length xs - 1) xs

-- oldest remaining time so far and a list of dropped times
type CleanupState = (LocalTime, [LocalTime])
