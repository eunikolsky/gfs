module GFS
    ( Period
    , PrettyTimeInterval(..)
    , cleanup
    , cleanup_
    ) where

import Control.Monad.Writer.Strict
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime

-- |Wrapper for @NominalDiffTime@ that @show@s the value in a human-friendlier
-- |way than just seconds, e.g. "1 w 2 h 10 s".
-- |Warning: seconds are always displayed as integers.
newtype PrettyTimeInterval = PrettyTimeInterval
  { unPrettyTimeInterval :: NominalDiffTime }

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
-- |__Assumption__: @periods@ is sorted in the ascending order (smaller to
-- |bigger) and the start of the next period is the end of the previous period,
-- |e.g.: @[(1d, 7d), (7d, 31d), (31d, 1y)]@.
cleanup :: NE.NonEmpty Period -> [LocalTime] -> LocalTime -> [LocalTime]
cleanup periods times = fst . runWriter . cleanup_ periods times

type L = Writer [String]

cleanup_ :: NE.NonEmpty Period -> [LocalTime] -> LocalTime -> L [LocalTime]
cleanup_ periods times now
  = consider . leaveNewest . takeWhile (before now) $ times
  where
    numSubperiods :: Period -> Int
    numSubperiods (PrettyTimeInterval from, PrettyTimeInterval to)  = ceiling . nominalDiffTimeToSeconds $ (to - from) / from

    getSubperiods :: Period -> [(LocalTime, LocalTime)]
    getSubperiods period@(PrettyTimeInterval from, _) = adjacentPairs . reverse $
      map
        (\index -> (addLocalTime (-from * (fromIntegral index + 1)) now))
        [0..numSubperiods period]

    timePeriods :: [(LocalTime, LocalTime)]
    timePeriods = (\(PrettyTimeInterval from, PrettyTimeInterval to) ->
      (addLocalTime (-to) now, addLocalTime (-from) now))
      <$> NE.toList periods

    withinOnePeriod = withinOneSubperiod

    withinOneSubperiod :: [(LocalTime, LocalTime)] -> LocalTime -> LocalTime -> Bool
    withinOneSubperiod subperiods t0 t1 = any (bothInsideSubperiod t0 t1) subperiods

    bothInsideSubperiod :: LocalTime -> LocalTime -> (LocalTime, LocalTime) -> Bool
    bothInsideSubperiod t0 t1 subperiod = t0 `insideSubperiod` subperiod && t1 `insideSubperiod` subperiod
      where t `insideSubperiod` (from, to) = t >= from && t < to

    combinedPeriod :: Period
    combinedPeriod = (earliestFrom, latestTo)
      where
        (earliestFrom, _) = NE.head periods
        (_, latestTo) = NE.last periods

    -- Any of the input times here can be cleaned up.
    consider :: [LocalTime] -> L [LocalTime]
    consider times = do
      tell [
        --"numSubperiods " <> show numSubperiods,
        --"subperiods " <> show subperiods,
        "considering " <> show times]
      let grouped = zip (NE.toList . NE.reverse $ periods) $ groupBy (withinOnePeriod timePeriods) times
          outside = outsideOfPeriod combinedPeriod times
      insideTimes <- concat <$> traverse (\(period, times) -> insidePeriod period times) grouped
      pure $ outside ++ insideTimes

      where
        isInside :: Period -> LocalTime -> Bool
        isInside (PrettyTimeInterval from, PrettyTimeInterval to) time = time >= addLocalTime (-to) now && time <= addLocalTime (-from) now

        outsideOfPeriod :: Period -> [LocalTime] -> [LocalTime]
        outsideOfPeriod period = filter (not . isInside period)
        insidePeriod :: Period -> [LocalTime] -> L [LocalTime]
        insidePeriod period times = do
          let subperiods = getSubperiods period
          tell ["insidePeriod " <> show period]
          filtered <- log "filter" (filter (isInside period)) times
          grouped <- log "groupBy" (groupBy (withinOneSubperiod subperiods)) filtered
          exceptNewest <- log "not newest" (map leaveNewest) grouped
          log "concat" concat exceptNewest

        -- filter :: (a -> Bool) -> [a] -> [a]
        -- filter inside :: [LocalTime] -> [LocalTime]
        -- log (filter inside) :: [LocalTime] -> L [LocalTime]
        -- groupBy x :: [LocalTime] -> [[LocalTime]]
        -- log (groupBy x) :: [LocalTime] -> L [[LocalTime]]

        log :: (Show a, Show b) => String -> (a -> b) -> a -> L b
        log n f x = do
          let r = f x
          tell [n <> ": " <> show x <> " => " <> show r]
          pure r

    before = flip (<=)
    leaveNewest = dropLast
    leaveAtMost = drop

-- |Returns the list without the last element.
dropLast :: [a] -> [a]
dropLast [] = []
dropLast xs = init xs

-- FIXME deduplicate
-- |Returns a list of adjacent pairs from the source list.
adjacentPairs :: [a] -> [(a, a)]
adjacentPairs [] = []
adjacentPairs xs = zip xs (tail xs)

-- oldest remaining time so far and a list of dropped times
type CleanupState = (LocalTime, [LocalTime])
