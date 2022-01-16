module GFS
    ( Offsets(..)
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
  deriving (Eq, Ord)

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
type Offset = PrettyTimeInterval
--type OffsetTo = PrettyTimeInterval

newtype Offsets = Offsets { unOffsets :: NE.NonEmpty Offset }

instance Show Offsets where
  show os = "Offsets " ++ (show . NE.toList . unOffsets $ os)

-- FIXME clarify the terms "range" vs "period"! and the direction OffsetFrom <-> OffsetTo!

-- |A @period@ is a segment of time which counts back from "now".
-- |@OffsetFrom@ must be less than @OffsetTo@ because the offsets here are
-- |positive even though they represent negative offsets from "now".
-- |
-- |E.g.: @(1d, 7d)@ means a range of @[now - 7d … now - 1d]@.
type Period_ = (Offset, Offset)

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
-- |__Note__: @periods@ can start with zero, which has the effect of leaving
-- |the most recent time between @now@ and the next period instead of removing
-- |all times in that range.
-- |__Note__: times that are in the future (bigger than @now@) are never removed
-- |(for safety).
-- |
-- |__Assumption__: @times@ is sorted in the ascending order (oldest to newest).
-- |__Assumption__: @periods@ is sorted in the ascending order (smaller to
-- |bigger), e.g.: @[0, 1d, 7d, 31d, 1y]@.
cleanup :: Offsets -> [LocalTime] -> LocalTime -> [LocalTime]
cleanup offsets times = fst . runWriter . cleanup_ offsets times

type L = Writer [String]

-- |Semantically equivalent to `Periods + LocalTime`.
type TimePeriod = (LocalTime, LocalTime)

-- |Converts `offsets` into times by subtracting them from `now`.
-- |`offsets` are assumed to be sorted smaller to bigger; return
-- |times are also sorted small to bigger.
-- |E.g.:
-- |- `now = 2022-01-10 00:00`;
-- |- `offsets = [0, 12h, 1d, 10d]`;
-- |=> `[2021-12-31 00:00, 2022-01-09 00:00, 2022-01-09 12:00, 2022-01-10 00:00]`.
timeDelimiters :: LocalTime -> Offsets -> NE.NonEmpty LocalTime
timeDelimiters now (Offsets offsets) = orderAsc times
  where
    orderAsc = NE.reverse
    times = (`addLocalTime` now) . negate . unPrettyTimeInterval <$> offsets

-- |Returns a list of offsets of all subperiods from the list of period
-- |`offsets`. The returned offsets define the boundaries where only a single
-- |time should be left.
-- |E.g.:
-- |- `offsets = [8h, 24h, 3d, 10d]` (that is, the first period `8h…24h` has
-- |  three subperiods with offsets `[8h, 16h, 24h]`, the second period
-- |  `1d…3d` has two subperiods with offsets `[1d, 2d, 3d]`, and the third
-- |  period `3d…10d` has three subperiods (the last one is shorter) with
-- |  offsets `[3d, 6d, 9d, 10d]`;
-- |=> `[8h, 16h, 1d, 2d, 3d, 6d, 9d, 10d]`.
allSubperiodOffsets :: Offsets -> Offsets
allSubperiodOffsets (Offsets offsets) =
  prependInitialOffset . mergeOffsets $ subperiodEnds <$> adjacentPairs offsets
  where
    prependInitialOffset :: [Offset] -> Offsets
    prependInitialOffset = Offsets . NE.fromList . (NE.head offsets :)
    mergeOffsets :: [NE.NonEmpty Offset] -> [Offset]
    mergeOffsets offsetsList = maybe [] (NE.toList . join) $ NE.nonEmpty offsetsList
    subperiodEnds :: (Offset, Offset) -> NE.NonEmpty Offset
    subperiodEnds (PrettyTimeInterval 0, to) = NE.fromList [to]
    subperiodEnds (PrettyTimeInterval from, PrettyTimeInterval to) =
      NE.fromList $ unfoldr
        (\off -> if off >= to
          then Nothing
          else Just . dup PrettyTimeInterval $ min (off + from) to)
        from

dup :: (a -> b) -> a -> (b, a)
dup f x = (f x, x)

-- |Returns pairs of adjacent elements in non-empty `list`. The return type
-- |is possibly an empty list for the case when input `list` has one element.
adjacentPairs :: NE.NonEmpty a -> [(a, a)]
adjacentPairs xs = NE.toList xs `zip` NE.tail xs

type InTime = LocalTime
type RmTime = LocalTime

type CleanupInsideState = ([LocalTime], [RmTime])

-- |Determines which of the given @intimes@ should be cleaned up (they are
-- |returned from this function) so that only the newest time is left
-- |in each of the periods delimited by @times@. This is the core of the
-- |GFS algorithm.
-- |__Assumption__: all @intimes@ are within the @times@.
cleanupInsideTimes :: [LocalTime] -> [InTime] -> [RmTime]
cleanupInsideTimes times = snd . foldr check (reverse times, [])
  where
    check :: InTime -> CleanupInsideState -> CleanupInsideState
    check intime (times@(time:rest), rmtimes) =
      if intime >= time
      then (times, intime:rmtimes)
      else (nexttimes, rmtimes)
      where
        nexttimes = dropUntil (\t -> intime >= t) rest
    check intime ([], rmtimes) = ([], intime:rmtimes)

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil f = dropWhile (not . f)

cleanup_ :: Offsets -> [LocalTime] -> LocalTime -> L [LocalTime]
cleanup_ os@(Offsets offsets) times now
  = consider . takeWhile (before now) $ times
  where
    combinedPeriod :: Period_
    combinedPeriod = (NE.head offsets, NE.last offsets)

    -- Any of the input times here can be cleaned up.
    consider :: [LocalTime] -> L [LocalTime]
    consider times = do
      tell [
        --"numSubperiods " <> show numSubperiods,
        --"subperiods " <> show subperiods,
        "considering " <> show times]
      let (inside, outside) = partition (isInside combinedPeriod) times
          delimiters = NE.toList . timeDelimiters now $ allSubperiodOffsets os
          insideTimes = cleanupInsideTimes delimiters inside
      pure $ outside ++ insideTimes

      where
        isInside :: Period_ -> LocalTime -> Bool
        isInside (PrettyTimeInterval from, PrettyTimeInterval to) time = time >= addLocalTime (-to) now && time <= addLocalTime (-from) now

    before = flip (<=)

-- |Returns the list without the last element.
dropLast :: [a] -> [a]
dropLast [] = []
dropLast xs = init xs
