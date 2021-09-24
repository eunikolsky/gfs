module GFS
    ( Period
    , cleanup
    ) where

import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime

type OffsetFrom = NominalDiffTime
type OffsetTo = NominalDiffTime
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
cleanup period times now = []
--fromMaybe [] $ do
  --let newestToOldest = reverse times
  --(newest, others) <- uncons newestToOldest
  --return . snd $ foldl' dropNextWhenCloseToPrevious (newest, []) others

  --where
    -- The folding function takes the oldest remaining time so far and if it's
    -- closer than the @period@ to the next older time, that time is dropped
    -- (appended to the state), otherwise it is made the oldest remaining time.
    --dropNextWhenCloseToPrevious :: CleanupState -> LocalTime -> CleanupState
    --dropNextWhenCloseToPrevious (oldest, dropped) older =
      --let olderShouldBeDropped = False -- oldest `diffLocalTime` older <= period
      --in if olderShouldBeDropped
        --then (oldest, older : dropped)
        --else (older, dropped)

-- oldest remaining time so far and a list of dropped times
type CleanupState = (LocalTime, [LocalTime])
