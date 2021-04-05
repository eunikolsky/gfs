module GFS
    ( cleanup
    ) where

import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.LocalTime

-- |The main cleanup function that takes a sorted list of @times@ and returns
-- |the ones that should be removed to satisfy the requirements of the GFS
-- |algorithm. At most one of the @times@ is left within every @period@ going
-- |backwards from the most recent time. The order of returned cleaned up times
-- |is not specified.
-- |
-- |__Note__: the most recent time is never cleaned up.
-- |__Note__: times that are in the future (bigger than @now@) are never removed
-- |(for safety).
-- |
-- |__Assumption__: @times@ is sorted in the ascending order (oldest to newest).
cleanup :: NominalDiffTime -> [LocalTime] -> LocalTime -> [LocalTime]
cleanup period times now = fromMaybe [] $ do
  let newestToOldest = reverse times
  (newest, others) <- uncons newestToOldest
  return . snd $ foldl' dropNextWhenCloseToPrevious (newest, []) others

  where
    -- The folding function takes the oldest remaining time so far and if it's
    -- closer than the @period@ to the next older time, that time is dropped
    -- (appended to the state), otherwise it is made the oldest remaining time.
    dropNextWhenCloseToPrevious :: CleanupState -> LocalTime -> CleanupState
    dropNextWhenCloseToPrevious (oldest, dropped) older =
      let olderShouldBeDropped = oldest `diffLocalTime` older <= period
      in if olderShouldBeDropped
        then (oldest, older : dropped)
        else (older, dropped)

-- oldest remaining time so far and a list of dropped times
type CleanupState = (LocalTime, [LocalTime])
