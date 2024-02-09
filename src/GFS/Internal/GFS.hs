module GFS.Internal.GFS
  ( gfsRemove
  ) where

import GFS.Internal.Checkpoints
import GFS.Internal.TimeList

import Data.List (uncons)
import Data.List.NonEmpty ((<|), NonEmpty(..))
import Data.Time.LocalTime
import qualified Data.List.NonEmpty as NE
import Data.Maybe

gfsRemove :: Checkpoints -> TimeList -> TimeList
gfsRemove checkpoints times =
  -- "to remove" means "to return times from this function"
  let tooOld :| timesBetweenCheckpoints = (unTimeList times) `splitAtCheckpoints` (unCheckpoints checkpoints)
      keepOldest = drop 1
      -- TODO should we require uniqueness of time values?
      -- TODO concatenating the filtered sorted times in correct order always produces a sorted
      -- list — is it possible to explain this to the type system?
  in mkTimeList $
      -- everything older than the oldest checkpoint is removed
      tooOld
      -- for every list of times between a pair of checkpoints, only the oldest
      -- time is kept, everything else is removed; also, the newest time (before
      -- now) is always kept
      <> concatMap keepOldest (keepNewest $ removeEmpty timesBetweenCheckpoints)

-- | Removes the newest time from the lists of times considered for cleanup. The
-- times are expected to be ordered from older to newer through the lists.
keepNewest :: [NonEmpty TimeItem] -> [[TimeItem]]
keepNewest = reverse . onHead dropLast . reverse
  where
    onHead f xs = case uncons xs of
      Just (x, rest) -> f x : fmap NE.toList rest
      Nothing -> []

    dropLast = NE.init

splitAtCheckpoints :: [TimeItem] -> NonEmpty LocalTime -> NonEmpty [TimeItem]
splitAtCheckpoints xs checkpoints =
  let (checkpoint, maybeOtherCheckpoints) = NE.uncons checkpoints
      (beforeCheckpoint, rest) = span ((< checkpoint) . itTime) xs
  in beforeCheckpoint <|
    {- note: the last block of times (times after the last checkpoint, "now") is
     - ignored (`rest` is unused) and never returned to `gfsRemove`, thus the times
     - are never cleaned, which is the expected behavior
     -}
    maybe (NE.singleton []) (splitAtCheckpoints rest) maybeOtherCheckpoints

-- | Remove empty lists from a list of lists.
removeEmpty :: [[a]] -> [NonEmpty a]
removeEmpty = mapMaybe NE.nonEmpty
