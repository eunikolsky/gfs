module GFS.Internal.GFS
  ( gfsRemove
  ) where

import GFS.Internal.Checkpoints
import GFS.Internal.TimeList

import Data.List.NonEmpty ((<|), NonEmpty(..))
import Data.Time.LocalTime
import qualified Data.List.NonEmpty as NE

gfsRemove :: Checkpoints -> TimeList -> TimeList
gfsRemove checkpoints times =
  -- "to remove" means "to return times from this function"
  let tooOld :| timesBetweenCheckpoints = (unTimeList times) `splitAtCheckpoints` (unCheckpoints checkpoints)
      keepOldest = drop 1
      -- TODO should we require uniqueness of time values?
      -- TODO concatenating the filtered sorted times in correct order always produces a sorted
      -- list — is it possible to explain this to the type system?
  in mkTimeList $ tooOld ++ concatMap keepOldest timesBetweenCheckpoints

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
