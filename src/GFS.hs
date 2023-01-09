module GFS
  ( gfsRemove

  , Checkpoints
  , TimeList
  , mkCheckpoints
  , mkSingletonCheckpoint
  , mkTimeList
  , unCheckpoints
  ) where

import Data.List (sort)
import Data.List.NonEmpty ((<|), NonEmpty(..))
import Data.Time.LocalTime
import qualified Data.List.NonEmpty as NE

gfsRemove :: Checkpoints -> TimeList -> TimeList
gfsRemove (Checkpoints checkpoints) (TimeList times) =
  -- "to remove" means "to return times from this function"
  let tooOld :| newerThanOffset = times `splitAtCheckpoints` checkpoints
      keepOldest = drop 1
      -- TODO should we require uniqueness of time values?
      -- TODO concatenating the filtered sorted times in correct order always produces a sorted
      -- list — is it possible to explain this to the type system?
  in mkTimeList $ tooOld ++ concatMap keepOldest newerThanOffset

splitAtCheckpoints :: Ord a => [a] -> NonEmpty a -> NonEmpty [a]
splitAtCheckpoints xs checkpoints =
  let (checkpoint, maybeOtherCheckpoints) = NE.uncons checkpoints
      (beforeCheckpoint, rest) = span (< checkpoint) xs
  in beforeCheckpoint <|
    {- note: the last block of times (times after the last checkpoint, "now") is
     - ignored (`rest` is unused) and never returned to `gfsRemove`, thus the times
     - are never cleaned, which is the expected behavior
     -}
    maybe (NE.singleton []) (splitAtCheckpoints rest) maybeOtherCheckpoints

-- TODO extract to a new module
-- | Non-empty, sorted (oldest to newest) list of unique `LocalTime` values, used to
-- specify boundary points in time where the next available time should be kept.
newtype Checkpoints = Checkpoints { unCheckpoints :: NonEmpty LocalTime }
  deriving Show

mkSingletonCheckpoint :: LocalTime -> Checkpoints
mkSingletonCheckpoint = Checkpoints . NE.singleton

mkCheckpoints :: LocalTime -> [LocalTime] -> Checkpoints
mkCheckpoints one = Checkpoints . NE.nub . NE.sort . (one :|)

-- TODO extract to a new module
-- | Sorted (oldest to newest) list of `LocalTime` values.
newtype TimeList = TimeList { unTimeList :: [LocalTime] }
  deriving (Eq, Show)

-- | Smart constructor for `TimeList` — sorts the input list if necessary.
mkTimeList :: [LocalTime] -> TimeList
mkTimeList = TimeList . sort
