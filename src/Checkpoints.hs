module Checkpoints
  ( Checkpoints
  , mkCheckpoints
  , mkSingletonCheckpoint
  , oldestCheckpointsTime
  , unCheckpoints
  ) where

-- TODO hide `unCheckpoints` and provide higher-level functions instead

import Data.Time.LocalTime
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE

-- | Non-empty, sorted (oldest to newest) list of unique `LocalTime` values, used to
-- specify boundary points in time where the next available time should be kept.
newtype Checkpoints = Checkpoints { unCheckpoints :: NonEmpty LocalTime }
  deriving (Eq, Show)

mkSingletonCheckpoint :: LocalTime -> Checkpoints
mkSingletonCheckpoint = Checkpoints . NE.singleton

mkCheckpoints :: LocalTime -> [LocalTime] -> Checkpoints
mkCheckpoints one = Checkpoints . NE.nub . NE.sort . (one :|)

oldestCheckpointsTime :: Checkpoints -> LocalTime
oldestCheckpointsTime = NE.head . unCheckpoints
