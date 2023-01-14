module GFSRange
  ( GFSRange(..)
  , GFSRanges
  , applyRange
  , applyRanges
  , mkGFSRanges
  , unGFSRanges
  ) where

import Checkpoints
import TimeInterval

import Data.List.NonEmpty (NonEmpty(..))
import Data.Time.LocalTime
import qualified Data.List.NonEmpty as NE

-- | A single range for the GFS algorithm; it's used to calculate checkpoints relative
-- to "now" by going back in time by `rStep` from a start time as many times as
-- necessary until the `rLimit` from `"now".
data GFSRange = GFSRange
  { rStep :: !TimeInterval
  , rLimit :: !TimeInterval
  }
  deriving Show

-- | A non-empty list of `GFSRange`s sorted by the limit.
newtype GFSRanges = GFSRanges { unGFSRanges :: NonEmpty GFSRange }
  deriving Show

mkGFSRanges :: GFSRange -> [GFSRange] -> GFSRanges
mkGFSRanges range = GFSRanges . NE.sortWith rLimit . (range :|)

type StartTime = LocalTime
type Now = LocalTime

applyRange :: GFSRange -> Now -> StartTime -> Checkpoints
applyRange range@(GFSRange step _) now startTime = mkCheckpoints endTime times
  where
    endTime = getEndTime range now
    times = takeWhile (> endTime)
      $ flip subTimeInterval startTime . flip scaleTimeInterval step <$> [1..]

getEndTime :: GFSRange -> Now -> LocalTime
getEndTime (GFSRange _ limit) = subTimeInterval limit

applyRanges :: GFSRanges -> Now -> StartTime -> Checkpoints
applyRanges (GFSRanges ranges) now startTime = mkCheckpoints startTime [endTime]
  where endTime = getEndTime (NE.last ranges) now
