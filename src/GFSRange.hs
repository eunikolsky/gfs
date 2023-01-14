module GFSRange
  ( GFSRange(..)
  , applyRange
  ) where

import Checkpoints
import TimeInterval

import Data.Time.LocalTime

-- | A single range for the GFS algorithm; it's used to calculate checkpoints relative
-- to "now" by going back in time by `rStep` from a start time as many times as
-- necessary until the `rLimit` from `"now".
data GFSRange = GFSRange
  { rStep :: !TimeInterval
  , rLimit :: !TimeInterval
  }
  deriving Show

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
