module GFS.Internal.GFSRange
  ( GFSRange(..)
  , GFSRanges
  , Now
  , applyRange
  , applyRanges
  , mkGFSRanges
  , unGFSRanges
  ) where

import GFS.Internal.Checkpoints
import GFS.Internal.TimeInterval

import Data.List (foldl', intercalate, sort)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe
import Data.Time.LocalTime
import qualified Data.List.NonEmpty as NE

-- | A single range for the GFS algorithm; it's used to calculate checkpoints relative
-- to "now"'s midnight by going back in time by `rStep` from a start time as many times as
-- necessary until the `rLimit` from `"now"'s midnight.
data GFSRange = GFSRange
  { rStep :: !TimeInterval
  , rLimit :: !TimeInterval
  }
  deriving Eq

instance Show GFSRange where
  show (GFSRange step limit) = show step <> ":" <> show limit

-- | A non-empty list of `GFSRange`s sorted by the limit.
newtype GFSRanges = GFSRanges { unGFSRanges :: NonEmpty GFSRange }
  deriving Eq

instance Show GFSRanges where
  show (GFSRanges rs) = intercalate "," . NE.toList $ fmap show rs

mkGFSRanges :: GFSRange -> [GFSRange] -> GFSRanges
mkGFSRanges range = GFSRanges . NE.sortWith rLimit . (range :|)

type StartTime = LocalTime
type Now = LocalTime
type StartOfDayNow = LocalTime
type TotalEndTime = LocalTime

applyRange :: TotalEndTime -> GFSRange -> StartOfDayNow -> StartTime -> [LocalTime]
applyRange totalEndTime range@(GFSRange step _) startOfDayNow startTime
  = sort
  . filter (>= totalEndTime)
  .  (if shouldAddEndTime then (endTime :) else id)
  $ times
  where
    shouldAddEndTime = endTime < startTime
    endTime = getEndTime range startOfDayNow
    times = takeWhile (> endTime)
      $ flip subTimeInterval startTime . flip scaleTimeInterval step <$> [1..]

getEndTime :: GFSRange -> StartOfDayNow -> LocalTime
getEndTime (GFSRange _ limit) = subTimeInterval limit

applyRanges :: GFSRanges -> Now -> Checkpoints
applyRanges (GFSRanges ranges) now = mkCheckpoints now times
  where
    startOfDayNow = atMidnight now
    totalEndTime = getEndTime (NE.last ranges) startOfDayNow
    times = snd $ foldl' computeRanges (startOfDayNow, [startOfDayNow]) ranges
    computeRanges (startTime, accCheckpoints) range =
      let checkpoints = applyRange totalEndTime range startOfDayNow startTime
          newStartTime = fromMaybe startTime $ maybeHead checkpoints
      in (newStartTime, checkpoints <> accCheckpoints)

atMidnight :: LocalTime -> LocalTime
atMidnight t = t { localTimeOfDay = midnight }

maybeHead :: [a] -> Maybe a
maybeHead = listToMaybe
