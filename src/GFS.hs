module GFS
  ( gfsRemove

  -- re-exports
  , GFSRange(..)
  , mkGFSRanges
  , mkTimeInterval
  , mkTimeList
  ) where

import GFS.Internal.GFSRange
import GFS.Internal.TimeInterval
import GFS.Internal.TimeList
import qualified GFS.Internal.GFS as Internal

gfsRemove :: GFSRanges -> Now -> TimeList -> TimeList
gfsRemove ranges now times = keepNewestTime $ Internal.gfsRemove checkpoints times
  where
    checkpoints = applyRanges ranges now
