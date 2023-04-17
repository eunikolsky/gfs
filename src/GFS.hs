module GFS
  ( gfsRemove

  -- re-exports
  , GFSRange(..)
  , GFSRanges
  , Now
  , TimeInterval
  , TimeItem(..)
  , TimeList
  , mkGFSRanges
  , mkTimeIntervalHours
  , mkTimeIntervalMonths
  , mkTimeList
  , unTimeList
  ) where

import GFS.Internal.Checkpoints
import GFS.Internal.GFSRange
import GFS.Internal.TimeInterval
import GFS.Internal.TimeList
import qualified GFS.Internal.GFS as Internal

import Control.Monad.Logger
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

gfsRemove :: MonadLogger m => GFSRanges -> Now -> TimeList -> m TimeList
gfsRemove ranges now times = do
  mapM_ logDebugN
    [ "now: " <> T.pack (show now)
    , "ranges: " <> T.pack (show ranges)
    ]
  let checkpoints = applyRanges ranges now
  logDebugN $ "checkpoints: " <> (T.pack . intercalate ", " . NE.toList . fmap show . unCheckpoints $ checkpoints)
  pure . keepNewestTime $ Internal.gfsRemove checkpoints times
