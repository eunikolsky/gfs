module GFS
  ( gfsRemove

  -- re-exports
  , GFSRange(..)
  , GFSRanges
  , Now
  , TimeInterval
  , TimeList
  , addTimeInterval
  , mkGFSRanges
  , mkTimeInterval
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
import Data.Text (Text)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T

gfsRemove :: MonadLogger m => GFSRanges -> Now -> TimeList -> m TimeList
gfsRemove ranges now times = do
  mapM_ logDebugN
    [ "now: " <> T.pack (show now)
    , "ranges: " <> (T.intercalate ", " . NE.toList . fmap showRange . unGFSRanges $ ranges)
    ]
  let checkpoints = applyRanges ranges now
  logDebugN $ "checkpoints: " <> (T.pack . intercalate ", " . NE.toList . fmap show . unCheckpoints $ checkpoints)
  pure . keepNewestTime $ Internal.gfsRemove checkpoints times

-- this is another confusing thing for a beginner in Haskell: `String` is highly discouraged,
-- yet `Show` produces a `String`; one can implement a custom "show" returning `Text`,
-- but there is no standard typeclass to be able to use e.g. `show'` on a conforming
-- value and generate the instance automatically; now every such `show'` has to have
-- a distinct name
showRange :: GFSRange -> Text
showRange (GFSRange step limit) = mconcat ["GFSRange{ ", showTimeInterval step, showTimeInterval limit, " }"]
