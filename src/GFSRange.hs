module GFSRange
  ( GFSRange(..)
  , applyRange
  ) where

import Data.Time.LocalTime

-- | A single range for the GFS algorithm; it's used to calculate checkpoints relative
-- to "now" by going back in time by `rStep` from a start time as many times as
-- necessary until the `rLimit` from `"now".
-- `CalendarDiffTime` (instead of `NominalDiffTime`) is required to support
-- calendar-based time calculations.
data GFSRange = GFSRange
  { rStep :: CalendarDiffTime
  , rLimit :: CalendarDiffTime
  }
  deriving Show

applyRange :: GFSRange -> LocalTime -> [LocalTime]
applyRange _ _ = []
