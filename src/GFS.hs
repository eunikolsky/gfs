module GFS
  ( gfsRemove
  ) where

import Data.Time.Clock
import Data.Time.LocalTime

-- TODO use `newtype`
type Now = LocalTime

-- TODO use `newtype`
type Offset = NominalDiffTime

-- TODO use `Sorted LocalTime`?
gfsRemove :: Now -> Offset -> [LocalTime] -> [LocalTime]
gfsRemove now offset times =
  -- "remove" means to return times from the function
  let removeBeforeTime = addLocalTime (negate offset) now
  in filter (< removeBeforeTime) times
