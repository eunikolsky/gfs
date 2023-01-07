module GFS
  ( gfsRemove
  ) where

import Data.Time.Clock
import Data.Time.LocalTime

-- TODO use `newtype`
type Now = LocalTime

-- TODO use `newtype`
type Offset = NominalDiffTime

-- TODO use more specific `LocalTimeList`? unique, sorted times!
gfsRemove :: Now -> Offset -> [LocalTime] -> [LocalTime]
gfsRemove now offset times =
  -- "remove" means to return times from the function
  let removeBeforeTime = addLocalTime (negate offset) now
      (tooOld, newerThanOffset) = span (< removeBeforeTime) times
      -- FIXME partial function!
      -- TODO if the input is always sorted, only one pass through `times` is always enough
      oldestInNewerThanOffset = minimum newerThanOffset
  -- TODO what about duplicates? `/= oldest` would remove multiple values
  in tooOld ++ filter (/= oldestInNewerThanOffset) newerThanOffset
