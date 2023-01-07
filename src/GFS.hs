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
gfsRemove _ _ [] = []
gfsRemove _ _ times = times
