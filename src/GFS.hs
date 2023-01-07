module GFS
  ( gfsRemove
  ) where

import Data.Time.LocalTime

-- TODO use `newtype`
type Now = LocalTime

-- TODO use `Sorted LocalTime`?
gfsRemove :: Now -> [LocalTime] -> [LocalTime]
gfsRemove _ _ = []
