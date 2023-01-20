module GFS.Internal.TimeList
  ( TimeList
  , mkTimeList
  , keepNewestTime
  , unTimeList
  ) where

import Data.List (sort)
import Data.Time.LocalTime

-- | Sorted (oldest to newest) list of `LocalTime` values.
newtype TimeList = TimeList { unTimeList :: [LocalTime] }
  deriving (Eq, Show)

-- | Smart constructor for `TimeList` — sorts the input list if necessary.
mkTimeList :: [LocalTime] -> TimeList
mkTimeList = TimeList . sort

keepNewestTime :: TimeList -> TimeList
keepNewestTime times = case times of
  TimeList [] -> times
  xs -> TimeList . init . unTimeList $ xs
