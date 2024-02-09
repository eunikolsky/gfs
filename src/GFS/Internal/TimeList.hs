module GFS.Internal.TimeList
  ( TimeItem(..)
  , TimeList
  , mkTimeList
  , unTimeList
  ) where

import Data.List (sortOn)
import Data.Text (Text)
import Data.Time.LocalTime

-- | A single item provided by the user, consisting of the original string and
-- the time parsed from it. The original string is needed in order to print it
-- as is for GFS-removed times.
data TimeItem = TimeItem
  { itStr :: Text
  , itTime :: LocalTime
  }
  deriving (Eq, Show)

-- | Sorted (oldest to newest) list of `TimeItem` values.
newtype TimeList = TimeList { unTimeList :: [TimeItem] }
  deriving (Eq, Show)

-- | Smart constructor for `TimeList` — sorts the input list if necessary.
mkTimeList :: [TimeItem] -> TimeList
mkTimeList = TimeList . sortOn itTime
