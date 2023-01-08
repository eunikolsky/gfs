module GFS
  ( gfsRemove
  , mkTimeList
  ) where

import Data.List (sort, uncons)
import Data.Time.Clock
import Data.Time.LocalTime

-- TODO use `newtype`
type Now = LocalTime

-- TODO use `newtype`
type Offset = NominalDiffTime

gfsRemove :: Now -> Offset -> TimeList -> TimeList
gfsRemove now offset (TimeList times) =
  -- "to remove" means "to return times from this function"
  let removeBeforeTime = addLocalTime (negate offset) now
      (tooOld, newerThanOffset) = span (< removeBeforeTime) times
      -- TODO if the input is always sorted, only one pass through `times` is always enough
      unnecessaryNewerThanOffset = case uncons newerThanOffset of
        Just (_oldestInNewer, xs) -> xs
        Nothing -> []
  -- TODO should we require uniqueness of time values?
  -- TODO concatenating the filtered sorted times in correct order always produces a sorted
  -- list — is it possible to explain this to the type system?
  in mkTimeList $ tooOld ++ unnecessaryNewerThanOffset

-- TODO extract to a new module?
-- | Sorted (oldest to newest) list of `LocalTime` values.
newtype TimeList = TimeList { unTimeList :: [LocalTime] }
  deriving (Eq, Show)

-- | Smart constructor for `TimeList` — sorts the input list if necessary.
mkTimeList :: [LocalTime] -> TimeList
mkTimeList = TimeList . sort
