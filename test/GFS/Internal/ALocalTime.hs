module GFS.Internal.ALocalTime where

import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Test.QuickCheck

-- | Newtype wrapper for `LocalTime` in order to implement the `Arbitrary` instance.
newtype ALocalTime = ALocalTime { unALocalTime :: LocalTime }
  deriving Show

instance Arbitrary ALocalTime where
  arbitrary = do
    -- TODO use random year
    day <- fromOrdinalDate <$> pure 2023 <*> chooseInt (1, 365)
    -- TODO use random second
    time <- TimeOfDay <$> chooseInt (0, 23) <*> chooseInt (0, 59) <*> pure 0
    pure . ALocalTime $ LocalTime day time
  -- TODO try implementing `shrink`
