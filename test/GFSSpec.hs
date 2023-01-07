module GFSSpec where

import GFS

import Data.Time.Calendar.OrdinalDate
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "gfsRemove" $ do
    it "returns nothing for empty times" $
      property $ \(ALocalTime now) ->
        gfsRemove now [] == []

-- | Newtype wrapper for `LocalTime` in order to implement the `Arbitrary` instance.
newtype ALocalTime = ALocalTime LocalTime
  deriving Show

instance Arbitrary ALocalTime where
  arbitrary = do
    -- TODO use random year
    day <- fromOrdinalDate <$> pure 2023 <*> chooseInt (1, 365)
    -- TODO use random second
    time <- TimeOfDay <$> chooseInt (0, 23) <*> chooseInt (0, 59) <*> pure 0
    pure . ALocalTime $ LocalTime day time
