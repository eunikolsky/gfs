module GFSSpec where

import GFS

import Data.List (singleton)
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "gfsRemove" $ do
    it "returns nothing for empty times" $
      property $ \(ALocalTime now) ->
        let noTimes = mkTimeList []
            checkpoints = mkSingletonCheckpoint now
        in gfsRemove checkpoints noTimes == noTimes

    describe "given fixed 1 hour offset" $ do
      let offset' = 60 * 60 :: Integer
          offset = fromInteger offset' :: NominalDiffTime

      it "returns all times older than 1 hour" $
        property $ \(ALocalTime now) -> do
          -- all times are older than 1 hour
          -- TODO extract the generator?
          times <- mkTimeList . fmap (flip addLocalTime now . negate . (+ offset) . fromInteger) <$> listOf (chooseInteger (1, 9000))
          let checkpoints = mkCheckpoints (subLocalTime now offset) [now]
              cleaned = gfsRemove checkpoints times
          pure . counterexample ("times: " <> show times <> "\ncleaned: " <> show cleaned) $ cleaned == times

      it "returns nothing for one time newer than 1 hour" $
        property $ \(ALocalTime now) -> do
          oneTime <- mkTimeList . singleton . flip addLocalTime now . negate . fromInteger <$> chooseInteger (1, offset' - 1)
          let checkpoints = mkCheckpoints (subLocalTime now offset) [now]
          pure . counterexample ("oneTime: " <> show oneTime) $ gfsRemove checkpoints oneTime == mkTimeList []

      it "returns nothing for (keeps) time exactly 1 hour older" $
        property $ \(ALocalTime now) ->
          let oneTime = mkTimeList . singleton $ addLocalTime (negate offset) now
              checkpoints = mkCheckpoints (subLocalTime now offset) [now]
          in gfsRemove checkpoints oneTime == mkTimeList []

      it "returns all times except oldest" $
        property $ \(ALocalTime now) -> do
          oldestTimeOffset <- chooseInteger (offset' `div` 2, offset' - 1)
          let oldestTime = addLocalTime (negate . fromInteger $ oldestTimeOffset) now
          -- TODO is it possible to encapsulate and hide the offset subtractions so that they are always positive in the properties?
          times <- fmap (flip addLocalTime now . negate . fromInteger) <$> listOf (chooseInteger (1, oldestTimeOffset))
          let inputTimes = mkTimeList $ oldestTime : times
              checkpoints = mkCheckpoints (subLocalTime now offset) [now]
              cleaned = gfsRemove checkpoints inputTimes
          pure . counterexample ("input times: " <> show inputTimes <> "\ncleaned: " <> show cleaned) $ cleaned == mkTimeList times

subLocalTime :: LocalTime -> NominalDiffTime -> LocalTime
subLocalTime t = flip addLocalTime t . negate

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
  -- TODO try implementing `shrink`
