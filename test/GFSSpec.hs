module GFSSpec where

import GFS

import Control.Monad
import Data.List (singleton)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE

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

    describe "given multiple offsets" $ do
      it "returns all times older than the oldest offset" $
        property $ \(ALocalTime now) -> do
          checkpoints <- chooseCheckpoints now
          let oldestCheckpoint = NE.head . unCheckpoints $ checkpoints
          times <- chooseTimesOlderThan oldestCheckpoint
          verifyRemoved checkpoints times times

      it "returns nothing for single times in each checkpoint range" $
        property $ \(ALocalTime now) -> do
          checkpoints <- chooseCheckpoints now
          let checkpointPairs = adjacentPairs . unCheckpoints $ checkpoints
          times <- chooseSingleTimeInEachRange checkpointPairs
          verifyRemoved checkpoints times (mkTimeList [])

      it "returns nothing for (keeps) times exactly at the checkpoints" $
        property $ \(ALocalTime now) -> do
          checkpoints <- chooseCheckpoints now
          let times = timeListFromCheckpoints checkpoints
          verifyRemoved checkpoints times (mkTimeList [])

verifyRemoved :: Checkpoints -> TimeList -> TimeList -> Gen Property
verifyRemoved checkpoints times expected =
  let cleaned = gfsRemove checkpoints times
  in pure . counterexample (mconcat
      [ "checkpoints: ", show checkpoints
      , "\ntimes: ", show times
      , "\ncleaned: ", show cleaned
      ]
    ) $ cleaned == expected

chooseCheckpoints :: LocalTime -> Gen Checkpoints
chooseCheckpoints now = do
  let chooseOffset = fromInteger <$> chooseInteger (1, 9000)
  offset <- chooseOffset
  offsets <- listOf chooseOffset
  pure $ mkCheckpoints (now `subLocalTime` offset) (subLocalTime now <$> offsets)

chooseTimesOlderThan :: LocalTime -> Gen TimeList
chooseTimesOlderThan t = do
  offsets <- listOf $ fromInteger <$> chooseInteger (1, 9000)
  pure . mkTimeList $ subLocalTime t <$> offsets

chooseSingleTimeInEachRange :: [(LocalTime, LocalTime)] -> Gen TimeList
chooseSingleTimeInEachRange ranges = fmap mkTimeList $
  forM ranges $ \(from, to) -> do
    let diff = to `diffLocalTime` from
    fromOffset <- fromInteger <$> chooseInteger (0, floor diff - 1)
    pure $ addLocalTime fromOffset from

timeListFromCheckpoints :: Checkpoints -> TimeList
timeListFromCheckpoints = mkTimeList . NE.toList . unCheckpoints

subLocalTime :: LocalTime -> NominalDiffTime -> LocalTime
subLocalTime t = flip addLocalTime t . negate

adjacentPairs :: NonEmpty a -> [(a, a)]
adjacentPairs xs = zip (NE.toList xs) (NE.tail xs)

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
