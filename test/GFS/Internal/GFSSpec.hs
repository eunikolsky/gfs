module GFS.Internal.GFSSpec where

import GFS.Internal.Checkpoints
import GFS.Internal.GFS
import GFS.Internal.TimeList

import GFS.Internal.ALocalTime

import Control.Monad
import Data.Bifunctor
import Data.List ((\\), foldl', singleton, sort)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
  describe "gfsRemove" $ do
    it "cleans up day 31 properly" $ do
      let checkpoints = mkCheckpoints
            (read "2023-12-03 00:00:00")
            [read "2024-01-03 00:00:00"]
          times = mkTimeList
            [ TimeItem "" $ read "2023-12-11 07:04:00"
            , TimeItem "" $ read "2023-12-23 16:00:00"
            , TimeItem "" $ read "2023-12-31 23:10:00"
            , TimeItem "" $ read "2024-01-02 23:10:00"
            ]
          expected = mkTimeList . drop 1 . init . unTimeList $ times
      gfsRemove checkpoints times `shouldBe` expected

    it "returns nothing for empty times" $
      property $ \(ALocalTime now) ->
        let noTimes = mkTimeList []
            checkpoints = mkSingletonCheckpoint now
        in gfsRemove checkpoints noTimes == noTimes

    it "never removes times after now (last checkpoint)" $
      property $ \(ALocalTime now) -> do
        checkpoints <- chooseCheckpoints now
        times <- chooseTimesAfterNow now
        verifyRemoved checkpoints times (mkTimeList [])

    describe "given fixed 1 hour offset" $ do
      let offset' = 60 * 60 :: Integer
          offset = fromInteger offset' :: NominalDiffTime

      it "returns all times older than 1 hour" $
        property $ \(ALocalTime now) -> do
          -- all times are older than 1 hour
          -- TODO extract the generator?
          times <- mkTimeList . fmap (TimeItem "" . flip addLocalTime now . negate . (+ offset) . fromInteger) <$> listOf (chooseInteger (1, 9000))
          let checkpoints = mkCheckpoints (subLocalTime now offset) [now]
              cleaned = gfsRemove checkpoints times
          pure . counterexample ("times: " <> show times <> "\ncleaned: " <> show cleaned) $ cleaned == times

      it "returns nothing for one time newer than 1 hour" $
        property $ \(ALocalTime now) -> do
          oneTime <- mkTimeList . singleton . TimeItem "" . flip addLocalTime now . negate . fromInteger <$> chooseInteger (1, offset' - 1)
          let checkpoints = mkCheckpoints (subLocalTime now offset) [now]
          pure . counterexample ("oneTime: " <> show oneTime) $ gfsRemove checkpoints oneTime == mkTimeList []

      it "returns nothing for (keeps) time exactly 1 hour older" $
        property $ \(ALocalTime now) ->
          let oneTime = mkTimeList . singleton . TimeItem "" $ addLocalTime (negate offset) now
              checkpoints = mkCheckpoints (subLocalTime now offset) [now]
          in gfsRemove checkpoints oneTime == mkTimeList []

      it "returns all times except oldest and newest" $
        property $ \(ALocalTime now) -> do
          oldestTimeOffset <- chooseInteger (offset' `div` 2, offset' - 2)
          let oldestTime = TimeItem "" $ addLocalTime (negate . fromInteger $ oldestTimeOffset) now
              newestTime = TimeItem "" $ addLocalTime (-1) now
          -- TODO is it possible to encapsulate and hide the offset subtractions so that they are always positive in the properties?
          times <- fmap (TimeItem "" . flip addLocalTime now . negate . fromInteger) <$> listOf (chooseInteger (1, oldestTimeOffset))
          let inputTimes = mkTimeList $ oldestTime : newestTime : times
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

      {-it "returns all times except the oldest in each checkpoint range" $
        property $ \(ALocalTime now) -> do
          checkpoints <- chooseCheckpoints now
          let checkpointPairs = adjacentPairs . unCheckpoints $ checkpoints
          (times, timesWithoutOldest) <- chooseTimesInEachRange checkpointPairs
          verifyRemoved checkpoints times $ timesWithoutOldest-}

      it "is idempotent" $
        property $ \(ALocalTime now) -> do
          checkpoints <- chooseCheckpoints now
          let checkpointPairs = adjacentPairs . unCheckpoints $ checkpoints
          (times, _) <- chooseTimesInEachRange checkpointPairs
          let left = mkTimeList $ unTimeList times \\ unTimeList (gfsRemove checkpoints times)
          verifyRemoved checkpoints left $ mkTimeList []

verifyRemoved :: Checkpoints -> TimeList -> TimeList -> Gen Property
verifyRemoved checkpoints times expected =
  let cleaned = gfsRemove checkpoints times
  in pure . counterexample (mconcat
      [ "checkpoints: ", show checkpoints
      , "\ntimes: ", show times
      , "\nexpected: ", show expected
      , "\nactual: ", show cleaned
      ]
    ) $ cleaned == expected

chooseCheckpoints :: LocalTime -> Gen Checkpoints
chooseCheckpoints now = do
  let chooseOffset = fromInteger <$> chooseInteger (1, 9000)
  offsets <- listOf chooseOffset
  pure $ mkCheckpoints now (subLocalTime now <$> offsets)

chooseTimesAfterNow :: LocalTime -> Gen TimeList
chooseTimesAfterNow now = do
  offsets <- listOf $ fromInteger <$> chooseInteger (1, 9000)
  pure . mkTimeList $ TimeItem "" . (`addLocalTime` now) <$> offsets

chooseTimesOlderThan :: LocalTime -> Gen TimeList
chooseTimesOlderThan t = do
  offsets <- listOf $ fromInteger <$> chooseInteger (1, 9000)
  pure . mkTimeList $ TimeItem "" . subLocalTime t <$> offsets

chooseSingleTimeInEachRange :: [(LocalTime, LocalTime)] -> Gen TimeList
chooseSingleTimeInEachRange ranges = fmap mkTimeList $
  forM ranges $ \(from, to) -> do
    let diff = to `diffLocalTime` from
    fromOffset <- fromInteger <$> chooseInteger (0, floor diff - 1)
    pure . TimeItem "" $ addLocalTime fromOffset from

chooseTimesInEachRange :: [(LocalTime, LocalTime)] -> Gen (TimeList, TimeList)
chooseTimesInEachRange ranges = fmap combineTimes .
  forM ranges $ \(from, to) -> do
    let diff = to `diffLocalTime` from
    fromOffsets <- fmap fromInteger <$> listOf1 (chooseInteger (0, floor diff - 1))
    let times = sort $ (`addLocalTime` from) <$> fromOffsets
    pure (times, dropOldest times)

  where
    dropOldest = drop 1
    combineTimes :: [([LocalTime], [LocalTime])] -> (TimeList, TimeList)
    combineTimes = bimap mkTimeList' mkTimeList' . foldl' (\(acctimes, acctimes') (times, times') -> (acctimes ++ times, acctimes' ++ times')) ([], [])
    mkTimeList' = mkTimeList . fmap (TimeItem "")

timeListFromCheckpoints :: Checkpoints -> TimeList
timeListFromCheckpoints = mkTimeList . NE.toList . fmap (TimeItem "") . unCheckpoints

subLocalTime :: LocalTime -> NominalDiffTime -> LocalTime
subLocalTime t = flip addLocalTime t . negate

adjacentPairs :: NonEmpty a -> [(a, a)]
adjacentPairs xs = zip (NE.toList xs) (NE.tail xs)
