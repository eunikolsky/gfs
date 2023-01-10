module CheckpointsSpec where

import ALocalTime
import Checkpoints

import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
  describe "offsetsToCheckpoints" $ do
    it "always includes now at the end" $
      property $ \(ALocalTime now) ->
        let actual = offsetsToCheckpoints now []
        in NE.last (unCheckpoints actual) == now

    it "converts offsets to times in the past of now" $
      property . forAll chooseOffsets $ \((ALocalTime now), offsets, checkpoints) ->
        let actual = offsetsToCheckpoints now offsets
        in counterexample ("actual: " <> show actual) $ actual == checkpoints

chooseOffsets :: Gen (ALocalTime, [NominalDiffTime], Checkpoints)
chooseOffsets = do
  count <- chooseInt (2, 20)
  times <- vectorOf count $ unALocalTime <$> arbitrary
  let now = maximum times
      rest = filter (/= now) times
      offsets = fmap (now `diffLocalTime`) rest
  pure (ALocalTime now, offsets, mkCheckpoints now rest)
