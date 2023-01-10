module CheckpointsSpec where

import ALocalTime
import Checkpoints

import Test.Hspec
import Test.QuickCheck
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec = do
  describe "offsetsToCheckpoints" $ do
    it "always includes now at the end" $
      property $ \(ALocalTime now) ->
        let actual = offsetsToCheckpoints now
        in NE.last (unCheckpoints actual) == now
