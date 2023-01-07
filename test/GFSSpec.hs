module GFSSpec where

import GFS

import Data.Time.Calendar.OrdinalDate
import Data.Time.Clock
import Data.Time.LocalTime
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "gfsRemove" $ do
    it "returns nothing for empty times" $
      property $ \(ALocalTime now) (AOffset offset) ->
        gfsRemove now offset [] == []

    describe "given fixed 1 hour offset" $ do
      let offset = 60 * 60 :: NominalDiffTime

      it "returns all times older than 1 hour" $
        property $ \(ALocalTime now) -> do
          -- all times are older than 1 hour
          -- TODO extract the generator?
          times <- fmap (flip addLocalTime now . negate . (+ offset) . fromInteger) <$> listOf (chooseInteger (0, 9000))
          pure . counterexample ("times: " ++ show times) $ gfsRemove now offset times == times

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

newtype AOffset = AOffset NominalDiffTime
  deriving Show

instance Arbitrary AOffset where
  arbitrary = AOffset . fromInteger <$> chooseInteger (0, 86400)
