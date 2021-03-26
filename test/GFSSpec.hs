module GFSSpec where

import GFS

import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "cleanup" $
    it "returns nothing for empty input" $
      cleanup [] `shouldBe` []
