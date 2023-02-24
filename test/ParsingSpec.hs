module ParsingSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "parsing" $ do
    it "parses" $ do
      1 `shouldBe` 1
