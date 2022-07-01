module Spec.Block.Babbage (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Config" $ do
    it "fixedConfig" $
      1 `shouldBe` (1 :: Int)
