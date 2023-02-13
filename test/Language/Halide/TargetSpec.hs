module Language.Halide.TargetSpec (spec) where

import Language.Halide
import Test.Hspec

spec :: Spec
spec = do
  describe "setFeature" $ do
    it "adds features to JIT targets" $ do
      setFeature FeatureCUDA hostTarget `shouldSatisfy` hasGpuFeature