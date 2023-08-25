{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ArrayFire qualified as AF
import Language.Halide
import Language.Halide.ArrayFire
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "IsHalideBuffer" $
      it "supports CPU arrays" $ do
        let arr = 1 + AF.moddims (AF.range @Double [6] (-1)) [2, 3]
        -- print arr
        -- print =<< AF.getManualEvalFlag
        withHalideBuffer @2 @Double arr peekToList `shouldReturn` [[1, 3, 5], [2, 4, 6]]
    describe "produceShapedArray" $
      it "produces ShapedArrays" $ do
        kernel <- compile $ do
          i <- mkVar "i"
          j <- mkVar "j"
          define "dest" (i, j) (i + j)
        produceShapedArray [3, 2] kernel
          `shouldReturn` mkShapedArray [3, 2] (AF.mkArray @Int32 [6] [0, 1, 2, 1, 2, 3])
