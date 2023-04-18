{-# LANGUAGE DataKinds #-}

module Main (main) where

import ArrayFire qualified as AF
import Language.Halide
import Language.Halide.ArrayFire
import Test.Hspec

main :: IO ()
main =
  hspec $
    describe "IsHalideBuffer" $ do
      it "supports CPU arrays" $ do
        let arr = 1 + AF.moddims (AF.range @Double [6] (-1)) [2, 3]
        -- print arr
        -- print =<< AF.getManualEvalFlag
        withHalideBuffer @2 @Double arr peekToList `shouldReturn` [[1, 3, 5], [2, 4, 6]]
