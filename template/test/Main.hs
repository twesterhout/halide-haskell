module Main (main) where

import Language.Halide
import Playground
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "awesomeSequence" $ do
    it "compiles the kernel & generates a sequence" $ do
      kernel <- awesomeSequence
      allocaCpuBuffer [10] $ \buf -> do
        kernel buf
        peekToList buf `shouldReturn` [x * x | x <- [0 .. 9]]
