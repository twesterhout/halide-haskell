module Language.Halide.BoundaryConditionsSpec (spec) where

import Language.Halide
import Test.Hspec

spec :: Spec
spec = do
  it "extends the domain by repeating the edges" $ do
    let mat :: [[Float]]
        mat = [[1, 2, 3], [4, 5, 6]]
    asBufferParam @2 @Float mat $ \mat' -> do
      [i, j] <- mapM mkVar ["i", "j"]
      extended <- repeatEdge mat'
      f <- define "f" (i, j) $ extended ! (i - 1, j - 1)
      realize f [4, 5] peekToList
        `shouldReturn` [[1, 1, 2, 3, 3], [1, 1, 2, 3, 3], [4, 4, 5, 6, 6], [4, 4, 5, 6, 6]]
  it "extends the domain by filling with a constant value" $ do
    let mat :: [[Float]]
        mat = [[1, 2, 3], [4, 5, 6]]
    asBufferParam @2 @Float mat $ \mat' -> do
      [i, j] <- mapM mkVar ["i", "j"]
      extended <- constantExterior 0 mat'
      f <- define "f" (i, j) $ extended ! (i - 1, j - 1)
      realize f [4, 5] peekToList
        `shouldReturn` [[0, 0, 0, 0, 0], [0, 1, 2, 3, 0], [0, 4, 5, 6, 0], [0, 0, 0, 0, 0]]
