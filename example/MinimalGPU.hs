module Main (main) where

import Control.Monad (unless, void)
import Language.Halide

main :: IO ()
main = do
  [x, y, xo, xi, yo, yi] <- mapM mkVar ["x", "y", "xo", "xi", "yo", "yi"]
  f <- define "f" (x, y) $ x + y

  void $
    split TailAuto x (xo, xi) 16 f
      >>= split TailAuto y (yo, yi) 16
      >>= reorder [xi, yi, xo, yo]
      >>= gpuBlocks DeviceDefaultGPU (xo, yo)
      >>= gpuThreads DeviceDefaultGPU (xi, yi)

  case gpuTarget of
    Nothing -> putStrLn "no GPU target found; skipping ..."
    Just target -> do
      r <- realizeOnTarget (setFeature FeatureDebug target) f [32, 32] peekToList
      let expected = [[i + j | i <- [0 .. 31]] | j <- [0 .. 31]]
      unless (r == expected) . error $
        "wrong result:"
          <> "\n          got: "
          <> show r
          <> ",\n but expected: "
          <> show expected
