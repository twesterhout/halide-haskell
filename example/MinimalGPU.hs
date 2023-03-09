module Main (main) where

import Control.Monad (void, when)
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

  let target = setFeature FeatureOpenCL . setFeature FeatureDebug $ hostTarget

  realizeOnTarget target f [32, 32] $ \buf -> do
    pure ()
