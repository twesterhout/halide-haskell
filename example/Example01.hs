{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Language.Halide.Internal

main :: IO ()
main = do
  i <- mkVar "i"
  f <- define "f" i $ 2 * cast @Float i + 1
  g <- define "g" i $ 1 + f ! i
  printLoopNest g
  print =<< realize1D g 10
  print =<< realize1D g 10