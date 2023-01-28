{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Int
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide.Internal

typedExample :: IO ()
typedExample = do
  i <- mkVar "i"
  f <- define "f" i $ 2 * cast @Float i + 1
  g <- define "g" i $ 1 + f ! i
  printLoopNest g
  print =<< realize1D g 10

main :: IO ()
main = do
  typedExample