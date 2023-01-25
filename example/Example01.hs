{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Int
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide.Internal

main :: IO ()
main = do
  i <- mkVar "i"
  f <- mkFunc (Just "f")
  ref <- applyFunc f [i]
  defineFunc ref $ i + i + mkExpr (2 :: Int32)
  printLoopNest f

  mv <- SM.replicate 10 (0 :: Int32)
  realizeOnBuffer f mv
  print =<< S.unsafeFreeze mv
