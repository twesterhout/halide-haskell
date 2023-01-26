{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Data.Int
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide.Internal

typedExample :: IO ()
typedExample = do
  (i :: TypedExpr Int32) <- TypedExpr <$> mkVar "i"
  f <- define "f" i $ i + i + 2
  g@(TypedFunc func) <- define "g" i $ 1 + f ! i
  printLoopNest func

  mv <- SM.replicate 10 (0 :: Int32)
  realizeTypedOnBuffer1D g mv
  print =<< S.unsafeFreeze mv

untypedExample :: IO ()
untypedExample = do
  i <- mkVar "i"
  f <- mkFunc (Just "f")
  ref <- applyFunc f [i]
  defineFunc ref $ i + i + mkExpr (2 :: Int32)
  printLoopNest f

  mv <- SM.replicate 10 (0 :: Int32)
  realizeOnBuffer f mv
  print =<< S.unsafeFreeze mv

main :: IO ()
main = do
  untypedExample
  typedExample