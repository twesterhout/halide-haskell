{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.ST (RealWorld)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide
import Language.Halide.Target

main :: IO ()
main = do
  testOpenCL
  -- i <- mkVar "i"
  -- f <- define "f" i $ 2 * cast @Float i + 1
  -- g <- define "g" i $ 1 + f ! i
  -- printLoopNest g
  -- print =<< realize1D g 10
  -- print =<< realize1D g 10
  -- print "Compiling kernel ..."
  -- kernel <- testKernel1
  -- (buffer :: SM.MVector RealWorld Float) <- SM.new 5
  -- print "Invoking kernel ..."
  -- withHalideBuffer buffer $ \p ->
  --   kernel Nil p
  -- print =<< S.unsafeFreeze buffer
  kernel <- mkKernel $ \(a :: Expr Float) (g :: Func 1 Float) -> do
    j <- mkVar "j"
    define "func" j $ g ! j / a
  (g :: SM.MVector RealWorld Float) <- SM.generate 5 fromIntegral
  (out :: SM.MVector RealWorld Float) <- SM.new 5
  withHalideBuffer g $ \gPtr ->
    withHalideBuffer out $ \outPtr ->
      kernel 3 gPtr outPtr
  print =<< S.unsafeFreeze g
  print =<< S.unsafeFreeze out

-- testKernel2
