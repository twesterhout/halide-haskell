{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.ST (RealWorld)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide.Buffer
import Language.Halide.Internal

main :: IO ()
main = do
  i <- mkVar "i"
  f <- define "f" i $ 2 * cast @Float i + 1
  g <- define "g" i $ 1 + f ! i
  printLoopNest g
  print =<< realize1D g 10
  print =<< realize1D g 10
  print "Compiling kernel ..."
  kernel <- testKernel1
  (buffer :: SM.MVector RealWorld Float) <- SM.new 5
  print "Invoking kernel ..."
  withHalideBuffer buffer $ \p ->
    kernel Nil p
  print =<< S.unsafeFreeze buffer
  print "Compiling kernel ..."
  let buildFunc :: Arguments 1 '[Expr Float] -> IO (Func 1 Float)
      buildFunc (a ::: Nil) = do
        setName a "a"
        j <- mkVar "j"
        func <- define "func" j $ print' a
        printLoopNest func
        pure func
  kernel2 <- mkKernel $ buildFunc
  (buffer2 :: SM.MVector RealWorld Float) <- SM.new 5
  print "Invoking kernel ..."
  withHalideBuffer buffer2 $ \p ->
    kernel2 ((3 :: Float) ::: Nil) p
  print =<< S.unsafeFreeze buffer2
  testKernel2
