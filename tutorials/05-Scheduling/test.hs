#line 10 "all"
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
import Test.Hspec

import Language.Halide
#line 21 "all"
prepare :: IO (Expr Int32, Expr Int32, Func 'FuncTy 2 Int32)
prepare = do
  x <- mkVar "x"
  y <- mkVar "y"
  gradient <- define "gradient" (x, y) $ x + y
  pure (x, y, gradient)
#line 33 "all"
getIterationOrder :: (KnownNat n, IsHalideType a) => [Int] -> Func t n a -> IO [[Int]]
getIterationOrder shape f =
  fmap fst $
    collectIterationOrder (TraceStore ==) f $ do
      _ <- traceStores f
      realize f shape (void . pure)
#line 53 "all"
example01 :: Spec
example01 = it "Has column-major ordering by default" $ do
  -- First we observe the default ordering
  (_, _, gradient) <- prepare
  -- Let's first make sure that the function actually computes what we want
  realize gradient [4, 4] peekToList
    `shouldReturn` [[i + j | j <- [0 .. 3]] | i <- [0 .. 3]]
  -- We check our assumption about the iteration order by building the expected
  -- iteration order using standard list comprehensions:
  getIterationOrder [4, 4] gradient
    `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]
#line 73 "all"
main :: IO ()
main = hspec $ specify "Tutorial 1" $ do
  x <- mkVar "x"
  y <- mkVar "y"
