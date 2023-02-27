module Language.Halide.ExprSpec (spec) where

import Control.Monad (unless, when)
import Data.Int
import Data.Word
import Language.Halide
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, run)
import Type.Reflection
import Utils

isOverflowing :: Typeable a => (Integer -> Integer -> Integer) -> a -> a -> Bool
isOverflowing op x y
  | Just HRefl <- eqTypeRep (typeOf x) (typeRep @Int32) =
      op (toInteger x) (toInteger y) > toInteger (maxBound @Int32)
        || op (toInteger x) (toInteger y) < toInteger (minBound @Int32)
  | Just HRefl <- eqTypeRep (typeOf x) (typeRep @Int64) =
      op (toInteger x) (toInteger y) > toInteger (maxBound @Int64)
        || op (toInteger x) (toInteger y) < toInteger (minBound @Int64)
  | otherwise = False

infix 1 `evaluatesTo`

evaluatesTo :: (Eq a, IsHalideType a) => Expr a -> a -> PropertyM IO ()
evaluatesTo expr expected =
  assert . (expected ==) =<< (run . evaluate) expr

infix 1 `evaluatesToApprox`

evaluatesToApprox :: (Ord a, IsHalideType a, HasEpsilon a) => Expr a -> a -> PropertyM IO ()
evaluatesToApprox expr expected =
  assert . approx' expected =<< (run . evaluate) expr

infix 1 `shouldEvaluateTo`

shouldEvaluateTo :: (Eq a, IsHalideType a, Show a) => Expr a -> a -> Expectation
shouldEvaluateTo expr expected =
  evaluate expr `shouldReturn` expected

spec :: Spec
spec = do
  describe "mkExpr" $ modifyMaxSuccess (const 10) $ do
    let p :: forall a. (IsHalideType a, Eq a) => a -> Property
        p x = monadicIO $ mkExpr x `evaluatesTo` x
    prop "Bool" $ p @Bool

  describe "Num Expr" $ modifyMaxSuccess (const 10) $ do
    let whenNotOverflowing op x y check
          | isOverflowing op x y = pure ()
          | otherwise = check
        p :: forall a. (IsHalideType a, Eq a, Num a, Typeable a) => a -> a -> Property
        p x y =
          monadicIO $ do
            whenNotOverflowing (+) x y $
              mkExpr x + mkExpr y `evaluatesTo` x + y
            whenNotOverflowing (-) x y $
              mkExpr x - mkExpr y `evaluatesTo` x - y
            whenNotOverflowing (*) x y $
              mkExpr x * mkExpr y `evaluatesTo` x * y
            -- Temporary disable: see https://github.com/halide/Halide/issues/7365
            when (x /= -128) $
              abs (mkExpr x) `evaluatesTo` abs x
            negate (mkExpr x) `evaluatesTo` negate x
    prop "Int8" $ p @Int8
    prop "Int16" $ p @Int16
    prop "Int32" $ p @Int32
    prop "Int64" $ p @Int64
    prop "Word8" $ p @Word8
    prop "Word16" $ p @Word16
    prop "Word32" $ p @Word32
    prop "Word64" $ p @Word64
    prop "Float" $ p @Float
    prop "Double" $ p @Double

  describe "Fractional Expr" $ modifyMaxSuccess (const 10) $ do
    let p :: forall a. (IsHalideType a, Eq a, Fractional a) => a -> a -> Property
        p x y =
          monadicIO $
            unless (x == 0 && y == 0) $
              mkExpr x / mkExpr y `evaluatesTo` x / y
    prop "Float" $ p @Float
    prop "Double" $ p @Double

  describe "Floating Expr" $ modifyMaxSuccess (const 10) $ do
    let p :: forall a. (IsHalideType a, Ord a, Floating a, HasEpsilon a) => a -> Property
        p x = monadicIO $ do
          when (x > 0) $ do
            log (mkExpr x) `evaluatesToApprox` log x
            sqrt (mkExpr x) `evaluatesToApprox` sqrt x
          exp (mkExpr x) `evaluatesToApprox` exp x
          sin (mkExpr x) `evaluatesToApprox` sin x
          cos (mkExpr x) `evaluatesToApprox` cos x
          tan (mkExpr x) `evaluatesToApprox` tan x
          when (-1 <= x && x <= 1) $ do
            asin (mkExpr x) `evaluatesToApprox` asin x
            acos (mkExpr x) `evaluatesToApprox` acos x
            atan (mkExpr x) `evaluatesToApprox` atan x
          sinh (mkExpr x) `evaluatesToApprox` sinh x
          cosh (mkExpr x) `evaluatesToApprox` cosh x
          tanh (mkExpr x) `evaluatesToApprox` tanh x
          asinh (mkExpr x) `evaluatesToApprox` asinh x
          when (x >= 1) $
            acosh (mkExpr x) `evaluatesToApprox` acosh x
          when (-1 <= x && x <= 1) $
            atanh (mkExpr x) `evaluatesToApprox` atanh x
    prop "Float" $ p @Float
    prop "Double" $ p @Double
    it "defines pi" $ do
      (pi :: Expr Float) `shouldEvaluateTo` pi
      (pi :: Expr Double) `shouldEvaluateTo` pi
