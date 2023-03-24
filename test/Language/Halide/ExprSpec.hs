module Language.Halide.ExprSpec (spec) where

import Control.Monad (unless, when)
import Data.Int
import Data.Text (Text)
import Data.Word
import Language.Halide
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
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

-- infix 1 `evaluatesTo`

-- evaluatesTo :: (Eq a, IsHalideType a) => Expr a -> a -> PropertyM IO ()
-- evaluatesTo expr expected =
--   assert . (expected ==) =<< (run . evaluate) expr
--

infix 1 `shouldEvaluateToApprox`

shouldEvaluateToApprox :: (Ord a, IsHalideType a, HasEpsilon a, Show a) => Expr a -> a -> Expectation
shouldEvaluateToApprox expr expected = do
  e <- evaluate expr
  e `shouldBeApprox` expected

infix 1 `shouldEvaluateTo`

shouldEvaluateTo :: (Eq a, IsHalideType a, Show a) => Expr a -> a -> Expectation
shouldEvaluateTo expr expected = evaluate expr `shouldReturn` expected

spec :: Spec
spec = do
  describe "mkExpr" $ modifyMaxSuccess (const 10) $ do
    prop "Bool" $ \x ->
      mkExpr (x :: Bool) `shouldEvaluateTo` x

  describe "Num Expr" $ modifyMaxSuccess (const 10) $ do
    let whenNotOverflowing op x y check
          | isOverflowing op x y = pure ()
          | otherwise = check
        p :: forall a. (IsHalideType a, Eq a, Num a, Typeable a, Show a) => a -> a -> Expectation
        p x y = do
          whenNotOverflowing (+) x y $
            mkExpr x + mkExpr y `shouldEvaluateTo` x + y
          whenNotOverflowing (-) x y $
            mkExpr x - mkExpr y `shouldEvaluateTo` x - y
          whenNotOverflowing (*) x y $
            mkExpr x * mkExpr y `shouldEvaluateTo` x * y
          unless (x == -128) $
            abs (mkExpr x) `shouldEvaluateTo` abs x
          negate (mkExpr x) `shouldEvaluateTo` negate x
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
    let p :: forall a. (IsHalideType a, Eq a, Fractional a, Show a) => a -> a -> Expectation
        p x y = do
          unless (x == 0 && y == 0) $
            mkExpr x / mkExpr y `shouldEvaluateTo` x / y
    prop "Float" $ property (p @Float)
    prop "Double" $ property (p @Double)

  describe "Floating Expr" $ modifyMaxSuccess (const 10) $ do
    let p :: forall a. (IsHalideType a, Ord a, Floating a, HasEpsilon a, Show a) => a -> Expectation
        p x = do
          let y = mkExpr x
          when (x > 0) $ do
            log y `shouldEvaluateToApprox` log x
            sqrt y `shouldEvaluateToApprox` sqrt x
          exp y `shouldEvaluateToApprox` exp x
          sin y `shouldEvaluateToApprox` sin x
          cos y `shouldEvaluateToApprox` cos x
          tan y `shouldEvaluateToApprox` tan x
          when (-1 <= x && x <= 1) $ do
            asin y `shouldEvaluateToApprox` asin x
            acos y `shouldEvaluateToApprox` acos x
            atan y `shouldEvaluateToApprox` atan x
          sinh y `shouldEvaluateToApprox` sinh x
          cosh y `shouldEvaluateToApprox` cosh x
          tanh y `shouldEvaluateToApprox` tanh x
          asinh y `shouldEvaluateToApprox` asinh x
          when (x >= 1) $
            acosh y `shouldEvaluateToApprox` acosh x
          when (-1 <= x && x <= 1) $
            atanh y `shouldEvaluateToApprox` atanh x
    prop "Float" $ p @Float
    prop "Double" $ p @Double
    it "defines pi" $ do
      (pi :: Expr Float) `shouldEvaluateToApprox` pi
      (pi :: Expr Double) `shouldEvaluateToApprox` pi

  describe "printed" $
    it "prints expressions when evaluated" $ do
      printed (1 :: Expr Int32) `shouldEvaluateTo` 1
      printed (1 :: Expr Int32) ("<- when" :: String) ("haha" :: String) `shouldEvaluateTo` 1
      let x :: Expr Float
          x = 1
       in printed (sin x) ("<- sin(" :: Text) x (")" :: Text) `shouldEvaluateToApprox` sin 1

  -- describe "Show" $
  --   it "shows 123" $ do
  --     show (123 :: Expr Int32) `shouldBe` "123"
  describe "testWriteToStderr" $ do
    it "shows 123" $ do
      testWriteToStderr
