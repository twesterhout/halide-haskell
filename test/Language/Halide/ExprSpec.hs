module Language.Halide.ExprSpec (spec) where

import Data.Int
import Data.Word
import Language.Halide
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Type.Reflection

spec :: Spec
spec = do
  describe "Num Expr" $ modifyMaxSuccess (const 10) $ do
    let isOverflowing :: Typeable a => (Integer -> Integer -> Integer) -> a -> a -> Bool
        isOverflowing op x y
          | Just HRefl <- eqTypeRep (typeOf x) (typeRep @Int8) =
              op (toInteger x) (toInteger y) > toInteger (maxBound @Int8)
                || op (toInteger x) (toInteger y) < toInteger (minBound @Int8)
          | Just HRefl <- eqTypeRep (typeOf x) (typeRep @Int32) =
              op (toInteger x) (toInteger y) > toInteger (maxBound @Int32)
                || op (toInteger x) (toInteger y) < toInteger (minBound @Int32)
          | Just HRefl <- eqTypeRep (typeOf x) (typeRep @Int64) =
              op (toInteger x) (toInteger y) > toInteger (maxBound @Int64)
                || op (toInteger x) (toInteger y) < toInteger (minBound @Int64)
          | otherwise = False
        whenNotOverflowing op x y check
          | isOverflowing op x y = pure ()
          | otherwise = check
        p :: forall a. (IsHalideType a, Eq a, Num a, Typeable a) => a -> a -> Property
        p x y = monadicIO $ do
          whenNotOverflowing (+) x y $
            assert . (x + y ==) =<< run (let r = (mkExpr x + mkExpr y) in checkType @a r >> evaluate r)
          whenNotOverflowing (-) x y $
            assert . (x - y ==) =<< run (let r = (mkExpr x - mkExpr y) in checkType @a r >> evaluate r)
          whenNotOverflowing (*) x y $
            assert . (x * y ==) =<< run (let r = (mkExpr x * mkExpr y) in checkType @a r >> evaluate r)
          assert . (abs x ==) =<< run (evaluate (abs (mkExpr x)))
          assert . (negate x ==) =<< run (evaluate (negate (mkExpr x)))
    it "" $
      evaluate ((mkExpr (-128) :: Expr Int8) + mkExpr (-2)) `shouldReturn` 126
    modifyMaxSuccess (const 100) $
      prop "Int8" $
        p @Int8
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
        p x y = monadicIO $ do
          let z = x / y
          z' <- run $ evaluate (mkExpr x / mkExpr y)
          assert $ (x == 0 && y == 0) || z == z'
    prop "Float" $ p @Float
    prop "Double" $ p @Double
