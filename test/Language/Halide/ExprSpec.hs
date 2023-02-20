-- {-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Language.Halide.ExprSpec (spec) where

import Control.Monad (when)
import Data.Int
import Data.Word

-- import qualified Language.C.Inline.Cpp.Exception as C
import Language.Halide

-- import Language.Halide.Context
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Type.Reflection

-- importHalide

spec :: Spec
spec = do
  describe "Num Expr" $ modifyMaxSuccess (const 10) $ do
    let isOverflowing :: Typeable a => (Integer -> Integer -> Integer) -> a -> a -> Bool
        isOverflowing op x y
          -- \| Just HRefl <- eqTypeRep (typeOf x) (typeRep @Int8) =
          --     op (toInteger x) (toInteger y) > toInteger (maxBound @Int8)
          --       || op (toInteger x) (toInteger y) < toInteger (minBound @Int8)
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
        p x y =
          monadicIO $ do
            whenNotOverflowing (+) x y $
              assert . (x + y ==) =<< run (let r = (mkExpr x + mkExpr y) in checkType @a r >> evaluate r)
            whenNotOverflowing (-) x y $
              assert . (x - y ==) =<< run (let r = (mkExpr x - mkExpr y) in checkType @a r >> evaluate r)
            whenNotOverflowing (*) x y $
              assert . (x * y ==) =<< run (let r = (mkExpr x * mkExpr y) in checkType @a r >> evaluate r)
            -- Temporary disable: see https://github.com/halide/Halide/issues/7365
            when (x /= -128) $
              assert . (abs x ==) =<< run (evaluate (abs (mkExpr x)))
            assert . (negate x ==) =<< run (evaluate (negate (mkExpr x)))
    -- it "" $ do
    --   [C.throwBlock| void {
    --     handle_halide_exceptions([](){
    --       using namespace Halide;
    --       auto x = Expr{int8_t{-128}};
    --       auto y = cast(x.type(), abs(x));
    --       std::cout << evaluate<int8_t>(y) << std::endl;
    --     });
    --   } |]
    --   evaluate (abs (mkExpr (-128) :: Expr Int8)) `shouldReturn` abs (-128 :: Int8)
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
        p x y = monadicIO $ do
          let z = x / y
          z' <- run $ evaluate (mkExpr x / mkExpr y)
          assert $ (x == 0 && y == 0) || z == z'
    prop "Float" $ p @Float
    prop "Double" $ p @Double
