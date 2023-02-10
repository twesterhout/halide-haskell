{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Control.Monad.ST (RealWorld)
import Data.Int
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import Language.Halide
import Language.Halide.Context (importHalide)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, run)
import Type.Reflection

importHalide

data Matrix v a = Matrix
  { matrixRows :: !Int
  , matrixCols :: !Int
  , matrixData :: !(v a)
  }
  deriving stock (Show, Eq)

instance IsHalideType a => IsHalideBuffer (Matrix (SM.MVector RealWorld) a) 2 a where
  withHalideBuffer (Matrix n m v) f =
    SM.unsafeWith v $ \dataPtr ->
      bufferFromPtrShapeStrides dataPtr [n, m] [1, n] f

main :: IO ()
main = hspec $ do
  describe "mkKernel" $ do
    it "compiles a kernel that adds two vectors together" $ do
      vectorPlus <- mkKernel $ \a b -> do
        i <- mkVar "i"
        define "out" i $ a ! i + b ! i
      let a = S.replicate 10 (1 :: Float)
          b = S.replicate 10 (2 :: Float)
      out <- SM.new 10
      withHalideBuffer a $ \a' ->
        withHalideBuffer b $ \b' ->
          withHalideBuffer out $ \out' ->
            vectorPlus a' b' out'
      S.unsafeFreeze out `shouldReturn` S.zipWith (+) a b

    it "compiles a kernel that generates a scaled diagonal matrix declaratively" $ do
      scaledDiagonal <- mkKernel $ \(scale :: Expr Double) v -> do
        i <- mkVar "i"
        j <- mkVar "j"
        define "out" (i, j) $
          bool
            (i `equal` j)
            (v ! i / scale)
            0
      let a = S.fromList [1.0, 2.0, 3.0]
      out <- Matrix 3 3 <$> SM.replicate 9 0
      withHalideBuffer a $ \a' ->
        withHalideBuffer out $ \out' ->
          scaledDiagonal 2 a' out'
      S.unsafeFreeze (matrixData out) `shouldReturn` S.fromList [0.5, 0, 0, 0, 1, 0, 0, 0, 1.5]

    it "compiles a kernel that generates a scaled diagonal matrix statefully" $ do
      scaledDiagonal <- mkKernel $ \(scale :: Expr Double) v -> do
        i <- mkVar "i"
        j <- mkVar "j"
        out <- define "out" (i, j) 0
        update out (i, i) (v ! i / scale)
        pure out
      let a = S.fromList [1.0, 2.0, 3.0]
      out <- Matrix 3 3 <$> SM.replicate 9 0
      withHalideBuffer a $ \a' ->
        withHalideBuffer out $ \out' ->
          scaledDiagonal 2 a' out'
      S.unsafeFreeze (matrixData out) `shouldReturn` S.fromList [0.5, 0, 0, 0, 1, 0, 0, 0, 1.5]

  describe "vectorize" $ do
    it "vectorizes loops" $ do
      copy <- mkKernel $ \src -> do
        i <- mkVar "i"
        dest <- define "dest" i $ src ! i
        vectorize TailShiftInwards dest i 4
        pure dest
      -- Note that since we use TailShiftInwards, we need the buffers to be at
      -- least 4 elements long.
      let src :: S.Vector Int64
          src = S.generate 10 fromIntegral
      dest <- SM.new (S.length src)
      withHalideBuffer src $ \srcPtr ->
        withHalideBuffer dest $ \destPtr ->
          copy srcPtr destPtr
      S.unsafeFreeze dest `shouldReturn` src

  describe "Num Expr" $ modifyMaxSuccess (const 10) $ do
    let isOverflowing :: Typeable a => (Integer -> Integer -> Integer) -> a -> a -> Bool
        isOverflowing op x y
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
            assert . (x + y ==) =<< run (evaluate (mkExpr x + mkExpr y))
          whenNotOverflowing (-) x y $
            assert . (x - y ==) =<< run (evaluate (mkExpr x - mkExpr y))
          whenNotOverflowing (*) x y $
            assert . (x * y ==) =<< run (evaluate (mkExpr x * mkExpr y))
          assert . (abs x ==) =<< run (evaluate (abs (mkExpr x)))
          assert . (negate x ==) =<< run (evaluate (negate (mkExpr x)))
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
