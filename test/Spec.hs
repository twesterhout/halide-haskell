{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Main (main) where

import Control.Monad.ST (RealWorld)
import Data.Int
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import Language.Halide.Buffer
import Language.Halide.Internal
import Language.Halide.Type
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property)
import Test.QuickCheck.Monadic (assert, monadicIO, run)

data Matrix v a = Matrix
  { matrixRows :: !Int,
    matrixCols :: !Int,
    matrixData :: !(v a)
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

    it "compiles a kernel that generates a scaled diagonal matrix (uses bool)" $ do
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

    it "compiles a kernel that generates a scaled diagonal matrix (uses update)" $ do
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

  describe "Num Expr" $ modifyMaxSuccess (const 10) $ do
    let p :: forall a. (IsHalideType a, Eq a, Num a) => a -> a -> Property
        p x y = monadicIO $ do
          assert . (x + y ==) =<< run (evaluate (mkExpr x + mkExpr y))
          assert . (x - y ==) =<< run (evaluate (mkExpr x - mkExpr y))
          assert . (x * y ==) =<< run (evaluate (mkExpr x * mkExpr y))
          assert . (abs x ==) =<< run (evaluate (abs (mkExpr x)))
          assert . (negate x ==) =<< run (evaluate (negate (mkExpr x)))
    prop "Int8" $ p @Int8
    prop "Int16" $ p @Int16
    -- prop "Int32" $ p @Int32
    -- prop "Int64" $ p @Int64
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