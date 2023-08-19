{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Halide.KernelSpec (spec) where

import Language.Halide
import Test.Hspec
import Utils
import Prelude hiding (Eq (..))
import Prelude qualified

spec :: Spec
spec = do
  describe "compile" $ do
    it "compiles a kernel that adds two vectors together" $ do
      vectorPlus <- compile $ \a b -> do
        i <- mkVar "i"
        define "out" i $ (a ! i :: Expr Float) + b ! i
      let n = 10
          a = replicate 10 (1 :: Float)
          b = replicate 10 (2 :: Float)
      withHalideBuffer @1 @Float a $ \a' ->
        withHalideBuffer @_ @Float b $ \b' ->
          allocaCpuBuffer @_ @Float [n] $ \out' -> do
            vectorPlus a' b' out'
            peekToList out' `shouldReturn` zipWith (+) a b

    it "compiles a kernel that generates a scaled diagonal matrix declaratively" $ do
      scaledDiagonal <- compile $ \(scale :: Expr Double) v -> do
        i <- mkVar "i"
        j <- mkVar "j"
        define "out" (i, j) $
          ifThenElse (i == j) (v ! i / scale) 0
      let a :: [Double]
          a = [1.0, 2.0, 3.0]
      withHalideBuffer a $ \a' ->
        allocaCpuBuffer [3, 3] $ \out' -> do
          scaledDiagonal 2 a' out'
          peekToList out' `shouldReturn` [[0.5, 0, 0], [0, 1, 0], [0, 0, 1.5]]

    it "compiles a kernel that generates a scaled diagonal matrix statefully" $ do
      scaledDiagonal <- compile $ \(scale :: Expr Double) v -> do
        i <- mkVar "i"
        j <- mkVar "j"
        out <- define "out" (i, j) (mkExpr 0)
        update out (i, i) (v ! i / scale)
        pure out
      let a :: [Double]
          a = [1.0, 2.0, 3.0]
      withHalideBuffer a $ \a' ->
        allocaCpuBuffer [3, 3] $ \out' -> do
          scaledDiagonal 2 a' out'
          peekToList out' `shouldReturn` [[0.5, 0, 0], [0, 1, 0], [0, 0, 1.5]]

  describe "compileToLoweredStmt" $ do
    it "compiles to lowered stmt file" $ do
      let builder (buffer "src" -> src) (scalar @Float "c" -> c) = do
            i <- mkVar "i"
            define "dest1234" i $ c * src ! i
          target =
            setFeature FeatureNoAsserts . setFeature FeatureNoBoundsQuery $
              hostTarget
      s <- compileToLoweredStmt StmtText target builder
      s `shouldContainText` "func dest1234 (src, c, dest1234) {"
      s `shouldContainText` "produce dest1234 {"
