module Language.Halide.KernelSpec (spec) where

import Control.Monad.ST (RealWorld)
import qualified Data.Text as T
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide
import Test.Hspec

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

spec :: Spec
spec = do
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

  describe "compileToLoweredStmt" $ do
    it "compiles to lowered stmt file" $ do
      let target =
            setFeature FeatureNoAsserts . setFeature FeatureNoBoundsQuery $
              hostTarget
      s <- compileToLoweredStmt StmtText target $
        \(src :: Func 1 Float) -> do
          setName src "src"
          i <- mkVar "i"
          define "dest1234" i $ src ! i
      -- T.putStrLn s
      s `shouldSatisfy` T.isInfixOf "func dest1234 (src, dest1234) {"
      s `shouldSatisfy` T.isInfixOf "produce dest1234 {"
