{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Halide.FuncSpec (spec) where

import Control.Monad.ST (RealWorld)
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide
import Language.Halide.Context
import Language.Halide.Func
import Test.Hspec

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

gpuTarget :: Maybe (Target)
gpuTarget
  | hostSupportsTargetDevice openCLTarget = Just openCLTarget
  | hostSupportsTargetDevice cudaTarget = Just cudaTarget
  | otherwise = Nothing
  where
    openCLTarget = setFeature FeatureOpenCL hostTarget
    cudaTarget = setFeature FeatureCUDA hostTarget

spec :: Spec
spec = do
  describe "vectorize" $ do
    it "vectorizes loops" $ do
      let builder :: Func 'ParamTy 1 Int64 -> IO (Func 'FuncTy 1 Int64)
          builder src = do
            i <- mkVar "i"
            dest <- define "dest1" i $ src ! i
            vectorize TailShiftInwards dest i 4
            pure dest
      copy <- mkKernel builder
      -- Note that since we use TailShiftInwards, we need the buffers to be at
      -- least 4 elements long.
      let src :: S.Vector Int64
          src = S.generate 10 fromIntegral
      dest <- SM.new (S.length src)
      withHalideBuffer src $ \srcPtr ->
        withHalideBuffer dest $ \destPtr ->
          copy srcPtr destPtr
      S.unsafeFreeze dest `shouldReturn` src
      s <- compileToLoweredStmt StmtText hostTarget builder
      T.putStrLn s
      -- Check that we're generating ramps on the destination buffer
      s `shouldSatisfy` T.isInfixOf "ramp(dest1"

  describe "unroll" $ do
    it "unrolls loops" $ do
      copy <- mkKernel $ \src -> do
        i <- mkVar "i"
        dest <- define "dest" i $ src ! i
        unroll TailPredicate dest i 4
        pure dest
      let src :: S.Vector Int64
          src = S.generate 10 fromIntegral
      dest <- SM.new (S.length src)
      withHalideBuffer src $ \srcPtr ->
        withHalideBuffer dest $ \destPtr ->
          copy srcPtr destPtr
      S.unsafeFreeze dest `shouldReturn` src

  -- it "repeated unrolls work" $ do
  --   let builder (src :: Func 1 Int64) = do
  --         i <- mkVar "i"
  --         dest <- define "dest" i $ src ! i
  --         unroll TailPredicate dest i 4
  --         pure dest
  --   copy <- mkKernel builder
  --   let src :: S.Vector Int64
  --       src = S.generate 10 fromIntegral
  --   dest <- SM.new (S.length src)
  --   withHalideBuffer src $ \srcPtr ->
  --     withHalideBuffer dest $ \destPtr ->
  --       copy srcPtr destPtr
  --   S.unsafeFreeze dest `shouldReturn` src

  describe "reorder" $ do
    it "reorders loops" $ do
      let
        builder (src :: Func 'ParamTy 1 Int64) = do
          i <- mkVar "i"
          j <- mkVar "j"
          dest <-
            define "matrix" (i, j) $
              bool (equal i j) (src ! i) 0
          reorder dest (j, i)
          pure dest
      copy <- mkKernel builder
      let src :: S.Vector Int64
          src = S.generate 5 fromIntegral
      dest <- Matrix 5 5 <$> SM.new (S.length src * S.length src)
      withHalideBuffer src $ \srcPtr ->
        withHalideBuffer dest $ \destPtr ->
          copy srcPtr destPtr
      print =<< S.unsafeFreeze (matrixData dest)
      s <- compileToLoweredStmt StmtText (setFeature FeatureNoAsserts hostTarget) builder
      T.putStrLn s

  describe "gpuBlocks" $ do
    it "binds indices to gpu block indices" $ do
      let builder (src :: Func 'ParamTy 1 Int64) = do
            i <- mkVar "i"
            j <- mkVar "j"
            dest <-
              define "matrix" (i, j) $
                bool (equal i j) (src ! i) 0
            _ <-
              pure dest
                >>= gpuBlocks (i, j)
                >>= computeRoot
            s <- prettyLoopNest dest
            s `shouldSatisfy` T.isInfixOf "gpu_block i"
            s `shouldSatisfy` T.isInfixOf "gpu_block j"
            _ <-
              (src `asUsedBy` dest)
                >>= copyToDevice DeviceDefaultGPU
                >>= computeRoot
            asUsed dest >>= copyToHost
      case gpuTarget of
        Just target -> do
          copy <- mkKernelForTarget (setFeature FeatureDebug target) builder
          let src :: S.Vector Int64
              src = S.generate 3 fromIntegral
          dest <- Matrix 3 3 <$> SM.new (S.length src * S.length src)
          withHalideBuffer src $ \srcPtr ->
            withHalideBuffer dest $ \destPtr ->
              copy srcPtr destPtr
          print =<< S.unsafeFreeze (matrixData dest)
        Nothing -> do
          T.putStrLn "\nSkipping gpuBlocks test, because no GPU target is available"
          pure ()

-- s <- compileToLoweredStmt StmtText (setFeature FeatureNoAsserts hostTarget) builder
-- T.putStrLn s

-- s `shouldSatisfy` T.isInfixOf "ramp(dest1"

--  `shouldReturn` src

-- s <- compileToLoweredStmt StmtText (setFeature FeatureNoAsserts hostTarget) builder
-- T.putStrLn s
