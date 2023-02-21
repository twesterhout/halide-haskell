{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Halide.FuncSpec (spec) where

import Control.Monad (forM_)
import Control.Monad.ST (RealWorld)
import Data.Functor ((<&>))
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
import Test.Hspec hiding (parallel)
import Utils

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

spec :: Spec
spec = do
  describe "vectorize" $ do
    it "vectorizes loops" $ do
      i <- mkVar "i"
      ii <- mkVar "inner"
      func <- define "func" i $ (3 * i + 1) * (i - 5)
      -- by default, nothing is vectorized
      prettyLoopNest func >>= \s -> do
        s `shouldNotContainText` "vectorized"
        s `shouldNotContainText` "[0, 3]"

      void $
        pure func
          >>= split TailShiftInwards i (i, ii) 4
          >>= vectorize ii

      -- now, the inner loop is vectorized
      prettyLoopNest func >>= \s -> do
        s `shouldContainText` "vectorized i.inner"
        s `shouldContainText` "in [0, 3]"

      let n = 10
      realize1D n func
        `shouldReturn` S.generate n (\(fromIntegral -> k) -> (3 * k + 1) * (k - 5))

  describe "unroll" $ do
    it "unrolls loops" $ do
      i <- mkVar "i"
      ii <- mkVar "inner"
      func <- define "func" i $ (3 * i + 1) * (i - 5)
      -- by default, nothing is unrolled
      prettyLoopNest func >>= \s -> do
        s `shouldNotContainText` "unrolled"
        s `shouldNotContainText` "[0, 2]"

      void $
        pure func
          >>= split TailGuardWithIf i (i, ii) 3
          >>= unroll ii

      -- now, the inner loop is unrolled
      prettyLoopNest func >>= \s -> do
        s `shouldContainText` "unrolled i.inner"
        s `shouldContainText` "in [0, 2]"

      let n = 17
      realize1D n func
        `shouldReturn` S.generate n (\(fromIntegral -> k) -> (3 * k + 1) * (k - 5))

  describe "reorder" $ do
    it "reorders loops" $ do
      x <- mkVar "x"
      y <- mkVar "y"
      z <- mkVar "z"
      func <- define "func" (x, y, z) $ x * (x + y) - 3 * z

      -- we have
      --
      -- for z
      --   for y
      --     for x
      prettyLoopNest func >>= \s -> do
        s & "for z" `appearsBeforeText` "for y"
        s & "for y" `appearsBeforeText` "for x"

      reorder (z, x, y) func

      -- now we expect
      --
      -- for y
      --   for x
      --     for z
      prettyLoopNest func >>= \s -> do
        s & "for y" `appearsBeforeText` "for x"
        s & "for x" `appearsBeforeText` "for z"

  describe "split" $ do
    it "splits loops into sub-loops" $ do
      x <- mkVar "x"
      y <- mkVar "y"
      func <- define "func" (x, y) $ x * y

      -- we have
      --
      -- for y
      --   for x
      prettyLoopNest func >>= \s -> do
        s & "for y" `appearsBeforeText` "for x"
        s `shouldNotContainText` "outer"
        s `shouldNotContainText` "inner"

      outer <- mkVar "outer"
      inner <- mkVar "inner"
      split TailGuardWithIf x (outer, inner) 7 func

      -- now we expect
      --
      -- for y
      --   for x.outer
      --     for x.inner
      prettyLoopNest func >>= \s -> do
        s & "for y" `appearsBeforeText` "for x.outer"
        s & "for x.outer" `appearsBeforeText` "for x.inner"

  describe "fuse" $ do
    it "merges sub-loops into one" $ do
      x <- mkVar "x"
      y <- mkVar "y"
      func <- define "func" (x, y) $ x * y

      -- we have
      --
      -- for y
      --   for x
      prettyLoopNest func >>= \s -> do
        s `shouldNotContainText` "common"

      common <- mkVar "common"
      fuse (x, y) common func

      -- now we expect
      --
      -- for common
      prettyLoopNest func >>= \s -> do
        s `shouldNotContainText` "for x:"
        s `shouldNotContainText` "for y"
        s `shouldContainText` "for x.common"

  describe "parallel" $ do
    it "marks dimensions as parallel" $ do
      x <- mkVar "x"
      y <- mkVar "y"
      func <- define "func" (x, y) $ x * y

      prettyLoopNest func >>= \s ->
        s `shouldNotContainText` "parallel"

      parallel x func
        >>= serial y

      prettyLoopNest func >>= \s ->
        s `shouldContainText` "parallel x"

  describe "gpuBlocks" $ do
    it "marks dimensions as corresponding to GPU blocks" $ do
      do
        x <- mkVar "x"
        y <- mkVar "y"
        func <- define "func" (x, y) $ x * y

        prettyLoopNest func >>= \s -> do
          s `shouldNotContainText` "gpu_block"
          s `shouldNotContainText` "Default_GPU"
        gpuBlocks DeviceDefaultGPU (x, y) func
        prettyLoopNest func >>= \s -> do
          s `shouldContainText` "gpu_block y<Default_GPU>"
          s `shouldContainText` "gpu_block x<Default_GPU>"

      do
        x <- mkVar "x"
        y <- mkVar "y"
        func <- define "func" (x, y) $ x * y

        prettyLoopNest func >>= \s -> do
          s `shouldNotContainText` "gpu_block"
          s `shouldNotContainText` "CUDA"
        gpuBlocks DeviceCUDA y func
        prettyLoopNest func >>= \s -> do
          s `shouldContainText` "gpu_block y<CUDA>"

  describe "gpuThreads" $ do
    it "marks dimensions as corresponding to GPU threads" $ do
      do
        x <- mkVar "x"
        y <- mkVar "y"
        func <- define "func" (x, y) $ x * y

        prettyLoopNest func >>= \s -> do
          s `shouldNotContainText` "gpu_thread"
          s `shouldNotContainText` "Default_GPU"
        gpuThreads DeviceDefaultGPU (x, y) func
        prettyLoopNest func >>= \s -> do
          s `shouldContainText` "gpu_thread y<Default_GPU>"
          s `shouldContainText` "gpu_thread x<Default_GPU>"

      do
        x <- mkVar "x"
        y <- mkVar "y"
        func <- define "func" (x, y) $ x * y

        prettyLoopNest func >>= \s -> do
          s `shouldNotContainText` "gpu_block"
          s `shouldNotContainText` "gpu_thread"
          s `shouldNotContainText` "CUDA"
        gpuBlocks DeviceCUDA y func
          >>= gpuThreads DeviceCUDA x
        prettyLoopNest func >>= \s -> do
          s `shouldContainText` "gpu_block y<CUDA>"
          s `shouldContainText` "gpu_thread x<CUDA>"

-- :: (IndexTuple i ts, 1 <= Length ts, Length ts <= 3) => DeviceAPI -> i -> f n a -> IO (f n a)
-- gpuThreads :: (IndexTuple i ts, 1 <= Length ts, Length ts <= 3) => DeviceAPI -> i -> f n a -> IO (f n a)

-- describe "reductions" $ do
--   it "does reductions" $ do
--     let
--       builder (buffer @2 @Float "a" -> a) b = do
--         i <- mkVar "i"
--         j <- mkVar "j"
--         sumSize <- dim 0 a <&> (.extent)
--         k <- mkRVar "k" 0 sumSize
--         c <- define "C" (i, j) 0
--         update c (i, j) $ c ! (i, j) + a ! (i, k) * b ! (k, j)
--         s <- prettyLoopNest c
--         T.putStrLn s
--         pure c
--     copy <- mkKernel builder
--     pure ()
-- -- let src = S.generate 3 ((+ 1) . fromIntegral)
-- -- <- Matrix 3 3 <$> SM.new (S.length src * S.length src)
-- -- withHalideBuffer src $ \srcPtr ->
-- --   withHalideBuffer dest $ \destPtr ->
-- --     copy srcPtr destPtr
-- -- S.unsafeFreeze (matrixData dest) `shouldReturn` [1, 0, 0, 0, 2, 0, 0, 0, 3]

-- describe "gpuBlocks" $ do
--   it "binds indices to gpu block indices" $ do
--     let builder (src :: Func 'ParamTy 1 Int64) = do
--           i <- mkVar "i"
--           j <- mkVar "j"
--           dest <-
--             define "matrix" (i, j) $
--               bool (equal i j) (src ! i) 0
--           _ <-
--             pure dest
--               >>= gpuBlocks (i, j)
--               >>= computeRoot
--           s <- prettyLoopNest dest
--           s `shouldSatisfy` T.isInfixOf "gpu_block i"
--           s `shouldSatisfy` T.isInfixOf "gpu_block j"
--           _ <-
--             (src `asUsedBy` dest)
--               >>= copyToDevice DeviceDefaultGPU
--               >>= computeRoot
--           asUsed dest >>= copyToHost
--     case gpuTarget of
--       Nothing -> T.putStrLn "\nSkipping gpuBlocks test, because no GPU target is available"
--       Just target -> do
--         copy <- mkKernelForTarget (setFeature FeatureDebug target) builder
--         let src :: S.Vector Int64
--             src = S.generate 3 fromIntegral
--         dest <- Matrix 3 3 <$> SM.new (S.length src * S.length src)
--         withHalideBuffer src $ \srcPtr ->
--           withHalideBuffer dest $ \destPtr ->
--             copy srcPtr destPtr
--         print =<< S.unsafeFreeze (matrixData dest)

-- s <- compileToLoweredStmt StmtText (setFeature FeatureNoAsserts hostTarget) builder
-- T.putStrLn s

-- s `shouldSatisfy` T.isInfixOf "ramp(dest1"

--  `shouldReturn` src

-- s <- compileToLoweredStmt StmtText (setFeature FeatureNoAsserts hostTarget) builder
-- T.putStrLn s
