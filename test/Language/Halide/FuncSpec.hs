{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Halide.FuncSpec (spec) where

import Control.Monad.ST (RealWorld)
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide
import Language.Halide.Context
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
  withHalideBufferImpl (Matrix n m v) f =
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
        split TailShiftInwards i (i, ii) 4 func
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
        split TailGuardWithIf i (i, ii) 3 func
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

      void $ reorder [z, x, y] func

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
      void $ split TailGuardWithIf x (outer, inner) 7 func

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
      void $ fuse (x, y) common func

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

      void $
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
        void $ gpuBlocks DeviceDefaultGPU (x, y) func
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
        void $ gpuBlocks DeviceCUDA y func
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
        void $ gpuThreads DeviceDefaultGPU (x, y) func
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
        void $
          gpuBlocks DeviceCUDA y func
            >>= gpuThreads DeviceCUDA x
        prettyLoopNest func >>= \s -> do
          s `shouldContainText` "gpu_block y<CUDA>"
          s `shouldContainText` "gpu_thread x<CUDA>"

  describe "reductions" $ do
    it "computes reductions" $ do
      asBufferParam @1 @Int32 ([1, 2, 3, 4, 5] :: [Int32]) $ \src -> do
        n <- (.extent) <$> dim 0 src
        r <- mkRVar "r" 0 n
        i <- mkVar "i"
        f <- define "sum" i 0
        update f (0 :: Expr Int32) $ f ! (0 :: Expr Int32) + src ! r
        realize f [1] peekToList `shouldReturn` ([15] :: [Int32])

  describe "undef" $ do
    it "allows to skip stores" $ do
      i <- mkVar "i"
      f <- define "f" i $ bool (i `greaterThan` 5) i 0
      update f i $ bool ((f ! i) `equal` 0) (2 * i) undef
      realize f [10] peekToList `shouldReturn` ([0, 2, 4, 6, 8, 10] <> [6 .. 9] :: [Int32])
