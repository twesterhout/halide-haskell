{-# LANGUAGE OverloadedRecordDot #-}

module Language.Halide.ScheduleSpec (spec) where

import Control.Monad.ST (RealWorld)
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.ForeignPtr
import Language.Halide
import Language.Halide.Schedule
import Test.Hspec
import Utils

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

-- autoschedule :: Scheduler -> Func n a -> IO Schedule

spec :: Spec
spec = do
  describe "prints schedules" $ do
    it "of auto-scheduled pipelines" $ do
      let builder :: Target -> Func 'ParamTy 1 Int64 -> IO (Func 'FuncTy 1 Float)
          builder target src = do
            i <- mkVar "i"
            dest <- define "dest1" i $ sin (cast @Float (src ! i))
            -- dim 0 src >>= setEstimate 0 1000
            dim 0 src >>= setMin 0 >>= setStride 1 >>= print
            -- schedule <- do
            estimate i 0 1000 dest

            loadAutoScheduler Adams2019
            applyAutoScheduler Adams2019 target dest
            T.putStrLn =<< prettyLoopNest dest
            -- schedule <- getStageSchedule dest
            -- print schedule.dims
            -- print =<< (getSplits <$> getStageSchedule dest)
            pure dest
      let target = hostTarget -- setFeature FeatureOpenCL hostTarget
      copy <- compileForTarget target (builder target)
      -- let src :: S.Vector Int64
      --     src = S.generate 100 fromIntegral
      pure ()
    it "of computeWith" $ do
      x <- mkVar "x"
      y <- mkVar "y"
      f <- define "f" (x, y) $ x + y
      g <- define "g" (x, y) $ x - y
      k <- define "k" (x, y) $ x * y
      h <- define "h" (x, y) $ f ! (x, y) + g ! (x, y) + k ! (x, y)
      computeRoot f
      computeRoot g
      computeRoot k
      xi <- mkVar "xi"
      xo <- mkVar "xo"
      split TailAuto x (xo, xi) 8 f
      split TailAuto x (xo, xi) 8 g
      split TailAuto x (xo, xi) 8 k
      l <- getLoopLevelAtStage f xo 0
      print l
      computeWith LoopAlignAuto g l
      computeWith LoopAlignAuto f =<< getLoopLevelAtStage k xo 0

      hPutStrLn stderr =<< prettyLoopNest h

      schedule <- getStageSchedule =<< getStage g
      print schedule

-- prettyLoopNest h >>= \s -> do
--   s `shouldContainText` "for x.xi"
--   s `shouldContainText` "for fused.y"
--   s `shouldContainText` "for x.fused.xo"

-- dest <- SM.new (S.length src)
-- withHalideBuffer src $ \srcPtr ->
--   withHalideBuffer dest $ \destPtr ->
--     copy srcPtr destPtr
-- S.unsafeFreeze dest `shouldReturn` src
