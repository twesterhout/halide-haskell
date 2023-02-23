{-# LANGUAGE OverloadedRecordDot #-}

module Language.Halide.ScheduleSpec (spec) where

import Control.Monad.ST (RealWorld)
import Data.Int
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide
import Language.Halide.Schedule
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

-- autoschedule :: Scheduler -> Func n a -> IO Schedule

spec :: Spec
spec = do
  pure ()

-- describe "prints schedules" $ do
--   it "of auto-scheduled pipelines" $ do
--     let builder :: Target -> Func 'ParamTy 1 Int64 -> IO (Func 'FuncTy 1 Float)
--         builder target src = do
--           i <- mkVar "i"
--           dest <- define "dest1" i $ sin (cast @Float (src ! i))
--           -- dim 0 src >>= setEstimate 0 1000
--           dim 0 src >>= setMin 0 >>= setStride 1 >>= print
--           -- schedule <- do
--           estimate i 0 1000 dest
--           --  autoschedule defAdams2019{...} dest
--           -- applySchedule schedule{...} dest
--           -- vectorize TailShiftInwards dest i 4
--           applyAutoscheduler dest "Adams2019" target
--           T.putStrLn =<< prettyLoopNest dest
--           schedule <- getStageSchedule dest
--           print schedule.dims
--           print =<< (getSplits <$> getStageSchedule dest)
--           pure dest
--     let target = hostTarget -- setFeature FeatureOpenCL hostTarget
--     copy <- mkKernelForTarget target (builder target)
--     -- let src :: S.Vector Int64
--     --     src = S.generate 100 fromIntegral
--     pure ()

-- dest <- SM.new (S.length src)
-- withHalideBuffer src $ \srcPtr ->
--   withHalideBuffer dest $ \destPtr ->
--     copy srcPtr destPtr
-- S.unsafeFreeze dest `shouldReturn` src
