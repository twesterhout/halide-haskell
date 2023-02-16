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

spec :: Spec
spec = do
  describe "prints schedules" $ do
    it "of vectorized loops" $ do
      let builder :: Func 1 Int64 -> IO (Func 1 Int64)
          builder src = do
            i <- mkVar "i"
            dest <- define "dest1" i $ src ! i
            setEstimate dest i 0 100
            -- vectorize TailShiftInwards dest i 4
            -- applyAutoscheduler dest "Adams2019" hostTarget
            T.putStrLn =<< prettyLoopNest dest
            print =<< (getDims <$> getStageSchedule dest)
            print =<< (getSplits <$> getStageSchedule dest)
            pure dest
      copy <- mkKernel builder
      let src :: S.Vector Int64
          src = S.generate 100 fromIntegral
      dest <- SM.new (S.length src)
      withHalideBuffer src $ \srcPtr ->
        withHalideBuffer dest $ \destPtr ->
          copy srcPtr destPtr
      S.unsafeFreeze dest `shouldReturn` src
