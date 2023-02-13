module Language.Halide.FuncSpec (spec) where

import Data.Int
import qualified Data.Text as T
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Language.Halide
import Test.Hspec

spec :: Spec
spec = do
  describe "vectorize" $ do
    it "vectorizes loops" $ do
      let builder :: Func 1 Int64 -> IO (Func 1 Int64)
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
      -- Check that we're generating ramps on the destination buffer
      s `shouldSatisfy` T.isInfixOf "ramp(dest1"

  describe "unroll" $ do
    it "unrolls loops" $ do
      copy <- mkKernel $ \src -> do
        i <- mkVar "i"
        dest <- define "dest" i $ src ! i
        vectorize TailPredicate dest i 4
        pure dest
      let src :: S.Vector Int64
          src = S.generate 10 fromIntegral
      dest <- SM.new (S.length src)
      withHalideBuffer src $ \srcPtr ->
        withHalideBuffer dest $ \destPtr ->
          copy srcPtr destPtr
      S.unsafeFreeze dest `shouldReturn` src
