{-# LANGUAGE OverloadedRecordDot #-}

module Language.Halide.ScheduleSpec (spec) where

import Control.Monad (forM_)
import qualified Data.Text.IO as T
import GHC.TypeLits
import Language.Halide
import Test.Hspec
import Test.Hspec.QuickCheck
import Utils

checkScheduleRoundTrip :: (KnownNat n, IsHalideType a) => IO (Func t n a) -> (Func t n a -> IO ()) -> Expectation
checkScheduleRoundTrip prepare schedule = do
  f1 <- prepare
  f2 <- prepare

  schedule f1
  s1 <- getStageSchedule =<< getStage f1
  l1 <- prettyLoopNest f1

  applySchedule s1 =<< getStage f2
  s2 <- getStageSchedule =<< getStage f2
  l2 <- prettyLoopNest f2

  l1 `shouldBe` l2
  s1 `shouldBeEqForTesting` s2

spec :: Spec
spec = do
  describe "Extracts schedules" $ do
    it "supports vectorize" $ do
      [x, y, z] <- mapM mkVar ["x", "y", "z"]
      xInner <- mkVar "xInner"
      f <- define "f" (x, y, z) $ sin (cast @Float (x * y * z))
      void $
        split TailAuto x (x, xInner) 2 f
          >>= vectorize xInner
      schedule <- getStageSchedule =<< getStage f
      head schedule.dims `shouldBe` Dim "x.xInner" ForVectorized DeviceNone DimPureVar

    it "supports fuse" $ do
      [x, y, z] <- mapM mkVar ["x", "y", "z"]
      k <- mkVar "k"
      f <- define "f" (x, y, z) $ sin (cast @Float (x * y * z))
      void $ fuse (y, z) k f
      schedule <- getStageSchedule =<< getStage f
      print schedule

  describe "Applies schedules" $ do
    it "supports split" $ do
      let prepare = do
            [x, y, z] <- mapM mkVar ["x", "y", "z"]
            define "f" (x, y, z) $ sin (cast @Float (x * y * z))
          schedule f = do
            [x, _, _] <- getArgs f
            xInner <- mkVar "xInner"
            void $ split TailAuto x (x, xInner) 2 f
      checkScheduleRoundTrip prepare schedule
    it "supports fuse" $ do
      -- pendingWith "fails for unknown reason"
      let prepare = do
            [x, y, z] <- mapM mkVar ["x", "y", "z"]
            define "f" (x, y, z) $ sin (cast @Float (x * y * z))
          schedule f = do
            [_, y, z] <- getArgs f
            k <- mkVar "k"
            void $ fuse (y, z) k f
      checkScheduleRoundTrip prepare schedule
    it "supports vectorize" $ do
      -- pendingWith "fails for unknown reason"
      let prepare = do
            [x, y, z] <- mapM mkVar ["x", "y", "z"]
            define "f" (x, y, z) $ sin (cast @Float (x * y * z))
          schedule f = do
            [x, _, _] <- getArgs f
            xOuter <- mkVar "xOuter"
            xInnerOuter <- mkVar "xInnerOuter"
            xInnerInner <- mkVar "xInnerInner"
            void $
              split TailAuto x (x, xOuter) 4 f
                >>= split TailAuto xOuter (xInnerOuter, xInnerInner) 2
                >>= vectorize xInnerInner
      checkScheduleRoundTrip prepare schedule
    it "supports computeWith" $ do
      -- pendingWith "fails for unknown reason"
      let prepare = do
            x <- mkVar "x"
            y <- mkVar "y"
            f <- define "f" (x, y) $ x + y
            g <- define "g" (x, y) $ x - y
            h <- define "h" (x, y) $ f ! (x, y) + g ! (x, y)
            estimate x 0 200 h
            estimate y 0 200 h
            pure h
      let schedule h = do
            loadAutoScheduler Adams2019
            T.putStrLn =<< applyAutoScheduler Adams2019 hostTarget h
      checkScheduleRoundTrip prepare schedule

    -- [x, _, _] <- getArgs f
    -- xOuter <- mkVar "xOuter"
    -- xInnerOuter <- mkVar "xInnerOuter"
    -- xInnerInner <- mkVar "xInnerInner"
    -- void $
    --   split TailAuto x (x, xOuter) 4 f
    --     >>= split TailAuto xOuter (xInnerOuter, xInnerInner) 2
    --     >>= vectorize xInnerInner
    -- computeRoot f
    -- computeRoot g
    -- computeRoot k
    -- xi <- mkVar "xi"
    -- xo <- mkVar "xo"
    -- split TailAuto x (xo, xi) 8 f
    -- split TailAuto x (xo, xi) 8 g
    -- split TailAuto x (xo, xi) 8 k
    -- l <- getLoopLevelAtStage f xo 0
    -- print l
    -- computeWith LoopAlignAuto g l
    -- computeWith LoopAlignAuto f =<< getLoopLevelAtStage k xo 0

    prop "supports autoschedulers" $ do
      -- pendingWith "fails for unknown reason"
      let prepare1 = do
            [x, y] <- mapM mkVar ["x", "y"]
            f <- define "f" (x, y) $ x * y
            estimate x 0 100 f
            estimate y 0 100 f
            pure f
      let prepare2 = do
            x <- mkVar "x"
            y <- mkVar "y"
            f <- define "f" (x, y) $ x + y
            g <- define "g" (x, y) $ x - y
            h <- define "h" (x, y) $ f ! (x, y) + g ! (x, y)
            estimate x 0 200 h
            estimate y 0 200 h
            pure h
      let schedule target (Just scheduler) f = do
            loadAutoScheduler scheduler
            void $ applyAutoScheduler scheduler target f
          schedule _ _ _ = pure ()

      forM_ [prepare1, prepare2] $ \prepare -> do
        checkScheduleRoundTrip prepare (schedule hostTarget Nothing)
        checkScheduleRoundTrip prepare (schedule hostTarget (Just Adams2019))
        checkScheduleRoundTrip prepare (schedule hostTarget (Just Li2018))
        checkScheduleRoundTrip prepare (schedule hostTarget (Just Mullapudi2016))

-- (x, y, z, xInner, f1) <- prepare
-- split TailAuto x (x, xInner) 2 f1
-- nest1 <- prettyLoopNest f1
-- schedule1 <- getStageSchedule =<< getStage f1
-- print schedule1
-- (_, _, _, _, f2) <- prepare
-- applySchedule schedule1 =<< getStage f2
-- schedule2 <- getStageSchedule =<< getStage f2
-- nest2 <- prettyLoopNest f2
-- nest1 `shouldBe` nest2
-- T.putStrLn nest2
-- print schedule2

{-
describe "prints schedules" $ do
  it "of auto-scheduled pipelines" $ do
    let builder :: Bool -> Target -> Func 'ParamTy 1 Int64 -> IO (Func 'FuncTy 1 Float)
        builder useAutoScheduler target src = do
          i <- mkVar "i"
          dest <- define "dest1" i $ sin (cast @Float (src ! i))
          -- dim 0 src >>= setEstimate 0 1000
          -- dim 0 src >>= setMin 0 >>= setStride 1 >>= print
          -- schedule <- do
          estimate i 0 1000 dest

          when useAutoScheduler $ do
            loadAutoScheduler Adams2019
            T.putStrLn =<< applyAutoScheduler Adams2019 target dest
          print =<< getStageSchedule =<< getStage dest
          -- print =<< getStageSchedule =<< getStage dest

          -- T.putStrLn =<< prettyLoopNest dest
          -- T.putStrLn =<< prettyLoopNest clone
          -- schedule <- getStageSchedule dest
          -- print schedule.dims
          -- print =<< (getSplits <$> getStageSchedule dest)
          pure dest
    let target = hostTarget -- setFeature FeatureOpenCL hostTarget
    copy <- compileForTarget target (builder True target)
    -- let src :: S.Vector Int64
    --     src = S.generate 100 fromIntegral
    pure ()
  -}
{-
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
-}

-- prettyLoopNest h >>= \s -> do
--   s `shouldContainText` "for x.xi"
--   s `shouldContainText` "for fused.y"
--   s `shouldContainText` "for x.fused.xo"

-- dest <- SM.new (S.length src)
-- withHalideBuffer src $ \srcPtr ->
--   withHalideBuffer dest $ \destPtr ->
--     copy srcPtr destPtr
-- S.unsafeFreeze dest `shouldReturn` src
