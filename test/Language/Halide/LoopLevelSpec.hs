{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Halide.LoopLevelSpec (spec) where

import Control.Exception (catch)
import Control.Monad (void)
import Control.Monad.ST (RealWorld)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide
import Language.Halide.Context
import Language.Halide.LoopLevel
import Test.Hspec

importHalide

gpuTarget :: Maybe Target
gpuTarget
  | hostSupportsTargetDevice openCLTarget = Just openCLTarget
  | hostSupportsTargetDevice cudaTarget = Just cudaTarget
  | otherwise = Nothing
  where
    openCLTarget = setFeature FeatureOpenCL hostTarget
    cudaTarget = setFeature FeatureCUDA hostTarget

startIdxOf :: Text -> Text -> Maybe Int
startIdxOf haystack needle =
  case T.splitOn needle haystack of
    (before : _ : _) -> Just (T.length before)
    _ -> Nothing

spec :: Spec
spec = do
  describe "computeAt" $ do
    it "schedules the computation to happen at a particular loop level" $ do
      let innerLoop = do
            x <- mkVar "x"
            y <- mkVar "y"
            g <- define "g" (x, y) $ x * y
            f <- define "f" (x, y) $ g ! (x, y) + g ! (x, y + 1) + g ! (x + 1, y) + g ! (x + 1, y + 1)
            computeAt g =<< getLoopLevel f x
            s <- prettyLoopNest f
            s `shouldSatisfy` T.isInfixOf "produce g"
            s `shouldSatisfy` T.isInfixOf "consume g"
            -- Both loops should appear before the produce statement
            (pure (<) <*> (startIdxOf s "for y") <*> (startIdxOf s "produce g"))
              `shouldBe` Just True
            (pure (<) <*> (startIdxOf s "for x") <*> (startIdxOf s "produce g"))
              `shouldBe` Just True
          outerLoop = do
            x <- mkVar "x"
            y <- mkVar "y"
            g <- define "g" (x, y) $ x * y
            f <- define "f" (x, y) $ g ! (x, y) + g ! (x, y + 1) + g ! (x + 1, y) + g ! (x + 1, y + 1)
            computeAt g =<< getLoopLevel f y
            s <- prettyLoopNest f
            -- The produce statement should appear between for y and for x
            (pure (<) <*> (startIdxOf s "for y") <*> (startIdxOf s "produce g"))
              `shouldBe` Just True
            (pure (>) <*> (startIdxOf s "for x") <*> (startIdxOf s "produce g"))
              `shouldBe` Just True
      innerLoop
      outerLoop

  describe "computeWith" $ do
    it "schedules outer loops to be fused with another computation" $ do
      x <- mkVar "x"
      y <- mkVar "y"
      f <- define "f" (x, y) $ x + y
      g <- define "g" (x, y) $ x - y
      h <- define "h" (x, y) $ f ! (x, y) + g ! (x, y)
      computeRoot f
      computeRoot g
      xi <- mkVar "xi"
      xo <- mkVar "xo"
      split TailAuto f x xo xi 8
      split TailAuto g x xo xi 8
      computeWith LoopAlignAuto g =<< getLoopLevelAtStage f xo 0

      s <- prettyLoopNest h
      s `shouldSatisfy` T.isInfixOf "fused.y"
      s `shouldSatisfy` T.isInfixOf "x.fused.xo"

  describe "storeAt" $ do
    it "allocates storage at a particular loop level" $ do
      -- [C.throwBlock| void {
      --   using namespace Halide;
      --   Func f, g;
      --   Var x, y;
      --   g(x, y) = x*y;
      --   f(x, y) = g(x, y) + g(x, y+1) + g(x+1, y) + g(x+1, y+1);
      --   g.compute_at(f, x);

      --   f.print_loop_nest();
      -- } |]

      x <- mkVar "x"
      y <- mkVar "y"
      g <- define "g" (x, y) $ x * y
      f <- define "f" (x, y) $ g ! (x, y) + g ! (x, y + 1) + g ! (x + 1, y) + g ! (x + 1, y + 1)
      computeAt g =<< getLoopLevel f x
      storeAt g =<< getLoopLevel f y
      s <- prettyLoopNest f
      (pure (<) <*> (startIdxOf s "for y") <*> (startIdxOf s "store g"))
        `shouldBe` Just True
      (pure (>) <*> (startIdxOf s "for x") <*> (startIdxOf s "store g"))
        `shouldBe` Just True
