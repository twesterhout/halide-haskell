-- {-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Halide.LoopLevelSpec (spec) where

-- import Control.Exception (catch)
-- import Control.Monad (void)
-- import Control.Monad.ST (RealWorld)
-- import Data.Function ((&))
-- import Data.Int
-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.Encoding as T
-- import qualified Data.Text.IO as T
-- import qualified Data.Vector.Storable as S
-- import qualified Data.Vector.Storable.Mutable as SM
-- import qualified Language.C.Inline.Cpp.Exception as C
-- import qualified Language.C.Inline.Unsafe as CU
-- import Language.Halide.Context
-- import Language.Halide.LoopLevel
import Language.Halide
import Test.Hspec

importHalide

spec :: Spec
spec = do
  pure ()

-- describe "computeAt" $ do
--   it "schedules the computation to happen at a particular loop level" $ do
--     let innerLoop = do
--           x <- mkVar "x"
--           y <- mkVar "y"
--           g <- define "g" (x, y) $ x * y
--           -- f <- define "f" (x, y) $ g ! (x, y) + g ! (x, y + 1) + g ! (x + 1, y) + g ! (x + 1, y + 1)
--           f <-
--             define "f" (x, y) $
--               sum $
--                 (g !) <$> [(x, y), (x, y + 1), (x + 1, y), (x + 1, y + 1)]
--           -- T.putStrLn =<< prettyLoopNest f
--           computeAt g =<< getLoopLevel f x
--           s <- prettyLoopNest f
--           s `shouldContainText` "produce g"
--           s `shouldContainText` "consume g"
--           -- T.putStrLn s
--           -- Both loops should appear before the produce statement
--           s & "for y" `appearsBeforeText` "produce g"
--           s & "for x" `appearsBeforeText` "produce g"
--         outerLoop = do
--           x <- mkVar "x"
--           y <- mkVar "y"
--           g <- define "g" (x, y) $ x * y
--           -- f <- define "f" (x, y) $ g ! (x, y) + g ! (x, y + 1) + g ! (x + 1, y) + g ! (x + 1, y + 1)
--           f <-
--             define "f" (x, y) $
--               sum $
--                 (g !) <$> [(x, y), (x, y + 1), (x + 1, y), (x + 1, y + 1)]
--           computeAt g =<< getLoopLevel f y
--           s <- prettyLoopNest f
--           -- The produce statement should appear between for y and for x
--           s & "for y" `appearsBeforeText` "produce g"
--           s & "produce g" `appearsBeforeText` "for x"
--     innerLoop
--     outerLoop

-- describe "computeWith" $ do
--   it "schedules outer loops to be fused with another computation" $ do
--     x <- mkVar "x"
--     y <- mkVar "y"
--     f <- define "f" (x, y) $ x + y
--     g <- define "g" (x, y) $ x - y
--     h <- define "h" (x, y) $ f ! (x, y) + g ! (x, y)
--     computeRoot f
--     computeRoot g
--     xi <- mkVar "xi"
--     xo <- mkVar "xo"
--     split TailAuto f x xo xi 8
--     split TailAuto g x xo xi 8

--     prettyLoopNest h >>= \s -> do
--       s `shouldContainText` "for x.xo"
--       s `shouldContainText` "for x.xi"
--       s `shouldNotContainText` "fused"

--     computeWith LoopAlignAuto g =<< getLoopLevelAtStage f xo 0

--     prettyLoopNest h >>= \s -> do
--       s `shouldContainText` "for x.xi"
--       s `shouldContainText` "for fused.y"
--       s `shouldContainText` "for x.fused.xo"

-- describe "storeAt" $ do
--   it "allocates storage at a particular loop level" $ do
--     -- [C.throwBlock| void {
--     --   using namespace Halide;
--     --   Func f, g;
--     --   Var x, y;
--     --   g(x, y) = x*y;
--     --   f(x, y) = g(x, y) + g(x, y+1) + g(x+1, y) + g(x+1, y+1);
--     --   g.compute_at(f, x);

--     --   f.print_loop_nest();
--     -- } |]

--     x <- mkVar "x"
--     y <- mkVar "y"
--     g <- define "g" (x, y) $ x * y
--     f <- define "f" (x, y) $ g ! (x, y) + g ! (x, y + 1) + g ! (x + 1, y) + g ! (x + 1, y + 1)
--     computeAt g =<< getLoopLevel f x
--     T.putStrLn =<< prettyLoopNest f
--     storeAt g =<< getLoopLevel f y
--     T.putStrLn =<< prettyLoopNest f
--     s <- prettyLoopNest f
--     (pure (<) <*> (startIdxOf s "for y") <*> (startIdxOf s "store g"))
--       `shouldBe` Just True
--     (pure (>) <*> (startIdxOf s "for x") <*> (startIdxOf s "store g"))
--       `shouldBe` Just True
