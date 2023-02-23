module Language.Halide.Tutorial05Spec (spec) where

import Data.Text (Text, unpack)
import qualified Data.Text as T
import Data.Text.IO (hPutStrLn)
import Debug.Trace
import Language.Halide
import Language.Halide.Trace
import System.IO (stderr)
import Test.Hspec hiding (parallel)
import Utils

spec :: Spec
spec = do
  describe "Tutorial 5" $ do
    -- We're going to define and schedule our gradient function in several different ways,
    -- and see what order pixels are computed in. For this, we define a helper function such
    -- that we don't have to constantly repeat the definition of gradient.
    let prepare = do
          x <- mkVar "x"
          y <- mkVar "y"
          gradient <- define "gradient" (x, y) $ x + y
          pure (x, y, gradient)
        getIterationOrder shape f =
          fmap fst $
            collectIterationOrder (TraceStore ==) f $ do
              traceStores f
              realize f shape (void . pure)
        traceM_ x = trace ("\n" <> x) $ pure ()

    it "Has column-major ordering by default" $ do
      -- First we observe the default ordering

      (_, _, gradient) <- prepare
      -- By default, Halide traverses arrays in column-major order. That means that the loop over x
      -- happens first, and then the loop over y. We check our assumption by building the iteration
      -- order using standard list comprehensions
      order <- getIterationOrder [4, 4] gradient
      order `shouldBe` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]
      -- Uncomment the following line to visualize the order
      traceM_ $ show order

      -- Tracing is one useful way to understand what a schedule is doing. You can also ask Halide
      -- to print out pseudocode showing what loops Halide is generating:
      nest <- prettyLoopNest gradient
      traceM_ $ unpack nest

    it "Reorders loops to have compute gradient in row-major order" $ do
      (x, y, gradient) <- prepare
      -- If we reorder x and y, we can walk along the rows instead. The reorder call takes the
      -- arguments of the func, and sets a new nesting order for the for loops that are generated.
      -- The arguments are specified from the innermost loop out, so the following call puts y in
      -- the inner loop:
      void $ reorder [y, x] gradient

      -- We can again check our results using list comprehensions
      getIterationOrder [4, 4] gradient `shouldReturn` [[i, j] | i <- [0 .. 3], j <- [0 .. 3]]

    it "Splits a variable into two" $ do
      (x, y, gradient) <- prepare
      -- The most powerful primitive scheduling operation you can do to a var is to split it into
      -- inner and outer sub-variables:
      inner <- mkVar "inner"
      outer <- mkVar "outer"
      void $ split TailAuto x (outer, inner) 2 gradient

      -- This breaks the loop over x into two nested loops: an outer one over outer, and an inner
      -- one over inner. We have also specified the "split factor". The inner loop runs from zero
      -- to the split factor. The outer loop runs from zero to the extent required of x (4 in this case)
      -- divided by the split factor. Within the loops, the old variable is defined to be
      -- outer * factor + inner. If the old loop started at a value other than zero, then that is
      -- also added within the loops.
      --
      -- Note that the order of evaluation of pixels didn't actually
      -- change! Splitting by itself does nothing, but it does open
      -- up all of the scheduling possibilities that we will explore
      -- below.
      nest <- prettyLoopNest gradient
      traceM_ $ unpack nest
      nest `shouldContainText` "for x.outer"
      nest `shouldContainText` "for x.inner"
      nest & "for x.outer" `appearsBeforeText` "for x.inner"

    it "Fuses two variables into one" $ do
      (x, y, gradient) <- prepare
      -- The opposite of splitting is 'fusing'. Fusing two variables
      -- merges the two loops into a single for loop over the
      -- product of the extents. Fusing is less important than
      -- splitting, but it also sees use (as we'll see later in this
      -- lesson). Like splitting, fusing by itself doesn't change
      -- the order of evaluation.
      fused <- mkVar "fused"
      void $ fuse (x, y) fused gradient

      nest <- prettyLoopNest gradient
      traceM_ $ unpack nest
      nest `shouldContainText` "for x.fused"
      nest `shouldNotContainText` "for y"

    it "Evaluates in tiles" $ do
      (x, y, gradient) <- prepare
      [xOuter, xInner, yOuter, yInner] <- mapM mkVar ["xOuter", "xInner", "yOuter", "yInner"]

      void $
        split TailAuto x (xOuter, xInner) 2 gradient
          >>= split TailAuto y (yOuter, yInner) 2
          >>= reorder [xInner, yInner, xOuter, yOuter]

      traceM_ =<< fmap unpack (prettyLoopNest gradient)

      getIterationOrder [4, 4] gradient
        `shouldReturn` [ [i + 2 * iTile, j + 2 * jTile]
                       | jTile <- [0 .. 1]
                       , iTile <- [0 .. 1]
                       , j <- [0 .. 1]
                       , i <- [0 .. 1]
                       ]

    it "Evaluates in vectors" $ do
      (x, y, gradient) <- prepare
      inner <- mkVar "inner"

      void $
        split TailAuto x (x, inner) 4 gradient
          >>= vectorize inner

      nest <- prettyLoopNest gradient
      traceM_ $ unpack nest
      nest `shouldContainText` "vectorized x.inner"

      getIterationOrder [8, 4] gradient
        `shouldReturn` [ [4 * i .. 4 * i + 3] <> [j, j, j, j]
                       | j <- [0 .. 3]
                       , i <- [0 .. 1]
                       ]

    it "Unrolls loops" $ do
      (x, y, gradient) <- prepare
      inner <- mkVar "inner"

      void $
        split TailAuto x (x, inner) 2 gradient
          >>= unroll inner

      nest <- prettyLoopNest gradient
      traceM_ $ unpack nest
      nest `shouldContainText` "unrolled x.inner"

      getIterationOrder [4, 4] gradient
        `shouldReturn` [ [i, j]
                       | j <- [0 .. 3]
                       , i <- [0 .. 3]
                       ]

    it "Splits by factors that don't divide the extent" $ do
      (x, y, gradient) <- prepare
      inner <- mkVar "inner"

      void $ split TailAuto x (x, inner) 3 gradient

      traceM_ =<< fmap unpack (prettyLoopNest gradient)

      getIterationOrder [7, 4] gradient
        `shouldReturn` [ [iInner + min (3 * iOuter) (7 - 3), j]
                       | j <- [0 .. 3]
                       , iOuter <- [0 .. 2]
                       , iInner <- [0 .. 2]
                       ]

    it "Fuses, tiles, and parallelizes" $ do
      (x, y, gradient) <- prepare
      [xOuter, xInner, yOuter, yInner, tileIndex] <-
        mapM mkVar ["xOuter", "xInner", "yOuter", "yInner", "tileIndex"]

      void $
        split TailAuto x (xOuter, xInner) 4 gradient
          >>= split TailAuto y (yOuter, yInner) 4
          >>= reorder [xInner, yInner, xOuter, yOuter]
          >>= fuse (xOuter, yOuter) tileIndex
          >>= parallel tileIndex

      nest <- prettyLoopNest gradient
      traceM_ $ unpack nest
      nest `shouldContainText` "parallel x.xOuter.tileIndex"
      nest `shouldContainText` "for y.yInner in [0, 3]"
      nest `shouldContainText` "for x.xInner in [0, 3]"
      nest & "parallel" `appearsBeforeText` "for"

    it "Puts it all together for ultimate performance!" $ do
      -- Are you ready? We're going to use all of the features above now.
      (x, y, gradient) <- prepare

      -- We'll process 64x64 tiles in parallel.
      [xOuter, yOuter, xInner, yInner, tileIndex] <-
        mapM mkVar ["xOuter", "yOuter", "xInner", "yInner", "tileIndex"]
      void $
        split TailAuto x (xOuter, xInner) 64 gradient
          >>= split TailAuto y (yOuter, yInner) 64
          >>= reorder [xInner, yInner, xOuter, yOuter]
          >>= fuse (xOuter, yOuter) tileIndex
          >>= parallel tileIndex

      -- We'll compute two scanlines at once while we walk across each tile. We'll also vectorize in x.
      -- The easiest way to express this is to recursively tile again within each tile into 4x2 subtiles,
      -- then vectorize the subtiles across x and unroll them across y:
      [xInnerOuter, yInnerOuter, xVectors, yPairs] <-
        mapM mkVar ["xInnerOuter", "yInnerOuter", "xVectors", "yPairs"]
      void $
        split TailAuto xInner (xInnerOuter, xVectors) 4 gradient
          >>= split TailAuto yInner (yInnerOuter, yPairs) 2
          >>= reorder [xVectors, yPairs, xInnerOuter, yInnerOuter]
          >>= vectorize xVectors
          >>= unroll yPairs

      nest <- prettyLoopNest gradient
      traceM_ $ unpack nest

      realize gradient [350, 250] $ \buf ->
        peekToList buf `shouldReturn` [[i + j | j <- [0 .. 249]] | i <- [0 .. 349]]
