module Language.Halide.Tutorial05Spec (spec) where

import GHC.TypeLits (KnownNat)
import Language.Halide
import Language.Halide.Trace
import Test.Hspec hiding (parallel)
import Utils

-- | We're going to define and schedule our gradient function in several different ways,
-- and see what order pixels are computed in. For this, we define a helper function such
-- that we don't have to constantly repeat the definition of gradient.
prepare :: IO (Expr Int32, Expr Int32, Func 'FuncTy 2 Int32)
prepare = do
  x <- mkVar "x"
  y <- mkVar "y"
  gradient <- define "gradient" (x, y) $ x + y
  pure (x, y, gradient)

-- | We also define a helper function to evaluate a 'Func' and collect the order of stores
-- performed by the function. We will use it for testing.
getIterationOrder :: (KnownNat n, IsHalideType a) => [Int] -> Func t n a -> IO [[Int]]
getIterationOrder shape f =
  fmap fst $
    collectIterationOrder (TraceStore ==) f $ do
      _ <- traceStores f
      realize f shape (void . pure)

-- | By default, Halide traverses arrays in column-major order. That means that the loop over x
-- happens first, and then the loop over y. This can be visualized using the 'prettyLoopNest'
-- function:
--
-- >>> (x, y, gradient) <- prepare
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   for y:
--     for x:
--       gradient(...) = ...
example01 :: Spec
example01 = it "Has column-major ordering by default" $ do
  -- First we observe the default ordering
  (_, _, gradient) <- prepare
  -- Let's first make sure that the function actually computes what we want
  realize gradient [4, 4] peekToList
    `shouldReturn` [[i + j | j <- [0 .. 3]] | i <- [0 .. 3]]
  -- We check our assumption about the iteration order by building the expected
  -- iteration order using standard list comprehensions:
  getIterationOrder [4, 4] gradient
    `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]

-- | If we reorder x and y, we can walk along the rows instead. The 'reorder' call takes the
-- arguments of the func, and sets a new nesting order for the for loops that are generated.
-- The arguments are specified from the innermost loop out, so the following call puts y in
-- the inner loop:
--
-- >>> (x, y, gradient) <- prepare
-- >>> reorder [y, x] gradient
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   for x:
--     for y:
--       gradient(...) = ...
example02 :: Spec
example02 = it "Reorders loops to have row-major order" $ do
  (x, y, gradient) <- prepare
  void $ reorder [y, x] gradient
  getIterationOrder [4, 4] gradient
    `shouldReturn` [[i, j] | i <- [0 .. 3], j <- [0 .. 3]]

-- | The most powerful primitive scheduling operation you can do to a var is to split it into
-- inner and outer sub-variables:
--
-- >>> (x, y, gradient) <- prepare
-- >>> inner <- mkVar "inner"
-- >>> outer <- mkVar "outer"
-- >>> split TailAuto x (outer, inner) 2 gradient
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   for y:
--     for x.outer:
--       for x.inner in [0, 1]:
--         gradient(...) = ...
--
-- This breaks the loop over x into two nested loops: an outer one over outer, and an inner
-- one over inner. We have also specified the "split factor". The inner loop runs from zero
-- to the split factor. The outer loop runs from zero to the extent required of x (4 in this case)
-- divided by the split factor. Within the loops, the old variable is defined to be
-- outer * factor + inner. If the old loop started at a value other than zero, then that is
-- also added within the loops.
--
-- Note that the order of evaluation of pixels didn't actually change! Splitting by itself does
-- nothing, but it does open up all of the scheduling possibilities that we will explore below.
example03 :: Spec
example03 = it "Splits a variable into two" $ do
  (x, _, gradient) <- prepare
  inner <- mkVar "inner"
  outer <- mkVar "outer"
  void $ split TailAuto x (outer, inner) 2 gradient
  -- Check that there are now loops over outer and inner
  nest <- prettyLoopNest gradient
  nest `shouldContainText` "for x.outer"
  nest `shouldContainText` "for x.inner"
  nest & "for x.outer" `appearsBeforeText` "for x.inner"
  -- Check that the iteration order is still column-major
  getIterationOrder [4, 4] gradient
    `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]

-- | The opposite of splitting is 'fusing'. Fusing two variables merges the two loops into
-- a single for loop over the product of the extents. Fusing is less important than splitting,
-- but it also sees use (as we'll see later in this lesson). Like splitting, fusing by itself
-- doesn't change the order of evaluation.
--
-- >>> (x, y, gradient) <- prepare
-- >>> fused <- mkVar "fused"
-- >>> fuse (x, y) fused gradient
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   for x.fused:
--     gradient(...) = ...
example04 :: Spec
example04 = it "Fuses two variables into one" $ do
  (x, y, gradient) <- prepare
  fused <- mkVar "fused"
  void $ fuse (x, y) fused gradient
  -- Verify that the loop over y in gone and we're left with the loop over fused
  nest <- prettyLoopNest gradient
  nest `shouldContainText` "for x.fused"
  nest `shouldNotContainText` "for y"
  -- Check that the iteration order is still column-major
  getIterationOrder [4, 4] gradient
    `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]

-- | Now that we can both split and reorder, we can do tiled evaluation. Let's split both
-- x and y by a factor of four, and then reorder the vars to express a tiled traversal.
--
-- A tiled traversal splits the domain into small rectangular tiles, and outermost iterates
-- over the tiles, and within that iterates over the points within each tile. It can be good
-- for performance if neighboring pixels use overlapping input data, for example in a blur.
-- We can express a tiled traversal like so:
--
-- >>> (x, y, gradient) <- prepare
-- >>> [xOuter, xInner, yOuter, yInner] <- mapM mkVar ["xOuter", "xInner", "yOuter", "yInner"]
-- >>> split TailAuto x (xOuter, xInner) 2 gradient
-- >>> split TailAuto y (yOuter, yInner) 2 gradient
-- >>> reorder [xInner, yInner, xOuter, yOuter] gradient
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   for y.yOuter:
--     for x.xOuter:
--       for y.yInner in [0, 1]:
--         for x.xInner in [0, 1]:
--           gradient(...) = ...
example05 :: Spec
example05 = it "Evaluates in tiles" $ do
  (x, y, gradient) <- prepare
  [xOuter, xInner, yOuter, yInner] <- mapM mkVar ["xOuter", "xInner", "yOuter", "yInner"]
  void $
    split TailAuto x (xOuter, xInner) 2 gradient
      >>= split TailAuto y (yOuter, yInner) 2
      >>= reorder [xInner, yInner, xOuter, yOuter]
  getIterationOrder [4, 4] gradient
    `shouldReturn` [ [i + 2 * iTile, j + 2 * jTile]
                   | jTile <- [0 .. 1]
                   , iTile <- [0 .. 1]
                   , j <- [0 .. 1]
                   , i <- [0 .. 1]
                   ]

-- | The nice thing about splitting is that it guarantees the inner variable runs from zero
-- to the split factor. Most of the time the split-factor will be a compile-time constant,
-- so we can replace the loop over the inner variable with a single vectorized computation.
-- This time we'll split by a factor of four, because on X86 we can use SSE to compute in
-- 4-wide vectors.
--
-- >>> (x, y, gradient) <- prepare
-- >>> inner <- mkVar "inner"
-- >>> split TailAuto x (x, inner) 4 gradient
-- >>> vectorize inner gradient
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   for y:
--     for x.x:
--       vectorized x.inner in [0, 3]:
--         gradient(...) = ...
example06 :: Spec
example06 = it "Evaluates in vectors" $ do
  (x, _, gradient) <- prepare
  inner <- mkVar "inner"
  void $
    split TailAuto x (x, inner) 4 gradient
      >>= vectorize inner
  -- Check that vectorization happens:
  nest <- prettyLoopNest gradient
  nest `shouldContainText` "vectorized x.inner"
  -- Check that this is reflected in the iteration order. We now expect coordinates to be a
  -- length 8 list instead of length 2. I.e. each element is [x1, x2, x3, x4, y1, y2, y3, y4].
  --
  -- This time we'll evaluate over an 8x4 box, so that we have more than one vector of work
  -- per scanline.
  getIterationOrder [8, 4] gradient
    `shouldReturn` [ [4 * i .. 4 * i + 3] <> [j, j, j, j]
                   | j <- [0 .. 3]
                   , i <- [0 .. 1]
                   ]

-- | If multiple pixels share overlapping data, it can make sense to unroll a computation
-- so that shared values are only computed or loaded once. We do this similarly to how we
-- expressed vectorizing. We split a dimension and then fully unroll the loop of the inner
-- variable. Unrolling doesn't change the order in which things are evaluated.
--
-- >>> (x, _, gradient) <- prepare
-- >>> inner <- mkVar "inner"
-- >>> split TailAuto x (x, inner) 2 gradient >>= unroll inner
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   for y:
--     for x.x:
--       unrolled x.inner in [0, 1]:
--         gradient(...) = ...
example07 :: Spec
example07 = it "Unrolls loops" $ do
  (x, _, gradient) <- prepare
  inner <- mkVar "inner"
  void $
    split TailAuto x (x, inner) 2 gradient
      >>= unroll inner
  nest <- prettyLoopNest gradient
  nest `shouldContainText` "unrolled x.inner"
  getIterationOrder [4, 4] gradient
    `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]

-- | Splitting guarantees that the inner loop runs from zero to the split factor, which is
-- important for the uses we saw above. So what happens when the total extent we wish to
-- evaluate x over isn't a multiple of the split factor? We'll split by a factor three,
-- and we'll evaluate gradient over a 7x2 box instead of the 4x4 box we've been using.
--
-- >>> (x, y, gradient) <- prepare
-- >>> inner <- mkVar "inner"
-- >>> split TailAuto x (x, inner) 3 gradient
-- >>> getIterationOrder [7, 4] gradient
-- [[0,0],[1,0],[2,0],[3,0],[4,0],[5,0],[4,0],[5,0],[6,0],[0,1],[1,1],[2,1],[3,1],[4,1],[5,1],[4,1],[5,1],[6,1],[0,2],[1,2],[2,2],[3,2],[4,2],[5,2],[4,2],[5,2],[6,2],[0,3],[1,3],[2,3],[3,3],[4,3],[5,3],[4,3],[5,3],[6,3]]
--
-- If you read the output, you'll see that some coordinates were evaluated more than once.
-- That's generally OK, because pure Halide functions have no side-effects, so it's safe to
-- evaluate the same point multiple times.
--
-- The general rule is: If we require x from x_min to x_min + x_extent, and we split by a
-- factor factor, then:
--
-- x_outer runs from 0 to (x_extent + factor - 1)/factor
-- x_inner runs from 0 to factor
-- x = min(x_outer * factor, x_extent - factor) + x_inner + x_min
--
-- In our example, x_min was 0, x_extent was 7, and factor was 3.
example08 :: Spec
example08 = it "Splits by factors that don't divide the extent" $ do
  (x, _, gradient) <- prepare
  inner <- mkVar "inner"
  void $ split TailAuto x (x, inner) 3 gradient
  getIterationOrder [7, 4] gradient
    `shouldReturn` [ [iInner + min (3 * iOuter) (7 - 3), j]
                   | j <- [0 .. 3]
                   , iOuter <- [0 .. 2]
                   , iInner <- [0 .. 2]
                   ]

-- | We saw in the previous lesson that we can parallelize across a variable. Here we combine
-- it with fusing and tiling to express a useful pattern - processing tiles in parallel.
--
-- This is where fusing shines. Fusing helps when you want to parallelize across multiple
-- dimensions without introducing nested parallelism. Nested parallelism (parallel for loops
-- within parallel for loops) is supported by Halide, but often gives poor performance compared
-- to fusing the parallel variables into a single parallel for loop.
--
-- >>> (x, y, gradient) <- prepare
-- >>> [xOuter, xInner, yOuter, yInner, tileIndex] <- mapM mkVar ["xOuter", "xInner", "yOuter", "yInner", "tileIndex"]
-- >>> split TailAuto x (xOuter, xInner) 4 gradient
-- >>> split TailAuto y (yOuter, yInner) 4 gradient
-- >>> reorder [xInner, yInner, xOuter, yOuter] gradient
-- >>> fuse (xOuter, yOuter) tileIndex gradient
-- >>> parallel tileIndex gradient
-- >>> showInCodeLenses =<< prettyLoopNest gradient
-- produce gradient:
--   parallel x.xOuter.tileIndex:
--     for y.yInner in [0, 3]:
--       for x.xInner in [0, 3]:
--         gradient(...) = ...
example09 :: Spec
example09 = it "Fuses, tiles, and parallelizes" $ do
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
  nest `shouldContainText` "parallel x.xOuter.tileIndex"
  nest `shouldContainText` "for y.yInner in [0, 3]"
  nest `shouldContainText` "for x.xInner in [0, 3]"
  nest & "parallel" `appearsBeforeText` "for"

-- | Are you ready? We're going to use all of the features above now.
--
-- We'll process 64x64 tiles in parallel.
--
-- We'll compute two scanlines at once while we walk across each tile. We'll also vectorize in x.
-- The easiest way to express this is to recursively tile again within each tile into 4x2 subtiles,
-- then vectorize the subtiles across x and unroll them across y:
prepareSchedule10 :: IO (Func 'FuncTy 2 Int32)
prepareSchedule10 = do
  (x, y, gradient) <- prepare
  [xOuter, yOuter, xInner, yInner, tileIndex] <-
    mapM mkVar ["xOuter", "yOuter", "xInner", "yInner", "tileIndex"]
  void $
    split TailAuto x (xOuter, xInner) 64 gradient
      >>= split TailAuto y (yOuter, yInner) 64
      >>= reorder [xInner, yInner, xOuter, yOuter]
      >>= fuse (xOuter, yOuter) tileIndex
      >>= parallel tileIndex

  [xInnerOuter, yInnerOuter, xVectors, yPairs] <-
    mapM mkVar ["xInnerOuter", "yInnerOuter", "xVectors", "yPairs"]
  void $
    split TailAuto xInner (xInnerOuter, xVectors) 4 gradient
      >>= split TailAuto yInner (yInnerOuter, yPairs) 2
      >>= reorder [xVectors, yPairs, xInnerOuter, yInnerOuter]
      >>= vectorize xVectors
      >>= unroll yPairs

  pure gradient

-- >>> showInCodeLenses =<< prettyLoopNest =<< prepareSchedule10
-- produce gradient:
--   parallel x.xOuter.tileIndex:
--     for y.yInner.yInnerOuter in [0, 31]:
--       for x.xInner.xInnerOuter in [0, 15]:
--         unrolled y.yInner.yPairs in [0, 1]:
--           vectorized x.xInner.xVectors in [0, 3]:
--             gradient(...) = ...
--
example10 :: Spec
example10 = it "Puts it all together for ultimate performance!" $ do
  gradient <- prepareSchedule10
  realize gradient [350, 250] $ \buf ->
    peekToList buf `shouldReturn` [[i + j | j <- [0 .. 249]] | i <- [0 .. 349]]

spec :: Spec
spec = do
  example01
  example02
  example03
  example04
  example05
  example06
  example07
  example08
  example09
  example10
