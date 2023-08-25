# Vectorize, parallelize, unroll and tile your code

This lesson demonstrates how to manipulate the order in which you evaluate
pixels in a `Func`, including vectorization, parallelization, unrolling, and
tiling.

Because this README is also a literate Haskell file, we start with a few common imports.

```haskell
{-# LANGUAGE OverloadedStrings, TypeApplications, DataKinds #-}
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import System.IO (stderr)
import Test.Hspec hiding (parallel)
import Prelude hiding ((==))

import Language.Halide
```

We're going to define and schedule our gradient function in several different
ways, and see what order pixels are computed in. For this, we define a helper
function such that we don't have to constantly repeat the definition of it.

```haskell
prepare :: IO (Expr Int32, Expr Int32, Func 'FuncTy 2 (Expr Int32))
prepare = do
  x <- mkVar "x"
  y <- mkVar "y"
  gradient <- define "gradient" (x, y) $ x + y
  pure (x, y, gradient)
```

We also define a helper function to evaluate a `Func` and collect the order of
stores performed by the function. We will use it for testing.

```haskell
getIterationOrder :: (KnownNat n, IsHalideType a) => [Int] -> Func t n (Expr a) -> IO [[Int]]
getIterationOrder shape f =
  fmap fst $
    collectIterationOrder (TraceStore ==) f $ do
      _ <- traceStores f
      realize f shape (void . pure)
```

Finally, we define a helper function to dump `Text` to stderr.

```haskell
toStderr :: Text -> IO ()
toStderr a = hPutStrLn stderr ("\n" <> a)
```

## Default

By default, Halide traverses arrays in column-major order. That means that the
loop over x happens first, and then the loop over y. This can be visualized
using the `prettyLoopNest` function:

```haskell
main :: IO ()
main = hspec $ describe "Tutorial 5" $ do
  it "Has column-major ordering by default" $ do
    (_, _, gradient) <- prepare
    toStderr =<< prettyLoopNest gradient
```

This produces:

```
produce gradient:
  for y:
    for x:
      gradient(...) = ...
```

Let's make sure that the function actually computes what we want:

```haskell
    realize gradient [4, 4] peekToList
      `shouldReturn` [[i + j | j <- [0 .. 3]] | i <- [0 .. 3]]
```

`realize` function evaluates our pipeline over a rectangular domain -- in our
case a 4x4 buffer, and `peekToList` then reads the contents of the buffer into
a nested list.

We check our assumption about the iteration order by building the expected
iteration order using standard list comprehensions:

```haskell
    getIterationOrder [4, 4] gradient
      `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]
```

## Reorder

If we reorder `x` and `y`, we can walk along the rows instead. The `reorder` call takes the
arguments of the func, and sets a new nesting order for the for loops that are generated.
The arguments are specified from the innermost loop out, so the following puts `y` in
the inner loop:

```haskell
  it "Reorders loops to have row-major order" $ do
    (x, y, gradient) <- prepare
    void $ reorder [y, x] gradient
    toStderr =<< prettyLoopNest gradient
    getIterationOrder [4, 4] gradient
      `shouldReturn` [[i, j] | i <- [0 .. 3], j <- [0 .. 3]]
```

And the loop nest now reads

```
produce gradient:
  for x:
    for y:
      gradient(...) = ...
```

## Split

The most powerful primitive scheduling operation you can do to a var is to split it into
inner and outer sub-variables:

```haskell
  it "Splits a variable into two" $ do
    (x, _, gradient) <- prepare
    inner <- mkVar "inner"
    outer <- mkVar "outer"
    void $ split TailAuto x (outer, inner) 2 gradient
    toStderr =<< prettyLoopNest gradient
```

This breaks the loop over `x` into two nested loops: an outer one over outer, and an inner
one over inner. We have also specified the "split factor". The inner loop runs from zero
to the split factor. The outer loop runs from zero to the extent required of `x` (4 in this case)
divided by the split factor. Within the loops, the old variable is defined to be
`outer * factor + inner`. If the old loop started at a value other than zero, then that is
also added within the loops.

```
produce gradient:
  for y:
    for x.outer:
      for x.inner in [0, 1]:
        gradient(...) = ...
```

Note that the order of evaluation of pixels didn't actually change! Splitting by itself does
nothing, but it does open up all of the scheduling possibilities that we will explore below.

```haskell
    getIterationOrder [4, 4] gradient
      `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]
```

## Fuse

The opposite of splitting is fusing. Fusing two variables merges the two loops into
a single for loop over the product of the extents. Fusing is less important than splitting,
but it also sees use (as we'll see later in this lesson). Like splitting, fusing by itself
doesn't change the order of evaluation.

```haskell
  it "Fuses two variables into one" $ do
    (x, y, gradient) <- prepare
    fused <- mkVar "fused"
    void $ fuse (x, y) fused gradient
    toStderr =<< prettyLoopNest gradient

    getIterationOrder [4, 4] gradient
      `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]
```

```
produce gradient:
  for x.fused:
    gradient(...) = ...
```

## Tile

Now that we can both split and reorder, we can do tiled evaluation. Let's split both
x and y by a factor of four, and then reorder the vars to express a tiled traversal:

```haskell
  it "Evaluates in tiles" $ do
    (x, y, gradient) <- prepare
    [xOuter, xInner, yOuter, yInner] <- mapM mkVar ["xOuter", "xInner", "yOuter", "yInner"]
    void $
      split TailAuto x (xOuter, xInner) 2 gradient
        >>= split TailAuto y (yOuter, yInner) 2
        >>= reorder [xInner, yInner, xOuter, yOuter]
    toStderr =<< prettyLoopNest gradient
```

```
produce gradient:
  for y.yOuter:
    for x.xOuter:
      for y.yInner in [0, 1]:
        for x.xInner in [0, 1]:
          gradient(...) = ...
```

A tiled traversal splits the domain into small rectangular tiles, and outermost iterates
over the tiles, and within that iterates over the points within each tile. It can be good
for performance if neighboring pixels use overlapping input data, for example in a blur.

```haskell
    getIterationOrder [4, 4] gradient
      `shouldReturn` [ [i + 2 * iTile, j + 2 * jTile]
                     | jTile <- [0 .. 1]
                     , iTile <- [0 .. 1]
                     , j <- [0 .. 1]
                     , i <- [0 .. 1]
                     ]
```

## Vectorize

The nice thing about splitting is that it guarantees the inner variable runs from zero
to the split factor. Most of the time the split-factor will be a compile-time constant,
so we can replace the loop over the inner variable with a single vectorized computation.
This time we'll split by a factor of four, because on X86 we can use SSE to compute in
4-wide vectors.


```haskell
  it "Evaluates in vectors" $ do
    (x, _, gradient) <- prepare
    inner <- mkVar "inner"
    void $
      split TailAuto x (x, inner) 4 gradient
        >>= vectorize inner
    toStderr =<< prettyLoopNest gradient
```

```
produce gradient:
  for y:
    for x.x:
      vectorized x.inner in [0, 3]:
        gradient(...) = ...
```

We can check that the vectorization is reflected in the iteration order. We now expect coordinates to be a
length 8 list instead of length 2. I.e. each element is [x1, x2, x3, x4, y1, y2, y3, y4]. This time we'll evaluate over an 8x4 box, so that we have more than one vector of work per scanline.

```haskell
    getIterationOrder [8, 4] gradient
      `shouldReturn` [ [4 * i .. 4 * i + 3] <> [j, j, j, j]
                     | j <- [0 .. 3]
                     , i <- [0 .. 1]
                     ]
```

## Unroll

If multiple pixels share overlapping data, it can make sense to unroll a computation
so that shared values are only computed or loaded once. We do this similarly to how we
expressed vectorizing. We split a dimension and then fully unroll the loop of the inner
variable. Unrolling doesn't change the order in which things are evaluated.

```haskell
  it "Unrolls loops" $ do
    (x, _, gradient) <- prepare
    inner <- mkVar "inner"
    void $
      split TailAuto x (x, inner) 2 gradient
        >>= unroll inner
    toStderr =<< prettyLoopNest gradient

    getIterationOrder [4, 4] gradient
      `shouldReturn` [[i, j] | j <- [0 .. 3], i <- [0 .. 3]]

```

```
produce gradient:
  for y:
    for x.x:
      unrolled x.inner in [0, 1]:
        gradient(...) = ...
```

## Split again

Splitting guarantees that the inner loop runs from zero to the split factor, which is
important for the uses we saw above. So what happens when the total extent we wish to
evaluate x over isn't a multiple of the split factor? We'll split by a factor three,
and we'll evaluate gradient over a 7x2 box instead of the 4x4 box we've been using.

```haskell
  it "Splits by factors that don't divide the extent" $ do
    (x, _, gradient) <- prepare
    inner <- mkVar "inner"
    void $ split TailAuto x (x, inner) 3 gradient

    order <- getIterationOrder [7, 4] gradient
    toStderr . pack . show $ order
```

```
[[0,0],[1,0],[2,0],[3,0],[4,0],[5,0],[4,0],[5,0],[6,0],
 [0,1],[1,1],[2,1],[3,1],[4,1],[5,1],[4,1],[5,1],[6,1],
 [0,2],[1,2],[2,2],[3,2],[4,2],[5,2],[4,2],[5,2],[6,2],
 [0,3],[1,3],[2,3],[3,3],[4,3],[5,3],[4,3],[5,3],[6,3]]
```

If you read the output, you'll see that some coordinates were evaluated more than once.
That's generally OK, because pure Halide functions have no side-effects, so it's safe to
evaluate the same point multiple times.

The general rule is: If we require x from x_min to x_min + x_extent, and we split by a
factor factor, then:

x_outer runs from 0 to (x_extent + factor - 1)/factor
x_inner runs from 0 to factor
x = min(x_outer * factor, x_extent - factor) + x_inner + x_min

In our example, x_min was 0, x_extent was 7, and factor was 3.

```haskell
    order
      `shouldBe` [ [iInner + Prelude.min (3 * iOuter) (7 - 3), j]
                 | j <- [0 .. 3]
                 , iOuter <- [0 .. 2]
                 , iInner <- [0 .. 2]
                 ]
```


## Parallelize

We saw in the previous lesson that we can parallelize across a variable. Here we combine
it with fusing and tiling to express a useful pattern - processing tiles in parallel.

This is where fusing shines. Fusing helps when you want to parallelize across multiple
dimensions without introducing nested parallelism. Nested parallelism (parallel for loops
within parallel for loops) is supported by Halide, but often gives poor performance compared
to fusing the parallel variables into a single parallel for loop.

```haskell
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

    toStderr =<< prettyLoopNest gradient
```

```
produce gradient:
  parallel x.xOuter.tileIndex:
    for y.yInner in [0, 3]:
      for x.xInner in [0, 3]:
        gradient(...) = ...
```

## All together now

Are you ready? We're going to use all of the features above now.

We'll process 64x64 tiles in parallel.

```haskell
  it "Puts it all together for ultimate performance!" $ do
    (x, y, gradient) <- prepare
    [xOuter, yOuter, xInner, yInner, tileIndex] <-
      mapM mkVar ["xOuter", "yOuter", "xInner", "yInner", "tileIndex"]
    void $
      split TailAuto x (xOuter, xInner) 64 gradient
        >>= split TailAuto y (yOuter, yInner) 64
        >>= reorder [xInner, yInner, xOuter, yOuter]
        >>= fuse (xOuter, yOuter) tileIndex
        >>= parallel tileIndex
```

We'll compute two scanlines at once while we walk across each tile. We'll also vectorize in x.
The easiest way to express this is to recursively tile again within each tile into 4x2 subtiles,
then vectorize the subtiles across x and unroll them across y:

```haskell
    [xInnerOuter, yInnerOuter, xVectors, yPairs] <-
      mapM mkVar ["xInnerOuter", "yInnerOuter", "xVectors", "yPairs"]
    void $
      split TailAuto xInner (xInnerOuter, xVectors) 4 gradient
        >>= split TailAuto yInner (yInnerOuter, yPairs) 2
        >>= reorder [xVectors, yPairs, xInnerOuter, yInnerOuter]
        >>= vectorize xVectors
        >>= unroll yPairs

    toStderr =<< prettyLoopNest gradient
```

```
produce gradient:
  parallel x.xOuter.tileIndex:
    for y.yInner.yInnerOuter in [0, 31]:
      for x.xInner.xInnerOuter in [0, 15]:
        unrolled y.yInner.yPairs in [0, 1]:
          vectorized x.xInner.xVectors in [0, 3]:
            gradient(...) = ...
```

Let's check that the result is still as we expect

```haskell
    realize gradient [350, 250] peekToList
      `shouldReturn` [[i + j | j <- [0 .. 249]] | i <- [0 .. 349]]
```
