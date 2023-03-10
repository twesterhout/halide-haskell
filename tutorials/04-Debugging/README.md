# Debugging with tracing, `printed`, and `printedWhen`

This lesson demonstrates how to follow what Halide is doing at runtime.

Since this README is also a literate Haskell file, we start with a few common imports.

```haskell
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Text.IO (hPutStr)
import System.IO (withFile, IOMode (..))
import Test.Hspec hiding (parallel)

import Language.Halide hiding (evaluate)
```

### Printing out the value of Funcs as they are computed.

We'll define our gradient function as before.

```haskell
main :: IO ()
main = hspec $ specify "Tutorial 4" $ do
  x <- mkVar "x"
  y <- mkVar "y"
  gradient <- define "gradient" (x, y) $ x + y
```

And tell Halide that we'd like to be notified of all evaluations.

```haskell
  traceStores gradient
```

Realize the function over an 8x8 region.

```haskell
  putStrLn "Evaluating gradient ..."
  realize gradient [8, 8] . const $ pure ()
```

<details>
<summary>Show output...</summary>

```
Begin pipeline gradient.0()
Tag gradient.0() tag = "func_type_and_dim: 1 0 32 1 2 0 8 0 8"
Store gradient.0(0, 0) = 0
Store gradient.0(1, 0) = 1
Store gradient.0(2, 0) = 2
Store gradient.0(3, 0) = 3
Store gradient.0(4, 0) = 4
Store gradient.0(5, 0) = 5
Store gradient.0(6, 0) = 6
Store gradient.0(7, 0) = 7
Store gradient.0(0, 1) = 1
Store gradient.0(1, 1) = 2
Store gradient.0(2, 1) = 3
Store gradient.0(3, 1) = 4
Store gradient.0(4, 1) = 5
Store gradient.0(5, 1) = 6
Store gradient.0(6, 1) = 7
Store gradient.0(7, 1) = 8
Store gradient.0(0, 2) = 2
Store gradient.0(1, 2) = 3
Store gradient.0(2, 2) = 4
Store gradient.0(3, 2) = 5
Store gradient.0(4, 2) = 6
Store gradient.0(5, 2) = 7
Store gradient.0(6, 2) = 8
Store gradient.0(7, 2) = 9
Store gradient.0(0, 3) = 3
Store gradient.0(1, 3) = 4
Store gradient.0(2, 3) = 5
Store gradient.0(3, 3) = 6
Store gradient.0(4, 3) = 7
Store gradient.0(5, 3) = 8
Store gradient.0(6, 3) = 9
Store gradient.0(7, 3) = 10
Store gradient.0(0, 4) = 4
Store gradient.0(1, 4) = 5
Store gradient.0(2, 4) = 6
Store gradient.0(3, 4) = 7
Store gradient.0(4, 4) = 8
Store gradient.0(5, 4) = 9
Store gradient.0(6, 4) = 10
Store gradient.0(7, 4) = 11
Store gradient.0(0, 5) = 5
Store gradient.0(1, 5) = 6
Store gradient.0(2, 5) = 7
Store gradient.0(3, 5) = 8
Store gradient.0(4, 5) = 9
Store gradient.0(5, 5) = 10
Store gradient.0(6, 5) = 11
Store gradient.0(7, 5) = 12
Store gradient.0(0, 6) = 6
Store gradient.0(1, 6) = 7
Store gradient.0(2, 6) = 8
Store gradient.0(3, 6) = 9
Store gradient.0(4, 6) = 10
Store gradient.0(5, 6) = 11
Store gradient.0(6, 6) = 12
Store gradient.0(7, 6) = 13
Store gradient.0(0, 7) = 7
Store gradient.0(1, 7) = 8
Store gradient.0(2, 7) = 9
Store gradient.0(3, 7) = 10
Store gradient.0(4, 7) = 11
Store gradient.0(5, 7) = 12
Store gradient.0(6, 7) = 13
Store gradient.0(7, 7) = 14
End pipeline gradient.0()
```

</details>

This will print out all the times `gradient(x, y)` gets evaluated.

Now that we can snoop on what Halide is doing, let's try our first scheduling
primitive. We'll make a new version of gradient that processes each scanline in
parallel.

```haskell
  parallelGradient <- define "parallelGradient" (x, y) $ x + y
```

We'll also trace this function.

```haskell
  traceStores parallelGradient
```

Things are the same so far. We've defined the algorithm, but haven't said
anything about how to schedule it. In general, exploring different scheduling
decisions doesn't change the code that describes the algorithm.

Now we tell Halide to use a parallel for loop over the y coordinate. On Linux
we run this using a thread pool and a task queue. On OS X we call into grand
central dispatch, which does the same thing for us.

```haskell
  void $ parallel y parallelGradient
```

This time the printfs should come out of order, because each scanline is
potentially being processed in a different thread. The number of threads should
adapt to your system, but on linux you can control it manually using the
environment variable `HL_NUM_THREADS`.

```haskell
  putStrLn "Evaluating parallelGradient ..."
  realize parallelGradient [8, 8] . const $ pure ()
```

<details>
<summary>Show output...</summary>

```
Evaluating parallelGradient ...
Begin pipeline parallelGradient.0()
Tag parallelGradient.0() tag = "func_type_and_dim: 1 0 32 1 2 0 8 0 8"
Store parallelGradient.0(0, 0) = 0
Store parallelGradient.0(1, 0) = 1
Store parallelGradient.0(2, 0) = 2
Store parallelGradient.0(3, 0) = 3
Store parallelGradient.0(4, 0) = 4
Store parallelGradient.0(5, 0) = 5
Store parallelGradient.0(6, 0) = 6
Store parallelGradient.0(7, 0) = 7
Store parallelGradient.0(0, 4) = 4
Store parallelGradient.0(1, 4) = 5
Store parallelGradient.0(2, 4) = 6
Store parallelGradient.0(3, 4) = 7
Store parallelGradient.0(4, 4) = 8
Store parallelGradient.0(5, 4) = 9
Store parallelGradient.0(6, 4) = 10
Store parallelGradient.0(7, 4) = 11
Store parallelGradient.0(0, 3) = 3
Store parallelGradient.0(1, 3) = 4
Store parallelGradient.0(2, 3) = 5
Store parallelGradient.0(3, 3) = 6
Store parallelGradient.0(4, 3) = 7
Store parallelGradient.0(5, 3) = 8
Store parallelGradient.0(6, 3) = 9
Store parallelGradient.0(7, 3) = 10
Store parallelGradient.0(0, 7) = 7
Store parallelGradient.0(0, 5) = 5
Store parallelGradient.0(0, 6) = 6
Store parallelGradient.0(1, 7) = 8
Store parallelGradient.0(1, 5) = 6
Store parallelGradient.0(2, 5) = 7
Store parallelGradient.0(1, 6) = 7
Store parallelGradient.0(2, 7) = 9
Store parallelGradient.0(3, 5) = 8
Store parallelGradient.0(2, 6) = 8
Store parallelGradient.0(3, 7) = 10
Store parallelGradient.0(4, 5) = 9
Store parallelGradient.0(4, 7) = 11
Store parallelGradient.0(5, 5) = 10
Store parallelGradient.0(3, 6) = 9
Store parallelGradient.0(5, 7) = 12
Store parallelGradient.0(6, 5) = 11
Store parallelGradient.0(4, 6) = 10
Store parallelGradient.0(6, 7) = 13
Store parallelGradient.0(5, 6) = 11
Store parallelGradient.0(7, 5) = 12
Store parallelGradient.0(7, 7) = 14
Store parallelGradient.0(6, 6) = 12
Store parallelGradient.0(7, 6) = 13
Store parallelGradient.0(0, 1) = 1
Store parallelGradient.0(1, 1) = 2
Store parallelGradient.0(2, 1) = 3
Store parallelGradient.0(3, 1) = 4
Store parallelGradient.0(4, 1) = 5
Store parallelGradient.0(5, 1) = 6
Store parallelGradient.0(6, 1) = 7
Store parallelGradient.0(7, 1) = 8
Store parallelGradient.0(0, 2) = 2
Store parallelGradient.0(1, 2) = 3
Store parallelGradient.0(2, 2) = 4
Store parallelGradient.0(3, 2) = 5
Store parallelGradient.0(4, 2) = 6
Store parallelGradient.0(5, 2) = 7
Store parallelGradient.0(6, 2) = 8
Store parallelGradient.0(7, 2) = 9
End pipeline parallelGradient.0()
```

</details>
