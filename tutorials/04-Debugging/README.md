# Debugging with tracing, `printed`, and `printedWhen`

This lesson demonstrates how to follow what Halide is doing at runtime.

Since this README is also a literate Haskell file, we start with a few common imports.

```haskell
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
import Control.Monad (void)
import Data.Text (Text)
import Test.Hspec hiding (parallel)

import Language.Halide hiding (evaluate, mod, div)
import Prelude hiding (and)
```

### Printing out the value of Funcs as they are computed.

We'll define our gradient function as before.

```haskell
main :: IO ()
main = hspec $ describe "Tutorial 4" $ do
  it "Prints the value of Funcs when the are computed" $ do
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


### Printing individual Exprs.

`traceStores` can only print the value of a `Func`. Sometimes you want to
inspect the value of sub-expressions rather than the entire `Func`. The
function `printed` can be wrapped around any `Expr` to print the value of that
`Expr` every time it is evaluated.

For example, say we have some `Func` that is the sum of two terms:

```haskell
  it "Prints individual Exprs" $ do
    x <- mkVar "x"
    y <- mkVar "y"
    f <- define "f" (x, y) $ sin (cast @Float x) + cos (cast @Float y)
```

If we want to inspect just one of the terms, we can wrap it in a call to `printed`:

```haskell
    g <- define "g" (x, y) $ sin (cast @Float x) + printed (cos (cast @Float y))
    putStrLn "Evaluating sin(x) + cos(y), and just printing cos(y)..."
    realize g [4, 4] . const $ pure ()
```

<details>
<summary>Show output...</summary>

```
1.000000
1.000000
1.000000
1.000000
0.540302
0.540302
0.540302
0.540302
-0.416147
-0.416147
-0.416147
-0.416147
-0.989992
-0.989992
-0.989992
-0.989992
```

</details>


### Printing additional context

`printed` can take multiple arguments. It prints all of them and evaluates to
the first one. The arguments can be `Expr`s, `Text`, or `String`. This can be
used to print additional context alongside the value:

```haskell
  it "Prints additional context" $ do
    x <- mkVar "x"
    y <- mkVar "y"
    let second = printed (cos (cast @Float y)) ("<- this is cos(" :: Text)  y  (") when x =" :: Text) x
    f <- define "f" (x, y) $ sin (cast @Float x) + second
    putStrLn "Evaluating sin(x) + cos(y), and printing cos(y) with more context..."
    realize f [4, 4] . const $ pure ()
```

<details>
<summary>Show output...</summary>

```
1.000000 <- this is cos( 0 ) when x = 0
1.000000 <- this is cos( 0 ) when x = 1
1.000000 <- this is cos( 0 ) when x = 2
1.000000 <- this is cos( 0 ) when x = 3
0.540302 <- this is cos( 1 ) when x = 0
0.540302 <- this is cos( 1 ) when x = 1
0.540302 <- this is cos( 1 ) when x = 2
0.540302 <- this is cos( 1 ) when x = 3
-0.416147 <- this is cos( 2 ) when x = 0
-0.416147 <- this is cos( 2 ) when x = 1
-0.416147 <- this is cos( 2 ) when x = 2
-0.416147 <- this is cos( 2 ) when x = 3
-0.989992 <- this is cos( 3 ) when x = 0
-0.989992 <- this is cos( 3 ) when x = 1
-0.989992 <- this is cos( 3 ) when x = 2
-0.989992 <- this is cos( 3 ) when x = 3
```

</details>


### Conditional printing

Both `printed` and `traceStores` can produce a lot of output. If you're looking for
a rare event, or just want to see what happens at a single pixel, this amount
of output can be difficult to dig through. Instead, the function `printedWhen` can
be used to conditionally print an `Expr`. The first argument to `printedWhen`
is an `Expr Bool`. If the `Expr` evaluates to `True`, it returns the second
argument and prints all of the arguments. If the `Expr` evaluates to `False` it
just returns the second argument and does not print.

```haskell
  it "Conditionally prints Exprs" $ do
    x <- mkVar "x"
    y <- mkVar "y"
    let e = printedWhen (eq x 37 `and` eq y 42) (cos (cast @Float y)) ("<- this is cos(y) at x, y == (37, 42)" :: Text)
    f <- define "f" (x, y) $ sin (cast @Float x) + e
    putStrLn $ "Evaluating sin(x) + cos(y), and printing cos(y) at a single pixel..."
    realize f [640, 480] . const $ pure ()
```

<details>
<summary>Show output...</summary>

```
-0.399985 <- this is cos(y) at x, y == (37, 42)
```

</details>

`printedWhen` can also be used to check for values you're not expecting:

```haskell
    let e = cos (cast @Float y)
        e' = printedWhen (e `lt` 0) e ("cos(y) < 0 at y ==" :: Text) y
    g <- define "g" (x, y) $ sin (cast @Float x) + e'
    putStrLn $ "Evaluating sin(x) + cos(y), and printing whenever cos(y) < 0..."
    realize g [4, 4] . const $ pure ()
```

<details>
<summary>Show output...</summary>

```
-0.416147 cos(y) < 0 at y == 2
-0.416147 cos(y) < 0 at y == 2
-0.416147 cos(y) < 0 at y == 2
-0.416147 cos(y) < 0 at y == 2
-0.989992 cos(y) < 0 at y == 3
-0.989992 cos(y) < 0 at y == 3
-0.989992 cos(y) < 0 at y == 3
-0.989992 cos(y) < 0 at y == 3
```

</details>


### Printing expressions at compile-time.

The code below builds up a Halide expression across several lines of code. If
you're programmatically constructing a complex expression, and you want to
check the expression you've created is what you think it is, you can also print
out the expression itself the `Show` typeclass:

```haskell
  it "Prints expressions at compile-time" $ do
    fizz <- mkVar "fizz"
    buzz <- mkVar "buzz"
    let combine e i
          | i `mod` 3 == 0 && i `mod` 5 == 0 = e + fizz * buzz
          | i `mod` 3 == 0 = e + fizz
          | i `mod` 5 == 0 = e + buzz
          | otherwise = e + mkExpr i
    let e = foldl combine 1 [2 .. 99]
    print e
```

<details>
<summary>Show output...</summary>

```
((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((((1 + 2) + fizz) + 4) + buzz) + fizz) + 7) + 8) + fizz) + buzz) + 11) + fizz) + 13) + 14) + (fizz*buzz)) + 16) + 17) + fizz) + 19) + buzz) + fizz) + 22) + 23) + fizz) + buzz) + 26) + fizz) + 28) + 29) + (fizz*buzz)) + 31) + 32) + fizz) + 34) + buzz) + fizz) + 37) + 38) + fizz) + buzz) + 41) + fizz) + 43) + 44) + (fizz*buzz)) + 46) + 47) + fizz) + 49) + buzz) + fizz) + 52) + 53) + fizz) + buzz) + 56) + fizz) + 58) + 59) + (fizz*buzz)) + 61) + 62) + fizz) + 64) + buzz) + fizz) + 67) + 68) + fizz) + buzz) + 71) + fizz) + 73) + 74) + (fizz*buzz)) + 76) + 77) + fizz) + 79) + buzz) + fizz) + 82) + 83) + fizz) + buzz) + 86) + fizz) + 88) + 89) + (fizz*buzz)) + 91) + 92) + fizz) + 94) + buzz) + fizz) + 97) + 98) + fizz)
```

</details>
