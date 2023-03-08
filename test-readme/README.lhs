# halide-haskell

[![GitHub CI](https://github.com/twesterhout/halide-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/twesterhout/halide-haskell/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/halide-haskell.svg?logo=haskell)](https://hackage.haskell.org/package/halide-haskell-0.0.1.0/candidate)
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

[Halide](https://halide-lang.org/) is a programming language designed to make
it easier to write high-performance image and array processing code on modern
machines. Rather than being a standalone programming language, Halide is
embedded in C++. This means you write C++ code that builds an in-memory
representation of a Halide pipeline using Halide's C++ API. You can then
compile this representation to an object file, or JIT-compile it and run it in
the same process.

**This package provides Haskell bindings that allow to write Halide embedded in
Haskell without C++** 😋.

  - [Tutorials](https://github.com/twesterhout/halide-haskell/tree/master/tutorials)
  - [Reference documentation](https://hackage.haskell.org/package/halide-haskell-0.0.1.0)

## 🚀 Getting started

As a simple example, here's how you could implement array addition with halide-haskell:

```haskell
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, OverloadedStrings #-}
import Language.Halide

-- The algorithm
mkArrayPlus = compile $ \a b -> do
  -- Create an index variable
  i <- mkVar "i"
  -- Define the resulting function. We call it "out".
  -- In pseudocode it's equivalent to the following: out[i] = a[i] + b[i]
  out <- define "out" i $ a ! i + b ! i
  -- Perform a fancy optimization and use SIMD: we split the loop over i into
  -- an inner and an outer loop and then vectorize the inner loop
  inner <- mkVar "inner"
  split TailAuto i (i, inner) 4 out >>= vectorize inner

-- Example usage of our Halide pipeline
main :: IO ()
main = do
  let a, b :: [Float]
      a = [1, 2, 3, 4, 5]
      b = [6, 7, 8, 9, 10]
  -- Compile the code
  arrayPlus <- mkArrayPlus
  -- We tell Halide to treat our list as a one-dimensional buffer
  withHalideBuffer @1 @Float a $ \a' ->
    withHalideBuffer b $ \b' ->
      -- allocate a temporary buffer for the output
      allocaCpuBuffer [length a] $ \out' -> do
        -- execute the kernel -- it is a normal function call!
        arrayPlus a' b' out'
        -- print the result
        print =<< peekToList out'
```

For more examples, have a look a the [tutorials](https://github.com/twesterhout/halide-haskell/tree/master/tutorials).

## 🔨 Contributing

Currently, the best way to get started is to use Nix:

```sh
nix develop
```

This will drop you into a shell with all the necessary tools to build the code such that you can do

```sh
cabal build
```

and

```sh
cabal test
```
