# halide-haskell

[![GitHub CI](https://github.com/twesterhout/halide-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/twesterhout/halide-haskell/actions/workflows/ci.yml)
<!-- [![Hackage](https://img.shields.io/hackage/v/halide-haskell.svg?logo=haskell)](https://hackage.haskell.org/package/halide-haskell)-->
[![BSD-3-Clause license](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)

This package contains wrappers for the [Halide C++ library](https://halide-lang.org/) that allow you compile and run Halide pipelines directly from your Haskell program: no need to write any C++ code!

As a simple example, here's how you could implement vector addition using halide-haskell:

```haskell
mkVectorPlus :: (IsHalideType a, Num a) => IO (S.Vector a -> S.Vector a -> S.Vector a)
mkVectorPlus = do
  -- First, compile the kernel
  kernel <- mkKernel $ \a b -> do
    -- Create an index variable
    i <- mkVar "i"
    -- Define the resulting function. We call it "out".
    -- In pseudocode it's equivalent to the following: out[i] = a[i] + b[i]
    define "out" i $ a ! i + b ! i
  -- Create a Haskell function that will invoke the kernel
  pure $ \v1 v2 -> unsafePerformIO $ do
    out <- SM.new (S.length v1)
    withHalideBuffer v1 $ \a ->
      withHalideBuffer v2 $ \b ->
        withHalideBuffer out $ \out' ->
          kernel a b out'
    S.unsafeFreeze out

-- Example usage of our Halide kernel
main :: IO ()
main = do
  let a, b :: S.Vector Float
      a = S.fromList [1, 2, 3]
      b = S.fromList [4, 5, 6]
  -- This compiles the code (it can take a bit of time)
  vectorPlus <- mkVectorPlus
  -- Running the code (this is fast)
  print (vectorPlus a b)
```

## Contributing

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



