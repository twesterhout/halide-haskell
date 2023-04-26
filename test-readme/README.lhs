<h1 align="center">
halide-haskell
</h1>

<div align="center">

<img src="assets/construction_1f6a7.png" width="32">This project is still under heavy development and might not be production-ready<img src="assets/construction_1f6a7.png" width="32"><br>
We encourage experimenting with it and reporting any issues you run into via
[Github issues](https://github.com/twesterhout/halide-haskell/issues).

<br />

[**Tutorials**](https://github.com/twesterhout/halide-haskell/tree/master/tutorials) | [**Documentation**](https://hackage.haskell.org/package/halide-haskell-0.0.1.0) | [**Showcases**](https://github.com/twesterhout/halide-haskell-examples)

[![license](https://img.shields.io/github/license/twesterhout/halide-haskell.svg?style=flat-square)](LICENSE)
[![build](https://img.shields.io/github/actions/workflow/status/twesterhout/halide-haskell/ci.yml?style=flat-square)](https://github.com/twesterhout/halide-haskell/actions/workflows/ci.yml)
[![Hackage](https://img.shields.io/hackage/v/halide-haskell?style=flat-square)](https://hackage.haskell.org/package/halide-haskell)

</div>

<table>
<tr>
<td>

[Halide](https://halide-lang.org/) is a programming language designed to make
it easier to write high-performance image and array processing code on modern
machines. Rather than being a standalone programming language, Halide is
embedded in C++. This means you write C++ code that builds an in-memory
representation of a Halide pipeline using Halide's C++ API. You can then
compile this representation to an object file, or JIT-compile it and run it in
the same process.

</td>
</tr>
</table>

<h4 align="center" >
This package provides Haskell bindings that allow to write Halide embedded in
Haskell without C++ 😋.
</h4>

  - [Example usage](#-example-usage)
  - [Installing](#-installing)
  - [Motivation](#-motivation)
  - [Contributing](#-contributing)


## 🚀 Example usage

As a simple example, here's how you could implement array addition with halide-haskell:

```haskell
{-# LANGUAGE AllowAmbiguousTypes, DataKinds, OverloadedStrings, ViewPatterns #-}
import Language.Halide

-- The algorithm
mkArrayPlus = compile $ \(buffer "a" -> a) (buffer "b" -> b) -> do
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

For more examples, have a look at the [tutorials](https://github.com/twesterhout/halide-haskell/tree/master/tutorials).

## 💻 Installing

Currently, the easiest way to install the library is using
[Nix](https://nixos.org/). It is not a fundamental limitation, because the
library itself is just a normal Cabal-based Haskell project, but installing &
patching (not all our bug fixes have been upstreamed yet) the system
dependencies is just too much work without Nix.

So, once you have Nix installed, you can add halide-haskell to your flake
inputs like [this project
demonstrates](https://github.com/twesterhout/halide-haskell-examples/blob/main/flake.nix#L27)
and then include it in your `build-depends` section in the Cabal file.

If you just want to try building the library, type

```sh
nix build
```

and to run an example, try

```sh
nix run
nix run .#ghc927-intel-ocl.halide-haskell # for Intel OpenCL support
nix run .#ghc927-cuda.halide-haskell      # for CUDA support
nix run .#ghc944.halide-haskell           # to build with GHC 9.4.4 instead
```

(for OpenCL and CUDA, you may need to set `NIXPKGS_ALLOW_UNFREE=1`)

## 🤩 Motivation

The availability of Deep Learning frameworks such as
[PyTorch](https://pytorch.org/) or [JAX](https://github.com/google/jax) has
revolutionized array processing, independently of whether one works on Machine
Learning tasks or other numerical algorithms. The ecosystem in Haskell has been
catching up as well, and there are now multiple good array
libraries ([hmatrix](https://github.com/haskell-numerics/hmatrix),
[massiv](https://github.com/lehins/massiv),
[Accelerate](https://www.acceleratehs.org/),
[arrayfire-haskell](https://github.com/arrayfire/arrayfire-haskell),
[Hasktorch](https://github.com/hasktorch/hasktorch), are all high-quality
libraries). To accommodate multiple domains, such libraries
have to support hundreds if not thousands of operations (e.g. there are more
than 3.5 thousand of so called [“native” functions in PyTorch](https://github.com/pytorch/pytorch/blob/6a09847c42bf7d33ba0aea5b083eebd846661ce1/aten/src/ATen/native/native_functions.yaml)),
and this count does not include specializations for different device and data
types).

To overcome this difficulty, we propose to build a common extension mechanism
for Haskell array libraries. The mechanism is based on embedding the
[Halide](https://halide-lang.org/) language into Haskell that allows to
just-in-time (JIT) compile computational kernels for various hardware.

### 🤨 Why not Accelerate?

One might wonder, why write another package instead of relying on
[Accelerate](https://www.acceleratehs.org/) for the JIT compilation of the
kernels. Accelerate is a Haskell eDSL (embedded Domain Specific Language) for
collective operations on dense multi-dimensional arrays. It relies on
[LLVM](https://llvm.org/) to JIT compile the computational kernels for the
target architecture including multicore CPUs and GPUs. Users have to rely on
Accelerate to generate high-performance kernels and have no way to force some
low-level optimizations. For example, [Trevor L. McDonell et
al.](https://doi.org/10.1145/2887747.2804313) explain that the reason why
hand-written [CUDA](https://www.nvidia.com/en-gb/geforce/technologies/cuda/)
implementation of the [N-body
problem](https://en.wikipedia.org/wiki/N-body_problem) outperforms Accelerate
is the use of on-chip shared memory. Another example would be the matrix-matrix
product where achieving maximal performance requires writing no fewer than six
nested loops instead of the naive three ([ACM Trans. Math. Softw. 34, 3,
Article 12 (May 2008)](https://doi.org/10.1145/1356052.1356053)).
Accelerate has no way of knowing that such optimizations have to be applied and
cannot perform them automatically, and this is precisely the gap that we are
trying to fill by embedding Halide into Haskell.

Halide is a C++ eDSL for high-performance image and array processing. Its core
idea is to decouple the *algorithm* (i.e. what is computed) from the *schedule*
(i.e. where and when it is computed). The eDSL allows to quickly prototype and
test the algorithm and then move on to the optimization. Optimizations such as
fusion, tiling, parallelism and vectorization can be freely explored without
the risk of breaking the original algorithm definition. Schedulers can also be
generated automatically by [advanced optimization
algorithms](https://halide-lang.org/papers/autoscheduler2019.html)

Halide provides a lower level interface than Accelerate and thus does not aim
to replace it. Instead, Halide can be used to extend Accelerate, and later on
one might even think about using Halide as a backend for Accelerate.

## 🔨 Contributing

Currently, the best way to get started is to use Nix:

```sh
nix develop
```

This will drop you into a shell with all the necessary tools to build the code
such that you can do

```sh
cabal build
```

and

```sh
cabal test
```
