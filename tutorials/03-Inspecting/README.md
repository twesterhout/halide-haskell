# Tutorial 3: Inspecting the generated code

This lesson demonstrates how to inspect what the Halide compiler is producing.

Since this README is also a literate Haskell file, we start with a few common imports.

```haskell
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
import Control.Exception (evaluate)
import Data.Text.IO (hPutStr)
import System.IO (withFile, IOMode (..))
import Test.Hspec

import Language.Halide hiding (evaluate)
```

We'll start by defining the simple single-stage imaging pipeline from [Tutorial 1](../01-Basics).

This lesson will be about debugging, but unfortunately in Haskell, objects
don't know their own names, which makes it hard for us to understand the
generated code. To get around this, we specify names as `Text` when
constructing `Func` and `Var` objects.

```haskell
main :: IO ()
main = hspec $ specify "Tutorial 3" $ do
  x <- mkVar "x"
  y <- mkVar "y"
  gradient <- define "gradient" (x, y) $ x + y
```

Realize the function to produce an output image. We'll keep it very small for
this lesson. We also won't need the result, so we tell Halide to do nothing
with the generated buffer.

```haskell
  realize gradient [8, 8] $ const (pure ())
```

That line compiled and ran the pipeline. Try running this lesson with the
environment variable `HL_DEBUG_CODEGEN` set to 1. It will print out the various
stages of compilation, and a pseudocode representation of the final pipeline.

<details><summary>Generated output</summary>

```
Creating initial loop nests...
Injecting realization of { gradient }
Skipping injecting memoization...
Injecting tracing...
Adding checks for parameters
Computing bounds of each function's value
Clamping unsafe data-dependent accesses
Performing computation bounds inference...
Removing extern loops...
Performing sliding window optimization...
Uniquifying variable names...
Simplifying...
Simplifying correlated differences...
Performing allocation bounds inference...
Adding checks for images
Removing code that depends on undef values...
Performing storage folding optimization...
Injecting debug_to_file calls...
Injecting prefetches...
Discarding safe promises...
Dynamically skipping stages...
Forking asynchronous producers...
Destructuring tuple-valued realizations...
Bounding small realizations...
Performing storage flattening...
Adding atomic mutex allocation...
Unpacking buffer arguments...
Skipping rewriting memoized allocations...
Simplifying...
Reduce prefetch dimension...
Simplifying correlated differences...
Unrolling...
Vectorizing...
Detecting vector interleavings...
Partitioning loops to simplify boundary conditions...
Staging strided loads...
Trimming loops to the region over which they do something...
Rebasing loops to zero...
Hoisting loop invariant if statements...
Injecting early frees...
Simplifying correlated differences...
Bounding small allocations...
Simplifying...
Lowering unsafe promises...
Flattening nested ramps...
Removing dead allocations and moving loop invariant code...
Finding intrinsics...
Hoisting prefetches...
Lowering after final simplification:
assert(reinterpret<uint64>((struct halide_buffer_t *)gradient.buffer) != (uint64)0, halide_error_buffer_argument_is_null("gradient"))
let gradient = (void *)_halide_buffer_get_host((struct halide_buffer_t *)gradient.buffer)
let gradient.type = (uint32)_halide_buffer_get_type((struct halide_buffer_t *)gradient.buffer)
let gradient.device_dirty = (uint1)_halide_buffer_get_device_dirty((struct halide_buffer_t *)gradient.buffer)
let gradient.dimensions = _halide_buffer_get_dimensions((struct halide_buffer_t *)gradient.buffer)
let gradient.min.0 = _halide_buffer_get_min((struct halide_buffer_t *)gradient.buffer, 0)
let gradient.extent.0 = _halide_buffer_get_extent((struct halide_buffer_t *)gradient.buffer, 0)
let gradient.stride.0 = _halide_buffer_get_stride((struct halide_buffer_t *)gradient.buffer, 0)
let gradient.min.1 = _halide_buffer_get_min((struct halide_buffer_t *)gradient.buffer, 1)
let gradient.extent.1 = _halide_buffer_get_extent((struct halide_buffer_t *)gradient.buffer, 1)
let gradient.stride.1 = _halide_buffer_get_stride((struct halide_buffer_t *)gradient.buffer, 1)
if ((uint1)_halide_buffer_is_bounds_query((struct halide_buffer_t *)gradient.buffer)) {
 (struct halide_buffer_t *)_halide_buffer_init((struct halide_buffer_t *)gradient.buffer, (struct halide_dimension_t *)_halide_buffer_get_shape((struct halide_buffer_t *)gradient.buffer), reinterpret<(void *)>((uint64)0), (uint64)0, reinterpret<(struct halide_device_interface_t *)>((uint64)0), 0, 32, 2, (struct halide_dimension_t *)make_struct(gradient.min.0, gradient.extent.0, 1, 0, gradient.min.1, gradient.extent.1, gradient.extent.0, 0), (uint64)0)
}
if (!(uint1)_halide_buffer_is_bounds_query((struct halide_buffer_t *)gradient.buffer)) {
 assert(gradient.type == (uint32)73728, halide_error_bad_type("Output buffer gradient", gradient.type, (uint32)73728))
 assert(gradient.dimensions == 2, halide_error_bad_dimensions("Output buffer gradient", gradient.dimensions, 2))
 assert(0 <= gradient.extent.0, halide_error_buffer_extents_negative("Output buffer gradient", 0, gradient.extent.0))
 assert(0 <= gradient.extent.1, halide_error_buffer_extents_negative("Output buffer gradient", 1, gradient.extent.1))
 assert(gradient.stride.0 == 1, halide_error_constraint_violated("gradient.stride.0", gradient.stride.0, "1", 1))
 let gradient.total_extent.1 = int64(gradient.extent.1)*int64(gradient.extent.0)
 assert(uint64(gradient.extent.0) <= (uint64)2147483647, halide_error_buffer_allocation_too_large("gradient", uint64(gradient.extent.0), (uint64)2147483647))
 assert((uint64)abs(int64(gradient.extent.1)*int64(gradient.stride.1)) <= (uint64)2147483647, halide_error_buffer_allocation_too_large("gradient", (uint64)abs(int64(gradient.extent.1)*int64(gradient.stride.1)), (uint64)2147483647))
 assert(gradient.total_extent.1 <= (int64)2147483647, halide_error_buffer_extents_too_large("gradient", gradient.total_extent.1, (int64)2147483647))
 assert(!gradient.device_dirty, halide_error_device_dirty_with_no_device_support("Output buffer gradient"))
 assert(gradient != reinterpret<(void *)>((uint64)0), halide_error_host_is_null("Output buffer gradient"))
 produce gradient {
  let t2 = 0 - (gradient.min.1*gradient.stride.1)
  let t1 = gradient.min.0 + gradient.min.1
  for (gradient.s0.y.rebased, 0, gradient.extent.1) {
   let t4 = ((gradient.min.1 + gradient.s0.y.rebased)*gradient.stride.1) + t2
   let t3 = gradient.s0.y.rebased + t1
   for (gradient.s0.x.rebased, 0, gradient.extent.0) {
    gradient[gradient.s0.x.rebased + t4] = gradient.s0.x.rebased + t3
   }
  }
 }
}

Skipping Hexagon offload...
Skipping GPU offload...
Lowering Parallel Tasks...
Target triple of initial module: x86_64--linux-gnu
Generating llvm bitcode...
Generating llvm bitcode prolog for function gradient...
Generating llvm bitcode for function gradient...
JIT compiling shared runtime for x86-64-linux-avx-avx2-f16c-fma-jit-sse41-user_context
JIT compiling gradient for x86-64-linux-avx-avx2-f16c-fma-jit-sse41-user_context
```

</details>

If you set `HL_DEBUG_CODEGEN` to a higher number, you can see more and more
details of how Halide compiles your pipeline. Setting `HL_DEBUG_CODEGEN=2` shows
the Halide code at each stage of compilation, and also the llvm bitcode we
generate at the end.

Halide can also output an HTML version of this output, which supports syntax
highlighting and code-folding, so it can be nicer to read for large pipelines.
Open `gradient.html` with your browser after running this tutorial.

```haskell
  withFile "gradient.html" WriteMode $ \h ->
    hPutStr h =<< compileToLoweredStmt StmtHTML hostTarget (evaluate gradient)
```

You can usually figure out what code Halide is generating using this
pseudocode. In the next lesson we'll see how to snoop on Halide at runtime.
