-- |
-- Module      : Language.Halide
-- Copyright   : (c) Tom Westerhout, 2023
--
-- This package provides Haskell bindings that allow to write Halide embedded in Haskell without C++.
--
-- This module contains the reference documentation for Halide. If you're new, the best way to learn Halide is to have a look at the [tutorials](https://github.com/twesterhout/halide-haskell/tree/master/tutorials).
module Language.Halide
  ( -- * Scalar expressions

    -- | The basic building block of Halide pipelines is 'Expr'. @Expr a@ represents a scalar expression of
    -- type @a@, where @a@ must be an instance of 'IsHalideType'.
    Expr (..)
  , Var
  , RVar
  , VarOrRVar
  , IsHalideType

    -- ** Creating
  , mkExpr
  , mkVar
  , mkRVar
  , undef
  , cast
  , bool

    -- ** Inspecting
  , toIntImm
  , printed
  , printedWhen
  , evaluate

    -- ** Comparisons

    -- | We can't use 'Eq' and 'Ord' instances here, because we want the comparison to happen
    -- when the pipeline is run rather than when it's built. Hence, we define lifted version of
    -- various comparison operators. Note, that infix versions of the these functions have the
    -- same precedence as the normal comparison operators.
  , eq
  , neq
  , lt
  , lte
  , gt
  , gte
  , min
  , max

    -- ** Boolean functions
  , and
  , or

    -- * Functions
  , Func (..)
  , FuncTy (..)
  , Stage (..)

    -- ** Creating
  , define
  , update
  , (!)
  , repeatEdge
  , constantExterior

    -- ** Inspecting
  , getArgs
  , hasUpdateDefinitions
  , getUpdateStage

    -- * Buffers

    -- | In the C interface of Halide, buffers are described by the C struct
    -- [@halide_buffer_t@](https://halide-lang.org/docs/structhalide__buffer__t.html). On the Haskell side,
    -- we have 'HalideBuffer'.
  , HalideBuffer (..)
    -- | To easily test out your pipeline, there are helper functions to create 'HalideBuffer's without
    -- worrying about the low-level representation.
  , allocaCpuBuffer
  , allocaBuffer
    -- | Buffers can also be converted to lists to easily print them for debugging.
  , IsListPeek (..)
    -- | For production usage however, you don't want to work with lists. Instead, you probably want Halide
    -- to work with your existing array data types. For this, we define 'IsHalideBuffer' typeclass that
    -- teaches Halide how to convert your data into a 'HalideBuffer'. Depending on how you implement the
    -- instance, this can be very efficient, because it need not involve any memory copying.
  , IsHalideBuffer (..)
  , withHalideBuffer
    -- | There are also helper functions to simplify writing instances of 'IsHalideBuffer'.
  , bufferFromPtrShapeStrides
  , bufferFromPtrShape

    -- * Running the pipelines

    -- | There are a few ways how one can run a Halide pipeline.
    --
    -- The simplest way to build a t'Func' and then call 'realize' to evaluate it over a rectangular domain.
  , realize
  , realizeOnTarget
  , asBufferParam
    -- | The drawback of calling 'realize' all the time is that it's impossible to pass parameters to pipelines.
    -- We can define pipelines that operate on buffers using 'asBufferParam', but we have to recompile the
    -- pipeline for every new buffer.
    --
    -- A better way to handle pipeline parameters is to define a /Haskell/ function that accepts t'Expr's
    -- and t'Func's as arguments and returns a 'Func'. We can then pass this function to 'compile'
    -- (or 'compileForTarget'), and it compile it into a /Haskell/ function that can now be invoked with
    -- normal scalars instead of t'Expr's and @Ptr 'HalideBuffer'@s instead of 'Func's.
  , compile

    -- ** Parameters

    -- | Similar to how we can specify the name of a variable in 'mkVar' (or 'mkRVar') or function in 'define',
    -- one can also specify the name of a pipeline parameter. This is achieved by using the @ViewPatterns@
    -- extension together with the 'scalar' and 'buffer' helper functions.
  , buffer
  , scalar
    -- | Another common thing to do with the parameters is to explicitly specify their shapes. For this, we expose the 'Dimension' type:
  , Dimension (..)
  , dim
  , setMin
  , setExtent
  , setStride
  , setEstimate

    -- ** Targets
  , Target (..)
  , hostTarget
  , gpuTarget
  , compileForTarget
  , DeviceAPI (..)
  , TargetFeature (..)
  , setFeature
  , hasGpuFeature
  , hostSupportsTargetDevice

    -- * Scheduling
  , Schedulable (..)
  , TailStrategy (..)
  , LoopLevel (..)
  , LoopLevelTy (..)
  , LoopAlignStrategy (..)
  , computeRoot
  , getStage
  , getLoopLevel
  , getLoopLevelAtStage
  , asUsed
  , asUsedBy
  , copyToDevice
  , copyToHost
  , storeAt
  , computeAt
  , estimate
  , bound

    -- * Debugging / Tracing

    -- | For debugging, it's often useful to observe the value of an expression when it's evaluated. If you
    -- have a complex expression that does not depend on any buffers or indices, you can 'evaluate' it.
    -- | However, often an expression is only used within a definition of a pipeline, and it's impossible to
    -- call 'evaluate' on it. In such cases, it can be wrapped with 'printed' to indicate to Halide that the
    -- value of the expression should be dumped to screen when it's computed.
  , prettyLoopNest
  , compileToLoweredStmt
  , StmtOutputFormat (..)
  , TraceEvent (..)
  , TraceEventCode (..)
  , TraceLoadStoreContents (..)
  , setCustomTrace
  , traceStores
  , traceLoads
  , collectIterationOrder

    -- * Type helpers
  , IsTuple (..)
  , ToTuple
  , FromTuple
  , IndexTuple
  , Length
  , All

    -- * Internal
  , compileToCallable
  , testCUDA
  , testOpenCL
  , SomeLoopLevel (..)
  , RawHalideBuffer (..)
  , HalideDimension (..)
  , HalideDeviceInterface
  , rowMajorStrides
  , colMajorStrides
  , isDeviceDirty
  , isHostDirty
  , getBufferExtent
  , bufferCopyToHost
  , withCopiedToHost
  , withCropped
  , module Language.Halide.Schedule
  , IsFuncBuilder
  , ReturnsFunc
  , FunctionArguments
  , FunctionReturn
  , Curry (..)
  , UnCurry (..)
  , Lowered

    -- ** inline-c helpers
  , importHalide
  , testWriteToStderr
  , CxxExpr
  , CxxVar
  , CxxRVar
  , CxxParameter
  , CxxFunc
  , CxxImageParam
  , CxxStage
  , CxxDimension
  , CxxTarget
  , CxxLoopLevel

    -- * Convenience re-exports
  , Int32
  , Ptr
  , KnownNat
  )
where

import Foreign.Ptr (Ptr)
import GHC.TypeLits (KnownNat)
import Language.Halide.BoundaryConditions
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Dimension
import Language.Halide.Expr
import Language.Halide.Func
import Language.Halide.Kernel
import Language.Halide.LoopLevel
import Language.Halide.Schedule
import Language.Halide.Target
import Language.Halide.Trace
import Language.Halide.Type
import Prelude ()
