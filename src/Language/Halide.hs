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

    -- * Functions
  , Func (..)
  , FuncTy (..)
  , Stage (..)

    -- ** Creating
  , define
  , update
  , (!)

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
  , realize
  , compile

    -- ** Parameters
  , scalar
  , buffer
  , Dimension (..)
  , setMin
  , setExtent
  , setStride
  , setEstimate
  , asBufferParam

    -- ** Targets
  , compileForTarget
  , hostTarget
  , gpuTarget
  , Target (..)
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
  , dim
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

    -- * Internal
  , IsTuple (..)
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
  , bufferCopyToHost
  , module Language.Halide.Schedule

    -- ** inline-c helpers
  , importHalide

    -- * Convenience re-exports
  , Int32
  )
where

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
