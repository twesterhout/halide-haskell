{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Language.Halide.Target
-- Description : Compilation target (i.e. machine and OS)
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Target
  ( DeviceAPI (..)
  , TargetFeature (..)
  , testCUDA
  , testOpenCL
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.IORef
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Type
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (tail)

importHalide

-- | An enum describing a type of device API.
data DeviceAPI
  = DeviceNone
  | DeviceHost
  | DeviceDefaultGPU
  | DeviceCUDA
  | DeviceOpenCL
  | DeviceOpenGLCompute
  | DeviceMetal
  | DeviceHexagon
  | DeviceHexagonDma
  | DeviceD3D12Compute
  deriving stock (Show, Eq, Ord)

-- | A test that tries to compile and run a Halide pipeline using 'FeatureCUDA'.
--
-- This is implemented fully in C++ to make sure that we test the installation
-- rather than our Haskell code.
--
-- On non-NixOS systems one should do the following:
--
-- > nixGLNvidia cabal repl --ghc-options='-fobject-code -O0'
-- > ghci> testCUDA
testCUDA :: IO ()
testCUDA = do
  handleHalideExceptionsM $
    [C.tryBlock| void {
      handle_halide_exceptions([](){
        // Define a gradient function.
        Halide::Func f;
        Halide::Var x, y, xo, xi, yo, yi;
        f(x, y) = x + y;
        // Schedule f on the GPU in 16x16 tiles.
        f.gpu_tile(x, y, xo, yo, xi, yi, 16, 16);
        // Construct a target that uses the GPU.
        Halide::Target target = Halide::get_host_target();
        // Set CUDA as the GPU backend.
        target.set_feature(Halide::Target::CUDA);
        // Enable debugging so that you can see what CUDA API calls we do.
        target.set_feature(Halide::Target::Debug);
        // JIT-compile the pipeline.
        f.compile_jit(target);
        // Run it.
        Halide::Buffer<int> result = f.realize({32, 32});
        // Check correctness
        for (int y = 0; y < result.height(); y++) {
            for (int x = 0; x < result.width(); x++) {
                if (result(x, y) != x + y) {
                    printf("result(%d, %d) = %d instead of %d\n",
                           x, y, result(x, y), x + y);
                }
            }
        }
      });
    } |]

-- | Similar to 'testCUDA' but for 'FeatureOpenCL'.
testOpenCL :: IO ()
testOpenCL = do
  handleHalideExceptionsM $
    [C.tryBlock| void {
      handle_halide_exceptions([](){
        Halide::Func f;
        Halide::Var x, y, xo, xi, yo, yi;
        f(x, y) = x + y;
        f.gpu_tile(x, y, xo, yo, xi, yi, 4, 4);
        Halide::Target target = Halide::get_host_target();
        target.set_feature(Halide::Target::OpenCL);
        target.set_feature(Halide::Target::Debug);
        fprintf(stderr, "Compiling ...\n");
        f.compile_jit(target);
        fprintf(stderr, "Running on OpenCL ...\n");
        Halide::Buffer<int> result = f.realize({32, 32});
        for (int y = 0; y < result.height(); y++) {
            for (int x = 0; x < result.width(); x++) {
                if (result(x, y) != x + y) {
                    printf("result(%d, %d) = %d instead of %d\n",
                           x, y, result(x, y), x + y);
                }
            }
        }
      });
    } |]

-- |
--
-- Note: generated automatically using
-- > cat $HALIDE_PATH/include/Halide.h | \
-- >   grep -E '.* = halide_target_feature_.*' | \
-- >   sed -E 's/^\s*(.*) = .*$/  | \1/g' | \
-- >   grep -v 'FeatureEnd'
data TargetFeature
  = FeatureJIT
  | FeatureDebug
  | FeatureNoAsserts
  | FeatureNoBoundsQuery
  | FeatureSSE41
  | FeatureAVX
  | FeatureAVX2
  | FeatureFMA
  | FeatureFMA4
  | FeatureF16C
  | FeatureARMv7s
  | FeatureNoNEON
  | FeatureVSX
  | FeaturePOWER_ARCH_2_07
  | FeatureCUDA
  | FeatureCUDACapability30
  | FeatureCUDACapability32
  | FeatureCUDACapability35
  | FeatureCUDACapability50
  | FeatureCUDACapability61
  | FeatureCUDACapability70
  | FeatureCUDACapability75
  | FeatureCUDACapability80
  | FeatureCUDACapability86
  | FeatureOpenCL
  | FeatureCLDoubles
  | FeatureCLHalf
  | FeatureCLAtomics64
  | FeatureOpenGLCompute
  | FeatureEGL
  | FeatureUserContext
  | FeatureProfile
  | FeatureNoRuntime
  | FeatureMetal
  | FeatureCPlusPlusMangling
  | FeatureLargeBuffers
  | FeatureHexagonDma
  | FeatureHVX_128
  | FeatureHVX_v62
  | FeatureHVX_v65
  | FeatureHVX_v66
  | FeatureHVX_shared_object
  | FeatureFuzzFloatStores
  | FeatureSoftFloatABI
  | FeatureMSAN
  | FeatureAVX512
  | FeatureAVX512_KNL
  | FeatureAVX512_Skylake
  | FeatureAVX512_Cannonlake
  | FeatureAVX512_SapphireRapids
  | FeatureTraceLoads
  | FeatureTraceStores
  | FeatureTraceRealizations
  | FeatureTracePipeline
  | FeatureD3D12Compute
  | FeatureStrictFloat
  | FeatureTSAN
  | FeatureASAN
  | FeatureCheckUnsafePromises
  | FeatureEmbedBitcode
  | FeatureEnableLLVMLoopOpt
  | FeatureWasmSimd128
  | FeatureWasmSignExt
  | FeatureWasmSatFloatToInt
  | FeatureWasmThreads
  | FeatureWasmBulkMemory
  | FeatureSVE
  | FeatureSVE2
  | FeatureARMDotProd
  | FeatureARMFp16
  | FeatureRVV
  | FeatureARMv81a
  | FeatureSanitizerCoverage
  | FeatureProfileByTimer
  | FeatureSPIRV
  | FeatureSemihosting
  deriving stock (Eq, Show, Ord)

instance Enum TargetFeature where
  -- Generated using
  -- cat $HALIDE_PATH/include/Halide.h | grep -E '.* = halide_target_feature_.*' | grep -v 'FeatureEnd' | sed -E 's/[ \t]+(.*) = ([^,]*).*/    Feature\1 -> [CU.pure| int { \2 } |]/'
  fromEnum =
    fromIntegral . \case
      FeatureJIT -> [CU.pure| int { halide_target_feature_jit } |]
      FeatureDebug -> [CU.pure| int { halide_target_feature_debug } |]
      FeatureNoAsserts -> [CU.pure| int { halide_target_feature_no_asserts } |]
      FeatureNoBoundsQuery -> [CU.pure| int { halide_target_feature_no_bounds_query } |]
      FeatureSSE41 -> [CU.pure| int { halide_target_feature_sse41 } |]
      FeatureAVX -> [CU.pure| int { halide_target_feature_avx } |]
      FeatureAVX2 -> [CU.pure| int { halide_target_feature_avx2 } |]
      FeatureFMA -> [CU.pure| int { halide_target_feature_fma } |]
      FeatureFMA4 -> [CU.pure| int { halide_target_feature_fma4 } |]
      FeatureF16C -> [CU.pure| int { halide_target_feature_f16c } |]
      FeatureARMv7s -> [CU.pure| int { halide_target_feature_armv7s } |]
      FeatureNoNEON -> [CU.pure| int { halide_target_feature_no_neon } |]
      FeatureVSX -> [CU.pure| int { halide_target_feature_vsx } |]
      FeaturePOWER_ARCH_2_07 -> [CU.pure| int { halide_target_feature_power_arch_2_07 } |]
      FeatureCUDA -> [CU.pure| int { halide_target_feature_cuda } |]
      FeatureCUDACapability30 -> [CU.pure| int { halide_target_feature_cuda_capability30 } |]
      FeatureCUDACapability32 -> [CU.pure| int { halide_target_feature_cuda_capability32 } |]
      FeatureCUDACapability35 -> [CU.pure| int { halide_target_feature_cuda_capability35 } |]
      FeatureCUDACapability50 -> [CU.pure| int { halide_target_feature_cuda_capability50 } |]
      FeatureCUDACapability61 -> [CU.pure| int { halide_target_feature_cuda_capability61 } |]
      FeatureCUDACapability70 -> [CU.pure| int { halide_target_feature_cuda_capability70 } |]
      FeatureCUDACapability75 -> [CU.pure| int { halide_target_feature_cuda_capability75 } |]
      FeatureCUDACapability80 -> [CU.pure| int { halide_target_feature_cuda_capability80 } |]
      FeatureCUDACapability86 -> [CU.pure| int { halide_target_feature_cuda_capability86 } |]
      FeatureOpenCL -> [CU.pure| int { halide_target_feature_opencl } |]
      FeatureCLDoubles -> [CU.pure| int { halide_target_feature_cl_doubles } |]
      FeatureCLHalf -> [CU.pure| int { halide_target_feature_cl_half } |]
      FeatureCLAtomics64 -> [CU.pure| int { halide_target_feature_cl_atomic64 } |]
      FeatureOpenGLCompute -> [CU.pure| int { halide_target_feature_openglcompute } |]
      FeatureEGL -> [CU.pure| int { halide_target_feature_egl } |]
      FeatureUserContext -> [CU.pure| int { halide_target_feature_user_context } |]
      FeatureProfile -> [CU.pure| int { halide_target_feature_profile } |]
      FeatureNoRuntime -> [CU.pure| int { halide_target_feature_no_runtime } |]
      FeatureMetal -> [CU.pure| int { halide_target_feature_metal } |]
      FeatureCPlusPlusMangling -> [CU.pure| int { halide_target_feature_c_plus_plus_mangling } |]
      FeatureLargeBuffers -> [CU.pure| int { halide_target_feature_large_buffers } |]
      FeatureHexagonDma -> [CU.pure| int { halide_target_feature_hexagon_dma } |]
      FeatureHVX_128 -> [CU.pure| int { halide_target_feature_hvx_128 } |]
      FeatureHVX_v62 -> [CU.pure| int { halide_target_feature_hvx_v62 } |]
      FeatureHVX_v65 -> [CU.pure| int { halide_target_feature_hvx_v65 } |]
      FeatureHVX_v66 -> [CU.pure| int { halide_target_feature_hvx_v66 } |]
      FeatureHVX_shared_object -> [CU.pure| int { halide_target_feature_hvx_use_shared_object } |]
      FeatureFuzzFloatStores -> [CU.pure| int { halide_target_feature_fuzz_float_stores } |]
      FeatureSoftFloatABI -> [CU.pure| int { halide_target_feature_soft_float_abi } |]
      FeatureMSAN -> [CU.pure| int { halide_target_feature_msan } |]
      FeatureAVX512 -> [CU.pure| int { halide_target_feature_avx512 } |]
      FeatureAVX512_KNL -> [CU.pure| int { halide_target_feature_avx512_knl } |]
      FeatureAVX512_Skylake -> [CU.pure| int { halide_target_feature_avx512_skylake } |]
      FeatureAVX512_Cannonlake -> [CU.pure| int { halide_target_feature_avx512_cannonlake } |]
      FeatureAVX512_SapphireRapids -> [CU.pure| int { halide_target_feature_avx512_sapphirerapids } |]
      FeatureTraceLoads -> [CU.pure| int { halide_target_feature_trace_loads } |]
      FeatureTraceStores -> [CU.pure| int { halide_target_feature_trace_stores } |]
      FeatureTraceRealizations -> [CU.pure| int { halide_target_feature_trace_realizations } |]
      FeatureTracePipeline -> [CU.pure| int { halide_target_feature_trace_pipeline } |]
      FeatureD3D12Compute -> [CU.pure| int { halide_target_feature_d3d12compute } |]
      FeatureStrictFloat -> [CU.pure| int { halide_target_feature_strict_float } |]
      FeatureTSAN -> [CU.pure| int { halide_target_feature_tsan } |]
      FeatureASAN -> [CU.pure| int { halide_target_feature_asan } |]
      FeatureCheckUnsafePromises -> [CU.pure| int { halide_target_feature_check_unsafe_promises } |]
      FeatureEmbedBitcode -> [CU.pure| int { halide_target_feature_embed_bitcode } |]
      FeatureEnableLLVMLoopOpt -> [CU.pure| int { halide_target_feature_enable_llvm_loop_opt } |]
      FeatureWasmSimd128 -> [CU.pure| int { halide_target_feature_wasm_simd128 } |]
      FeatureWasmSignExt -> [CU.pure| int { halide_target_feature_wasm_signext } |]
      FeatureWasmSatFloatToInt -> [CU.pure| int { halide_target_feature_wasm_sat_float_to_int } |]
      FeatureWasmThreads -> [CU.pure| int { halide_target_feature_wasm_threads } |]
      FeatureWasmBulkMemory -> [CU.pure| int { halide_target_feature_wasm_bulk_memory } |]
      FeatureSVE -> [CU.pure| int { halide_target_feature_sve } |]
      FeatureSVE2 -> [CU.pure| int { halide_target_feature_sve2 } |]
      FeatureARMDotProd -> [CU.pure| int { halide_target_feature_arm_dot_prod } |]
      FeatureARMFp16 -> [CU.pure| int { halide_target_feature_arm_fp16 } |]
      FeatureRVV -> [CU.pure| int { halide_target_feature_rvv } |]
      FeatureARMv81a -> [CU.pure| int { halide_target_feature_armv81a } |]
      FeatureSanitizerCoverage -> [CU.pure| int { halide_target_feature_sanitizer_coverage } |]
      FeatureProfileByTimer -> [CU.pure| int { halide_target_feature_profile_by_timer } |]
      FeatureSPIRV -> [CU.pure| int { halide_target_feature_spirv } |]
      FeatureSemihosting -> [CU.pure| int { halide_target_feature_semihosting } |]
  toEnum k
    | fromIntegral k == [CU.pure| int { halide_target_feature_jit } |] = FeatureJIT
    | fromIntegral k == [CU.pure| int { halide_target_feature_debug } |] = FeatureDebug
    | fromIntegral k == [CU.pure| int { halide_target_feature_no_asserts } |] = FeatureNoAsserts
    | fromIntegral k == [CU.pure| int { halide_target_feature_no_bounds_query } |] = FeatureNoBoundsQuery
    | fromIntegral k == [CU.pure| int { halide_target_feature_sse41 } |] = FeatureSSE41
    | fromIntegral k == [CU.pure| int { halide_target_feature_avx } |] = FeatureAVX
    | fromIntegral k == [CU.pure| int { halide_target_feature_avx2 } |] = FeatureAVX2
    | fromIntegral k == [CU.pure| int { halide_target_feature_fma } |] = FeatureFMA
    | fromIntegral k == [CU.pure| int { halide_target_feature_fma4 } |] = FeatureFMA4
    | fromIntegral k == [CU.pure| int { halide_target_feature_f16c } |] = FeatureF16C
    | fromIntegral k == [CU.pure| int { halide_target_feature_armv7s } |] = FeatureARMv7s
    | fromIntegral k == [CU.pure| int { halide_target_feature_no_neon } |] = FeatureNoNEON
    | fromIntegral k == [CU.pure| int { halide_target_feature_vsx } |] = FeatureVSX
    | fromIntegral k == [CU.pure| int { halide_target_feature_power_arch_2_07 } |] = FeaturePOWER_ARCH_2_07
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda } |] = FeatureCUDA
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability30 } |] = FeatureCUDACapability30
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability32 } |] = FeatureCUDACapability32
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability35 } |] = FeatureCUDACapability35
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability50 } |] = FeatureCUDACapability50
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability61 } |] = FeatureCUDACapability61
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability70 } |] = FeatureCUDACapability70
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability75 } |] = FeatureCUDACapability75
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability80 } |] = FeatureCUDACapability80
    | fromIntegral k == [CU.pure| int { halide_target_feature_cuda_capability86 } |] = FeatureCUDACapability86
    | fromIntegral k == [CU.pure| int { halide_target_feature_opencl } |] = FeatureOpenCL
    | fromIntegral k == [CU.pure| int { halide_target_feature_cl_doubles } |] = FeatureCLDoubles
    | fromIntegral k == [CU.pure| int { halide_target_feature_cl_half } |] = FeatureCLHalf
    | fromIntegral k == [CU.pure| int { halide_target_feature_cl_atomic64 } |] = FeatureCLAtomics64
    | fromIntegral k == [CU.pure| int { halide_target_feature_openglcompute } |] = FeatureOpenGLCompute
    | fromIntegral k == [CU.pure| int { halide_target_feature_egl } |] = FeatureEGL
    | fromIntegral k == [CU.pure| int { halide_target_feature_user_context } |] = FeatureUserContext
    | fromIntegral k == [CU.pure| int { halide_target_feature_profile } |] = FeatureProfile
    | fromIntegral k == [CU.pure| int { halide_target_feature_no_runtime } |] = FeatureNoRuntime
    | fromIntegral k == [CU.pure| int { halide_target_feature_metal } |] = FeatureMetal
    | fromIntegral k == [CU.pure| int { halide_target_feature_c_plus_plus_mangling } |] = FeatureCPlusPlusMangling
    | fromIntegral k == [CU.pure| int { halide_target_feature_large_buffers } |] = FeatureLargeBuffers
    | fromIntegral k == [CU.pure| int { halide_target_feature_hexagon_dma } |] = FeatureHexagonDma
    | fromIntegral k == [CU.pure| int { halide_target_feature_hvx_128 } |] = FeatureHVX_128
    | fromIntegral k == [CU.pure| int { halide_target_feature_hvx_v62 } |] = FeatureHVX_v62
    | fromIntegral k == [CU.pure| int { halide_target_feature_hvx_v65 } |] = FeatureHVX_v65
    | fromIntegral k == [CU.pure| int { halide_target_feature_hvx_v66 } |] = FeatureHVX_v66
    | fromIntegral k == [CU.pure| int { halide_target_feature_hvx_use_shared_object } |] = FeatureHVX_shared_object
    | fromIntegral k == [CU.pure| int { halide_target_feature_fuzz_float_stores } |] = FeatureFuzzFloatStores
    | fromIntegral k == [CU.pure| int { halide_target_feature_soft_float_abi } |] = FeatureSoftFloatABI
    | fromIntegral k == [CU.pure| int { halide_target_feature_msan } |] = FeatureMSAN
    | fromIntegral k == [CU.pure| int { halide_target_feature_avx512 } |] = FeatureAVX512
    | fromIntegral k == [CU.pure| int { halide_target_feature_avx512_knl } |] = FeatureAVX512_KNL
    | fromIntegral k == [CU.pure| int { halide_target_feature_avx512_skylake } |] = FeatureAVX512_Skylake
    | fromIntegral k == [CU.pure| int { halide_target_feature_avx512_cannonlake } |] = FeatureAVX512_Cannonlake
    | fromIntegral k == [CU.pure| int { halide_target_feature_avx512_sapphirerapids } |] = FeatureAVX512_SapphireRapids
    | fromIntegral k == [CU.pure| int { halide_target_feature_trace_loads } |] = FeatureTraceLoads
    | fromIntegral k == [CU.pure| int { halide_target_feature_trace_stores } |] = FeatureTraceStores
    | fromIntegral k == [CU.pure| int { halide_target_feature_trace_realizations } |] = FeatureTraceRealizations
    | fromIntegral k == [CU.pure| int { halide_target_feature_trace_pipeline } |] = FeatureTracePipeline
    | fromIntegral k == [CU.pure| int { halide_target_feature_d3d12compute } |] = FeatureD3D12Compute
    | fromIntegral k == [CU.pure| int { halide_target_feature_strict_float } |] = FeatureStrictFloat
    | fromIntegral k == [CU.pure| int { halide_target_feature_tsan } |] = FeatureTSAN
    | fromIntegral k == [CU.pure| int { halide_target_feature_asan } |] = FeatureASAN
    | fromIntegral k == [CU.pure| int { halide_target_feature_check_unsafe_promises } |] = FeatureCheckUnsafePromises
    | fromIntegral k == [CU.pure| int { halide_target_feature_embed_bitcode } |] = FeatureEmbedBitcode
    | fromIntegral k == [CU.pure| int { halide_target_feature_enable_llvm_loop_opt } |] = FeatureEnableLLVMLoopOpt
    | fromIntegral k == [CU.pure| int { halide_target_feature_wasm_simd128 } |] = FeatureWasmSimd128
    | fromIntegral k == [CU.pure| int { halide_target_feature_wasm_signext } |] = FeatureWasmSignExt
    | fromIntegral k == [CU.pure| int { halide_target_feature_wasm_sat_float_to_int } |] = FeatureWasmSatFloatToInt
    | fromIntegral k == [CU.pure| int { halide_target_feature_wasm_threads } |] = FeatureWasmThreads
    | fromIntegral k == [CU.pure| int { halide_target_feature_wasm_bulk_memory } |] = FeatureWasmBulkMemory
    | fromIntegral k == [CU.pure| int { halide_target_feature_sve } |] = FeatureSVE
    | fromIntegral k == [CU.pure| int { halide_target_feature_sve2 } |] = FeatureSVE2
    | fromIntegral k == [CU.pure| int { halide_target_feature_arm_dot_prod } |] = FeatureARMDotProd
    | fromIntegral k == [CU.pure| int { halide_target_feature_arm_fp16 } |] = FeatureARMFp16
    | fromIntegral k == [CU.pure| int { halide_target_feature_rvv } |] = FeatureRVV
    | fromIntegral k == [CU.pure| int { halide_target_feature_armv81a } |] = FeatureARMv81a
    | fromIntegral k == [CU.pure| int { halide_target_feature_sanitizer_coverage } |] = FeatureSanitizerCoverage
    | fromIntegral k == [CU.pure| int { halide_target_feature_profile_by_timer } |] = FeatureProfileByTimer
    | fromIntegral k == [CU.pure| int { halide_target_feature_spirv } |] = FeatureSPIRV
    | fromIntegral k == [CU.pure| int { halide_target_feature_semihosting } |] = FeatureSemihosting
    | otherwise = error $ "unknown Target feature: " <> show k