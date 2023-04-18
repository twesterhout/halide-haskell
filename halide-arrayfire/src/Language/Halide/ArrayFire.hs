{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Halide.ArrayFire () where

import ArrayFire (AFType, Array)
import ArrayFire qualified as AF
import Data.Proxy
import GHC.TypeLits
import Language.Halide

forceNumDims :: AFType a => Array a -> Int -> [Int]
forceNumDims arr n
  | AF.getNumDims arr <= n = take n shape
  | otherwise =
      error $
        "cannot treat a "
          <> show (AF.getNumDims arr)
          <> "-dimensional array as a "
          <> show n
          <> "-dimensional buffer"
  where
    shape = let (d0, d1, d2, d3) = AF.getDims arr in [d0, d1, d2, d3]

instance (IsHalideType a, AFType a, KnownNat n, n <= 4) => IsHalideBuffer (Array a) n a where
  withHalideBufferImpl arr action = case AF.getBackend arr of
    AF.CPU -> AF.withDevicePtr arr $ \ptr -> bufferFromPtrShape ptr shape action
    AF.CUDA -> undefined
    AF.OpenCL -> undefined
    AF.Default -> error "do not know how to handle 'Default' backend"
    where
      shape = forceNumDims arr . fromIntegral $ natVal (Proxy @n)
