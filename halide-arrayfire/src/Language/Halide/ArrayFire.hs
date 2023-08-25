{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Halide.ArrayFire
  ( ShapedArray (..)
  , mkShapedArray
  , produceShapedArray
  ) where

import ArrayFire (AFType, Array)
import ArrayFire qualified as AF
import Data.Foldable (Foldable (foldl'))
import Data.Proxy
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Language.Halide
import Prelude hiding ((<=), (==))

forceNumDims :: (AFType a) => Array a -> Int -> [Int]
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

-- | ArrayFire's 'Array' supports up to 4-dimensional arrays. Quite often this is not enough.
-- 'ShapedArray' lets you store your data in an 'Array' and keeps track of the shape separately.
-- That way you can pass 'ShapedArray' to Halide as an arbitrary-dimensional buffer.
data ShapedArray a = ShapedArray ![Int] !(Array a)
  deriving stock (Show, Eq)

-- | Smart constructor for 'ShapedArray' that verifies that the number of elements in the array
-- match the shape. Throws an error if this is not the case.
mkShapedArray :: (HasCallStack, AFType a) => [Int] -> Array a -> ShapedArray a
mkShapedArray shape arr
  | foldl' (*) 1 shape == AF.getElements arr = ShapedArray shape arr
  | otherwise =
      error $
        "shape mismatch: an array with "
          <> show (AF.getElements arr)
          <> " cannot be reshaped to "
          <> show shape

produceShapedArray
  :: forall a n
   . (HasCallStack, IsHalideType a, AFType a, Num a, KnownNat n)
  => [Int]
  -> (Ptr (HalideBuffer n a) -> IO ())
  -> IO (ShapedArray a)
produceShapedArray shape func = do
  let numberElements = foldl' (*) 1 shape
      out = ShapedArray shape (AF.constant @a [numberElements] 0)
  () <- withHalideBuffer @n @a out func
  pure out

instance (IsHalideType a, AFType a, KnownNat n) => IsHalideBuffer (ShapedArray a) n a where
  withHalideBufferImpl (ShapedArray shape arr) action
    | length shape == fromIntegral (natVal (Proxy @n)) = case AF.getBackend arr of
        AF.CPU -> AF.withDevicePtr arr $ \ptr -> bufferFromPtrShape ptr shape action
        AF.CUDA -> undefined
        AF.OpenCL -> undefined
        AF.Default -> error "do not know how to handle 'Default' backend"
    | otherwise =
        error $
          "withHalideBufferImpl: cannot treat a "
            <> show (length shape)
            <> "-dimensional array as a "
            <> show (natVal (Proxy @n))
            <> "-dimensional buffer"
