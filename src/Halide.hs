-- |
-- Copyright: (c) 2021 Tom Westerhout
-- SPDX-License-Identifier: BSD-3-Clause
-- Maintainer: Tom Westerhout <14264576+twesterhout@users.noreply.github.com>
--
-- See README for more info
module Halide
  ( HalideBuffer (..),
    bufferFromPtrShape,
    bufferFromPtrShapeStrides,
    HalideDimension (..),
    IsHalideType (..),
    HalideType (..),
    IsHalideBuffer (..),
  )
where

import Data.Bits
import Data.Int
import Data.Proxy
import Data.Word
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable

data HalideDimension = HalideDimension
  { halideDimensionMin :: {-# UNPACK #-} !Int32,
    halideDimensionExtent :: {-# UNPACK #-} !Int32,
    halideDimensionStride :: {-# UNPACK #-} !Int32,
    halideDimensionFlags :: {-# UNPACK #-} !Word32
  }
  deriving stock (Read, Show, Eq)

instance Storable HalideDimension where
  sizeOf _ = 16
  alignment _ = 4
  peek p =
    HalideDimension
      <$> peekByteOff p 0
      <*> peekByteOff p 4
      <*> peekByteOff p 8
      <*> peekByteOff p 12
  poke p x = do
    pokeByteOff p 0 (halideDimensionMin x)
    pokeByteOff p 4 (halideDimensionExtent x)
    pokeByteOff p 8 (halideDimensionStride x)
    pokeByteOff p 12 (halideDimensionFlags x)

toInt32 :: (Bits a, Integral a) => a -> Int32
toInt32 x = case toIntegralSized x of
  Just y -> y
  Nothing -> error $ "integer overflow when converting " <> show (toInteger x) <> " to Int32"

simpleDimension :: Int -> Int -> HalideDimension
simpleDimension extent stride = HalideDimension 0 (toInt32 extent) (toInt32 stride) 0

rowMajorStrides :: Integral a => [a] -> [a]
rowMajorStrides = drop 1 . scanr (*) 1

data HalideDeviceInterface

data HalideTypeCode
  = HalideTypeInt
  | HalideTypeUInt
  | HalideTypeFloat
  | HalideTypeHandle
  | HalideTypeBfloat
  deriving stock (Read, Show, Eq)

instance Enum HalideTypeCode where
  fromEnum x = case x of
    HalideTypeInt -> 0
    HalideTypeUInt -> 1
    HalideTypeFloat -> 2
    HalideTypeHandle -> 3
    HalideTypeBfloat -> 4
  toEnum x = case x of
    0 -> HalideTypeInt
    1 -> HalideTypeUInt
    2 -> HalideTypeFloat
    3 -> HalideTypeHandle
    4 -> HalideTypeBfloat
    _ -> error $ "invalid HalideTypeCode: " <> show x

data HalideType = HalideType
  { halideTypeCode :: !HalideTypeCode,
    halideTypeBits :: {-# UNPACK #-} !Word8,
    halideTypeLanes :: {-# UNPACK #-} !Word16
  }
  deriving stock (Read, Show, Eq)

instance Storable HalideType where
  sizeOf _ = 4
  alignment _ = 4
  peek p =
    HalideType
      <$> (toEnum . (fromIntegral :: Word8 -> Int) <$> peekByteOff p 0)
      <*> peekByteOff p 1
      <*> peekByteOff p 2
  poke p (HalideType code bits lanes) = do
    pokeByteOff p 0 . (fromIntegral :: Int -> Word8) . fromEnum $ code
    pokeByteOff p 1 bits
    pokeByteOff p 2 lanes

class IsHalideType a where
  halideTypeFor :: proxy a -> HalideType

instance IsHalideType Float where halideTypeFor _ = HalideType HalideTypeFloat 32 1

instance IsHalideType Double where halideTypeFor _ = HalideType HalideTypeFloat 64 1

instance IsHalideType Bool where halideTypeFor _ = HalideType HalideTypeUInt 1 1

instance IsHalideType Word8 where halideTypeFor _ = HalideType HalideTypeUInt 8 1

instance IsHalideType Word16 where halideTypeFor _ = HalideType HalideTypeUInt 16 1

instance IsHalideType Word32 where halideTypeFor _ = HalideType HalideTypeUInt 32 1

instance IsHalideType Word64 where halideTypeFor _ = HalideType HalideTypeUInt 64 1

instance IsHalideType Int8 where halideTypeFor _ = HalideType HalideTypeInt 8 1

instance IsHalideType Int16 where halideTypeFor _ = HalideType HalideTypeInt 16 1

instance IsHalideType Int32 where halideTypeFor _ = HalideType HalideTypeInt 32 1

instance IsHalideType Int64 where halideTypeFor _ = HalideType HalideTypeInt 64 1

data HalideBuffer = HalideBuffer
  { halideBufferDevice :: !Word64,
    halideBufferDeviceInterface :: !(Ptr HalideDeviceInterface),
    halideBufferHost :: !(Ptr Word8),
    halideBufferFlags :: !Word64,
    halideBufferType :: !HalideType,
    halideBufferDimensions :: !Int32,
    halideBufferDim :: !(Ptr HalideDimension),
    halideBufferPadding :: !(Ptr ())
  }
  deriving stock (Show, Eq)

instance Storable HalideBuffer where
  sizeOf _ = 56
  alignment _ = 8
  peek p =
    HalideBuffer
      <$> peekByteOff p 0 -- device
      <*> peekByteOff p 8 -- interface
      <*> peekByteOff p 16 -- host
      <*> peekByteOff p 24 -- flags
      <*> peekByteOff p 32 -- type
      <*> peekByteOff p 36 -- dimensions
      <*> peekByteOff p 40 -- dim
      <*> peekByteOff p 48 -- padding
  poke p x = do
    pokeByteOff p 0 (halideBufferDevice x)
    pokeByteOff p 8 (halideBufferDeviceInterface x)
    pokeByteOff p 16 (halideBufferHost x)
    pokeByteOff p 24 (halideBufferFlags x)
    pokeByteOff p 32 (halideBufferType x)
    pokeByteOff p 36 (halideBufferDimensions x)
    pokeByteOff p 40 (halideBufferDim x)
    pokeByteOff p 48 (halideBufferPadding x)

bufferFromPtrShapeStrides :: forall a b. IsHalideType a => Ptr a -> [Int] -> [Int] -> (Ptr HalideBuffer -> IO b) -> IO b
bufferFromPtrShapeStrides p shape stride action =
  withArrayLen (zipWith simpleDimension shape stride) $ \n dim -> do
    let buffer =
          HalideBuffer
            { halideBufferDevice = 0,
              halideBufferDeviceInterface = nullPtr,
              halideBufferHost = castPtr p,
              halideBufferFlags = 0,
              halideBufferType = halideTypeFor (Proxy :: Proxy a),
              halideBufferDimensions = toInt32 n,
              halideBufferDim = dim,
              halideBufferPadding = nullPtr
            }
    with buffer action

bufferFromPtrShape :: forall a b. IsHalideType a => Ptr a -> [Int] -> (Ptr HalideBuffer -> IO b) -> IO b
bufferFromPtrShape p shape = bufferFromPtrShapeStrides p shape (rowMajorStrides shape)

class IsHalideBuffer a where
  withHalideBuffer :: a -> (Ptr HalideBuffer -> IO b) -> IO b
