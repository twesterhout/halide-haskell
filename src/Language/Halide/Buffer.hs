{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
-- Module      : Language.Halide.Buffer
-- Description : Buffers
-- Copyright   : (c) Tom Westerhout, 2021-2023
--
-- A buffer in Halide is a __view__ of some multidimensional array. Buffers can reference data that's
-- located on a CPU, GPU, or another device. In the C interface of Halide, buffers are described by
-- the C struct [@halide_buffer_t@](https://halide-lang.org/docs/structhalide__buffer__t.html). On the
-- Haskell side, we choose to define two types: 'RawHalideBuffer' and 'HalideBuffer'. 'RawHalideBuffer'
-- is the low-level untyped version, and 'HalideBuffer' builds on top of it and stores the element type
-- and the number of dimensions at the type level. Prefer 'HalideBuffer' whenever possible.
--
-- You can tell Halide how to work with your custom multidimensional arrays by defining an instance of
-- 'IsHalideBuffer'.
module Language.Halide.Buffer
  ( RawHalideBuffer (..)
  , HalideBuffer (..)
  , HalideDimension (..)
  , HalideDeviceInterface
  , IsHalideBuffer (..)

    -- * Constructing
    --
    -- | We define helper functions to easily construct 'HalideBuffer's from CPU pointers.
  , bufferFromPtrShapeStrides
  , bufferFromPtrShape
  )
where

import Control.Monad (unless, when)
import Control.Monad.ST (RealWorld)
import Data.Bits
import Data.Int
import Data.Kind (Type)
import Data.Proxy
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import Data.Word
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable
import GHC.Stack (HasCallStack)
import GHC.TypeNats
import Language.Halide.Type
import Language.Halide.Context

-- | Information about a dimension in a buffer.
--
-- It is the Haskell analogue of [@halide_dimension_t@](https://halide-lang.org/docs/structhalide__dimension__t.html).
--
data HalideDimension = HalideDimension
  { 
    -- | Starting index.
    halideDimensionMin :: {-# UNPACK #-} !Int32
    -- | Length of the dimension.
  , halideDimensionExtent :: {-# UNPACK #-} !Int32
    -- | Stride along this dimension.
  , halideDimensionStride :: {-# UNPACK #-} !Int32
    -- | Extra flags.
  , halideDimensionFlags :: {-# UNPACK #-} !Word32
  }
  deriving stock (Read, Show, Eq)

instance Storable HalideDimension where
  sizeOf _ = 16
  {-# INLINE sizeOf #-}
  alignment _ = 4
  {-# INLINE alignment #-}
  peek p =
    HalideDimension
      <$> peekByteOff p 0
      <*> peekByteOff p 4
      <*> peekByteOff p 8
      <*> peekByteOff p 12
  {-# INLINE peek #-}
  poke p x = do
    pokeByteOff p 0 (halideDimensionMin x)
    pokeByteOff p 4 (halideDimensionExtent x)
    pokeByteOff p 8 (halideDimensionStride x)
    pokeByteOff p 12 (halideDimensionFlags x)
  {-# INLINE poke #-}

toInt32 :: (HasCallStack, Bits a, Integral a) => a -> Int32
toInt32 x = case toIntegralSized x of
  Just y -> y
  Nothing -> error $ "integer overflow when converting " <> show (toInteger x) <> " to Int32"
{-# INLINE toInt32 #-}

-- | @simpleDimension extent stride@ creates a @HalideDimension@ of size @extent@ separated by
-- @stride@.
simpleDimension :: Int -> Int -> HalideDimension
simpleDimension extent stride = HalideDimension 0 (toInt32 extent) (toInt32 stride) 0
{-# INLINE simpleDimension #-}

rowMajorStrides :: Integral a => [a] -> [a]
rowMajorStrides = drop 1 . scanr (*) 1

-- | Haskell analogue of [@halide_device_interface_t@](https://halide-lang.org/docs/structhalide__device__interface__t.html).
data HalideDeviceInterface

-- | Haskell analogue of [@halide_buffer_t@](https://halide-lang.org/docs/structhalide__buffer__t.html).
data RawHalideBuffer = RawHalideBuffer
  { halideBufferDevice :: !Word64
  , halideBufferDeviceInterface :: !(Ptr HalideDeviceInterface)
  , halideBufferHost :: !(Ptr Word8)
  , halideBufferFlags :: !Word64
  , halideBufferType :: !HalideType
  , halideBufferDimensions :: !Int32
  , halideBufferDim :: !(Ptr HalideDimension)
  , halideBufferPadding :: !(Ptr ())
  }
  deriving stock (Show, Eq)

-- | An @n@-dimensional buffer of elements of type @a.
--
-- A wrapper around 'RawHalideBuffer' that ensures that Halide kernels receive
-- buffers of the right type and dimensionality.
newtype HalideBuffer (n :: Nat) (a :: Type) = HalideBuffer {unHalideBuffer :: RawHalideBuffer}
  deriving stock (Show, Eq)

importHalide

instance Storable RawHalideBuffer where
  sizeOf _ = 56
  alignment _ = 8
  peek p =
    RawHalideBuffer
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

-- | Construct a 'HalideBuffer' from a pointer to the data, a list of extents,
-- and a list of strides, and use it in an 'IO' action.
--
-- This function throws a runtime error if the number of dimensions does not
-- match @n@.
bufferFromPtrShapeStrides
  :: forall n a b
   . (HasCallStack, KnownNat n, IsHalideType a)
  => Ptr a
  -- ^ CPU pointer to the data
  -> [Int]
  -- ^ Extents (in number of elements, __not__ in bytes)
  -> [Int]
  -- ^ Strides (in number of elements, __not__ in bytes)
  -> (Ptr (HalideBuffer n a) -> IO b)
  -- ^ Action to run
  -> IO b
bufferFromPtrShapeStrides p shape stride action =
  withArrayLen (zipWith simpleDimension shape stride) $ \n dim -> do
    unless (n == fromIntegral (natVal (Proxy @n))) $
      error $
        "specified wrong number of dimensions: "
          <> show n
          <> "; expected "
          <> show (natVal (Proxy @n))
          <> " from the type declaration"
    let !buffer =
          RawHalideBuffer
            { halideBufferDevice = 0
            , halideBufferDeviceInterface = nullPtr
            , halideBufferHost = castPtr p
            , halideBufferFlags = 0
            , halideBufferType = halideTypeFor (Proxy :: Proxy a)
            , halideBufferDimensions = toInt32 n
            , halideBufferDim = dim
            , halideBufferPadding = nullPtr
            }
    with buffer $ \bufferPtr -> do
      r <- action (castPtr bufferPtr)
      hasDataOnDevice <- toEnum . fromIntegral
        <$> [CU.exp| bool { $(halide_buffer_t* bufferPtr)->device } |]
      when hasDataOnDevice $
        error "the Buffer still references data on the device; did you forget to call copyToHost?"
      pure r

-- | Similar to 'bufferFromPtrShapeStrides', but assumes row-major ordering of data.
bufferFromPtrShape
  :: forall n a b
   . (KnownNat n, IsHalideType a)
  => Ptr a
  -- ^ CPU pointer to the data
  -> [Int]
  -- ^ Extents (in number of elements, __not__ in bytes)
  -> (Ptr (HalideBuffer n a) -> IO b)
  -> IO b
bufferFromPtrShape p shape = bufferFromPtrShapeStrides p shape (rowMajorStrides shape)

-- | Specifies that a type @t@ can be used as an @n@-dimensional Halide buffer
-- with elements of type @a@.
class (KnownNat n, IsHalideType a) => IsHalideBuffer t n a | t -> n, t -> a where
  withHalideBuffer :: t -> (Ptr (HalideBuffer n a) -> IO b) -> IO b

-- | Storable vectors are one-dimensional buffers.
instance IsHalideType a => IsHalideBuffer (S.Vector a) 1 a where
  withHalideBuffer v f =
    S.unsafeWith v $ \dataPtr ->
      bufferFromPtrShape dataPtr [S.length v] f

-- | Storable vectors are one-dimensional buffers.
instance IsHalideType a => IsHalideBuffer (S.MVector RealWorld a) 1 a where
  withHalideBuffer v f =
    SM.unsafeWith v $ \dataPtr ->
      bufferFromPtrShape dataPtr [SM.length v] f
