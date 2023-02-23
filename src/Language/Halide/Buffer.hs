{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
  , isDeviceDirty
  , isHostDirty
  , bufferCopyToHost

    -- * Constructing

  --

    -- | We define helper functions to easily construct 'HalideBuffer's from CPU pointers.
  , bufferFromPtrShapeStrides
  , bufferFromPtrShape
  , rowMajorStrides
  , colMajorStrides
  , allocaCpuBuffer
  , IsListPeek (..)
  )
where

import Control.Monad (forM, unless, when)
import Control.Monad.ST (RealWorld)
import Data.Bits
import Data.Foldable (foldl')
import Data.Int
import Data.Kind (Type)
import qualified Data.List as List
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack (HasCallStack)
import GHC.TypeNats
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
import Language.Halide.Type

-- | Information about a dimension in a buffer.
--
-- It is the Haskell analogue of [@halide_dimension_t@](https://halide-lang.org/docs/structhalide__dimension__t.html).
data HalideDimension = HalideDimension
  { halideDimensionMin :: {-# UNPACK #-} !Int32
  -- ^ Starting index.
  , halideDimensionExtent :: {-# UNPACK #-} !Int32
  -- ^ Length of the dimension.
  , halideDimensionStride :: {-# UNPACK #-} !Int32
  -- ^ Stride along this dimension.
  , halideDimensionFlags :: {-# UNPACK #-} !Word32
  -- ^ Extra flags.
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

colMajorStrides :: Integral a => [a] -> [a]
colMajorStrides = scanl (*) 1 . init

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

data CxxBuffer a

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
      hasDataOnDevice <-
        toEnum . fromIntegral
          <$> [CU.exp| bool { $(halide_buffer_t* bufferPtr)->device } |]
      when hasDataOnDevice $
        error "the Buffer still references data on the device; did you forget to call copyToHost?"
      pure r

-- | Similar to 'bufferFromPtrShapeStrides', but assumes column-major ordering of data.
bufferFromPtrShape
  :: forall n a b
   . (KnownNat n, IsHalideType a)
  => Ptr a
  -- ^ CPU pointer to the data
  -> [Int]
  -- ^ Extents (in number of elements, __not__ in bytes)
  -> (Ptr (HalideBuffer n a) -> IO b)
  -> IO b
bufferFromPtrShape p shape = bufferFromPtrShapeStrides p shape (colMajorStrides shape)

-- | Specifies that a type @t@ can be used as an @n@-dimensional Halide buffer
-- with elements of type @a@.
class (KnownNat n, IsHalideType a) => IsHalideBuffer t n a where
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

instance IsHalideType a => IsHalideBuffer [a] 1 a where
  withHalideBuffer v = withHalideBuffer (S.fromList v)

instance IsHalideType a => IsHalideBuffer [[a]] 2 a where
  withHalideBuffer xs f = do
    let d0 = length xs
        d1 = if d0 == 0 then 0 else length (head xs)
        -- we want column-major ordering, so transpose first
        v = S.fromList (List.concat (List.transpose xs))
    when (S.length v /= d0 * d1) $
      error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
    S.unsafeWith v $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [d0, d1] f

instance IsHalideType a => IsHalideBuffer [[[a]]] 3 a where
  withHalideBuffer xs f = do
    let d0 = length xs
        d1 = if d0 == 0 then 0 else length (head xs)
        d2 = if d1 == 0 then 0 else length (head (head xs))
        -- we want column-major ordering, so transpose first
        v =
          S.fromList
            . List.concat
            . List.concatMap List.transpose
            . List.transpose
            . fmap List.transpose
            $ xs
    when (S.length v /= d0 * d1 * d2) $
      error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
    S.unsafeWith v $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [d0, d1, d2] f

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond f =
  cond >>= \case
    True -> f
    False -> pure ()

allocaCpuBuffer
  :: forall n a b
   . (HasCallStack, KnownNat n, IsHalideType a)
  => [Int]
  -> (Ptr (HalideBuffer n a) -> IO b)
  -> IO b
allocaCpuBuffer shape action =
  allocaArray numElements $ \cpuPtr ->
    bufferFromPtrShape cpuPtr shape $ \buf -> do
      r <- action buf
      whenM (isDeviceDirty (castPtr buf)) $
        error $
          "device_dirty is set on a CPU-only buffer; "
            <> "did you forget a copyToHost in your pipeline?"
      pure r
  where
    numElements = foldl' (*) 1 shape

-- | Do we have changes on the device the have not been copied to the host?
isDeviceDirty :: Ptr RawHalideBuffer -> IO Bool
isDeviceDirty p =
  toBool <$> [CU.exp| bool { $(const halide_buffer_t* p)->device_dirty() } |]

-- | Do we have changes on the device the have not been copied to the host?
isHostDirty :: Ptr RawHalideBuffer -> IO Bool
isHostDirty p =
  toBool <$> [CU.exp| bool { $(const halide_buffer_t* p)->host_dirty() } |]

bufferCopyToHost :: Ptr RawHalideBuffer -> IO ()
bufferCopyToHost p =
  [C.throwBlock| void {
    auto& buf = *$(halide_buffer_t* p);
    if (buf.device_dirty()) {
      if (buf.device_interface == nullptr) {
        throw std::runtime_error{"bufferCopyToHost: device_dirty is set, "
                                 "but device_interface is NULL"};
      }
      if (buf.host == nullptr) {
        throw std::runtime_error{"bufferCopyToHost: host is NULL; "
                                 "did you forget to allocate memory?"};
      }
      buf.device_interface->copy_to_host(nullptr, &buf);
    }
  } |]

class IsListPeek a where
  type ListPeekElem a :: Type
  peekToList :: HasCallStack => Ptr a -> IO [ListPeekElem a]

instance IsHalideType a => IsListPeek (HalideBuffer 1 a) where
  type ListPeekElem (HalideBuffer 1 a) = a
  peekToList p = do
    whenM (isDeviceDirty (castPtr p)) $
      error "cannot peek data from device; call bufferCopyToHost first"
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    (HalideDimension min0 extent0 stride0 _) <- peekElemOff (halideBufferDim raw) 0
    let ptr0 = castPtr @_ @a (halideBufferHost raw)
    forM [0 .. extent0 - 1] $ \i0 ->
      peekElemOff ptr0 (fromIntegral (min0 + stride0 * i0))

instance IsHalideType a => IsListPeek (HalideBuffer 2 a) where
  type ListPeekElem (HalideBuffer 2 a) = [a]
  peekToList p = do
    whenM (isDeviceDirty (castPtr p)) $
      error "cannot peek data from device; call bufferCopyToHost first"
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    (HalideDimension min0 extent0 stride0 _) <- peekElemOff (halideBufferDim raw) 0
    (HalideDimension min1 extent1 stride1 _) <- peekElemOff (halideBufferDim raw) 1
    let ptr0 = castPtr @_ @a (halideBufferHost raw)
    forM [0 .. extent0 - 1] $ \i0 -> do
      let ptr1 = ptr0 `advancePtr` fromIntegral (min0 + stride0 * i0)
      forM [0 .. extent1 - 1] $ \i1 ->
        peekElemOff ptr1 (fromIntegral (min1 + stride1 * i1))

instance IsHalideType a => IsListPeek (HalideBuffer 3 a) where
  type ListPeekElem (HalideBuffer 3 a) = [[a]]
  peekToList p = do
    whenM (isDeviceDirty (castPtr p)) $
      error "cannot peek data from device; call bufferCopyToHost first"
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    (HalideDimension min0 extent0 stride0 _) <- peekElemOff (halideBufferDim raw) 0
    (HalideDimension min1 extent1 stride1 _) <- peekElemOff (halideBufferDim raw) 1
    (HalideDimension min2 extent2 stride2 _) <- peekElemOff (halideBufferDim raw) 2
    let ptr0 = castPtr @_ @a (halideBufferHost raw)
    forM [0 .. extent0 - 1] $ \i0 -> do
      let ptr1 = ptr0 `advancePtr` fromIntegral (min0 + stride0 * i0)
      forM [0 .. extent1 - 1] $ \i1 -> do
        let ptr2 = ptr1 `advancePtr` fromIntegral (min1 + stride1 * i1)
        forM [0 .. extent2 - 1] $ \i2 ->
          peekElemOff ptr2 (fromIntegral (min2 + stride2 * i2))
