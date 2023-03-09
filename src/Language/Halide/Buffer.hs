{-# LANGUAGE AllowAmbiguousTypes #-}
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
-- located on a CPU, GPU, or another device. Halide pipelines use buffers for both input and output arguments.
module Language.Halide.Buffer
  ( -- * Buffers

  --

    -- | In the C interface of Halide, buffers are described by the C struct
    -- [@halide_buffer_t@](https://halide-lang.org/docs/structhalide__buffer__t.html). On the Haskell side,
    -- we have 'HalideBuffer'.
    HalideBuffer (..)
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

    -- * Internals
  , RawHalideBuffer (..)
  , HalideDimension (..)
  , HalideDeviceInterface
  , rowMajorStrides
  , colMajorStrides
  , isDeviceDirty
  , isHostDirty
  , bufferCopyToHost
  )
where

import Control.Exception (bracket_)
import Control.Monad (forM, unless, when)
import Control.Monad.ST (RealWorld)
import Data.Int
import Data.Kind (Type)
import Data.List qualified as List
import Data.Proxy
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Data.Word
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack (HasCallStack)
import GHC.TypeNats
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Halide.Context
import Language.Halide.Target
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

-- | @simpleDimension extent stride@ creates a @HalideDimension@ of size @extent@ separated by
-- @stride@.
simpleDimension :: Int -> Int -> HalideDimension
simpleDimension extent stride = HalideDimension 0 (fromIntegral extent) (fromIntegral stride) 0
{-# INLINE simpleDimension #-}

-- | Get strides corresponding to row-major ordering
rowMajorStrides
  :: Integral a
  => [a]
  -- ^ Extents
  -> [a]
rowMajorStrides = drop 1 . scanr (*) 1

-- | Get strides corresponding to column-major ordering.
colMajorStrides
  :: Integral a
  => [a]
  -- ^ Extents
  -> [a]
colMajorStrides = scanl (*) 1 . init

-- | Haskell analogue of [@halide_device_interface_t@](https://halide-lang.org/docs/structhalide__device__interface__t.html).
data HalideDeviceInterface

-- | The low-level untyped Haskell analogue of [@halide_buffer_t@](https://halide-lang.org/docs/structhalide__buffer__t.html).
--
-- It's quite difficult to use 'RawHalideBuffer' correctly, and misusage can result in crashes and
-- segmentation faults. Hence, prefer the higher-level 'HalideBuffer' wrapper for all your code
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

-- | An @n@-dimensional buffer of elements of type @a@.
--
-- Most pipelines use @'Ptr' ('HalideBuffer' n a)@ for input and output array arguments.
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
            , halideBufferDimensions = fromIntegral n
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
  :: (HasCallStack, KnownNat n, IsHalideType a)
  => Ptr a
  -- ^ CPU pointer to the data
  -> [Int]
  -- ^ Extents (in number of elements, __not__ in bytes)
  -> (Ptr (HalideBuffer n a) -> IO b)
  -> IO b
bufferFromPtrShape p shape = bufferFromPtrShapeStrides p shape (colMajorStrides shape)

-- | Specifies that a type @t@ can be used as an @n@-dimensional Halide buffer with elements of type @a@.
class (KnownNat n, IsHalideType a) => IsHalideBuffer t n a where
  withHalideBufferImpl :: t -> (Ptr (HalideBuffer n a) -> IO b) -> IO b

-- | Treat a type @t@ as a 'HalideBuffer' and use it in an 'IO' action.
--
-- This function is a simple wrapper around 'withHalideBufferImpl', except that the order of type parameters
-- is reversed. If you have @TypeApplications@ extension enabled, this allows you to write
-- @withHalideBuffer @3 @Float yourBuffer@ to specify that you want a 3-dimensional buffer of @Float@.
withHalideBuffer :: forall n a t b. IsHalideBuffer t n a => t -> (Ptr (HalideBuffer n a) -> IO b) -> IO b
withHalideBuffer = withHalideBufferImpl @t @n @a

-- | Storable vectors are one-dimensional buffers. This involves no copying.
instance IsHalideType a => IsHalideBuffer (S.Vector a) 1 a where
  withHalideBufferImpl v f =
    S.unsafeWith v $ \dataPtr ->
      bufferFromPtrShape dataPtr [S.length v] f

-- | Storable vectors are one-dimensional buffers. This involves no copying.
instance IsHalideType a => IsHalideBuffer (S.MVector RealWorld a) 1 a where
  withHalideBufferImpl v f =
    SM.unsafeWith v $ \dataPtr ->
      bufferFromPtrShape dataPtr [SM.length v] f

-- | Lists can also act as Halide buffers. __Use for testing only.__
instance IsHalideType a => IsHalideBuffer [a] 1 a where
  withHalideBufferImpl v = withHalideBuffer (S.fromList v)

-- | Lists can also act as Halide buffers. __Use for testing only.__
instance IsHalideType a => IsHalideBuffer [[a]] 2 a where
  withHalideBufferImpl xs f = do
    let d0 = length xs
        d1 = if d0 == 0 then 0 else length (head xs)
        -- we want column-major ordering, so transpose first
        v = S.fromList (List.concat (List.transpose xs))
    when (S.length v /= d0 * d1) $
      error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
    S.unsafeWith v $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [d0, d1] f

-- | Lists can also act as Halide buffers. __Use for testing only.__
instance IsHalideType a => IsHalideBuffer [[[a]]] 3 a where
  withHalideBufferImpl xs f = do
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

-- | Temporary allocate a CPU buffer.
--
-- This is useful for testing and debugging when you need to allocate an output buffer for your pipeline. E.g.
--
-- @
-- 'allocaCpuBuffer' [3, 3] $ \out -> do
--   myKernel out                -- fill the buffer
--   print =<< 'peekToList' out  -- print it for debugging
-- @
allocaCpuBuffer
  :: forall n a b
   . (HasCallStack, KnownNat n, IsHalideType a)
  => [Int]
  -> (Ptr (HalideBuffer n a) -> IO b)
  -> IO b
allocaCpuBuffer = allocaBuffer hostTarget

getTotalBytes :: Ptr RawHalideBuffer -> IO Int
getTotalBytes buf = do
  fromIntegral
    <$> [CU.block| size_t {
          auto const& b = *$(const halide_buffer_t* buf);
          auto const n = std::accumulate(b.dim, b.dim + b.dimensions, size_t{1},
                                         [](auto acc, auto const& dim) { return acc * dim.extent; });
          fprintf(stderr, "getTotalBytes: %zu\n", n * (b.type.bits * b.type.lanes / 8));
          return n * (b.type.bits * b.type.lanes / 8);
        } |]

allocateHostMemory :: Ptr RawHalideBuffer -> IO ()
allocateHostMemory buf = do
  ptr <- mallocBytes =<< getTotalBytes buf
  [CU.block| void { $(halide_buffer_t* buf)->host = $(uint8_t* ptr); } |]

freeHostMemory :: Ptr RawHalideBuffer -> IO ()
freeHostMemory buf = do
  ptr <-
    [CU.block| uint8_t* {
      auto& b = *$(halide_buffer_t* buf);
      auto const p = b.host;
      b.host = nullptr;
      return p;
    } |]
  free ptr

allocateDeviceMemory :: Ptr HalideDeviceInterface -> Ptr RawHalideBuffer -> IO ()
allocateDeviceMemory interface buf = do
  [CU.block| void {
    auto const* interface = $(const halide_device_interface_t* interface);
    interface->device_malloc(nullptr, $(halide_buffer_t* buf), interface);
  } |]

freeDeviceMemory :: HasCallStack => Ptr RawHalideBuffer -> IO ()
freeDeviceMemory buf = do
  deviceInterface <-
    [CU.exp| const halide_device_interface_t* { $(const halide_buffer_t* buf)->device_interface } |]
  when (deviceInterface == nullPtr) $
    error "cannot free device memory: device_interface is NULL"
  [CU.block| void {
    $(halide_buffer_t* buf)->device_interface->device_free(nullptr, $(halide_buffer_t* buf));
    $(halide_buffer_t* buf)->device = 0;
  } |]

allocaBuffer
  :: forall n a b
   . (HasCallStack, KnownNat n, IsHalideType a)
  => Target
  -> [Int]
  -> (Ptr (HalideBuffer n a) -> IO b)
  -> IO b
allocaBuffer target shape action = do
  deviceInterface <- getDeviceInterface target
  let onHost = deviceInterface == nullPtr
  withArrayLen (zipWith simpleDimension shape (colMajorStrides shape)) $ \n dim -> do
    unless (n == fromIntegral (natVal (Proxy @n))) $
      error $
        "specified wrong number of dimensions: "
          <> show n
          <> "; expected "
          <> show (natVal (Proxy @n))
          <> " from the type declaration"
    let rawBuffer =
          RawHalideBuffer
            { halideBufferDevice = 0
            , halideBufferDeviceInterface = nullPtr
            , halideBufferHost = nullPtr
            , halideBufferFlags = 0
            , halideBufferType = halideTypeFor (Proxy :: Proxy a)
            , halideBufferDimensions = fromIntegral n
            , halideBufferDim = dim
            , halideBufferPadding = nullPtr
            }
    with rawBuffer $ \buf -> do
      let allocate
            | onHost = allocateHostMemory
            | otherwise = allocateDeviceMemory deviceInterface
      let deallocate
            | onHost = freeHostMemory
            | otherwise = freeDeviceMemory
      bracket_ (allocate buf) (deallocate buf) $ do
        r <- action (castPtr buf)
        isHostNull <- toBool <$> [CU.exp| bool { $(halide_buffer_t* buf)->host == nullptr } |]
        isDeviceNull <- toBool <$> [CU.exp| bool { $(halide_buffer_t* buf)->device == 0 } |]
        when (onHost && not isDeviceNull) . error $
          "buffer was allocated on host, but its device pointer is not NULL"
            <> "; did you forget a copyToHost in your pipeline?"
        when (not onHost && not isHostNull) . error $
          "buffer was allocated on device, but its host pointer is not NULL"
            <> "; did you add an extra copyToHost?"
        pure r

getDeviceInterface :: Target -> IO (Ptr HalideDeviceInterface)
getDeviceInterface target =
  case device of
    DeviceNone -> pure nullPtr
    DeviceHost -> pure nullPtr
    _ ->
      withCxxTarget target $ \target' ->
        [C.throwBlock| const halide_device_interface_t* {
          return handle_halide_exceptions([=](){
            auto const device = static_cast<Halide::DeviceAPI>($(int api));
            auto const& target = *$(const Halide::Target* target');
            return Halide::get_device_interface_for_device_api(device, target, "getDeviceInterface");
          });
        } |]
  where
    device@(fromIntegral . fromEnum -> api) = deviceAPIForTarget target

-- | Do we have changes on the device the have not been copied to the host?
isDeviceDirty :: Ptr RawHalideBuffer -> IO Bool
isDeviceDirty p =
  toBool <$> [CU.exp| bool { $(const halide_buffer_t* p)->device_dirty() } |]

-- | Do we have changes on the device the have not been copied to the host?
isHostDirty :: Ptr RawHalideBuffer -> IO Bool
isHostDirty p =
  toBool <$> [CU.exp| bool { $(const halide_buffer_t* p)->host_dirty() } |]

-- | Copy the underlying memory from device to host.
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

checkNumberOfDimensions :: forall n. (HasCallStack, KnownNat n) => RawHalideBuffer -> IO ()
checkNumberOfDimensions raw = do
  unless (fromIntegral (natVal (Proxy @n)) == raw.halideBufferDimensions) $
    error $
      "type-level and runtime number of dimensions do not match: "
        <> show (natVal (Proxy @n))
        <> " != "
        <> show raw.halideBufferDimensions

-- | Specifies that @a@ can be converted to a list. This is very similar to 'GHC.Exts.IsList' except that
-- we read the list from a @'Ptr'@ rather than converting directly.
class IsListPeek a where
  type ListPeekElem a :: Type
  peekToList :: HasCallStack => Ptr a -> IO [ListPeekElem a]

instance IsHalideType a => IsListPeek (HalideBuffer 0 a) where
  type ListPeekElem (HalideBuffer 0 a) = a
  peekToList p = do
    whenM (isDeviceDirty (castPtr p)) $
      error "cannot peek data from device; call bufferCopyToHost first"
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    checkNumberOfDimensions @0 raw
    fmap pure . peek $ castPtr @_ @a (halideBufferHost raw)

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
