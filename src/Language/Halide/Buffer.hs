{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
  , peekScalar
    -- | For production usage however, you don't want to work with lists. Instead, you probably want Halide
    -- to work with your existing array data types. For this, we define 'IsHalideBuffer' typeclass that
    -- teaches Halide how to convert your data into a 'HalideBuffer'. Depending on how you implement the
    -- instance, this can be very efficient, because it need not involve any memory copying.
  , IsHalideBuffer (..)
  , withHalideBuffer
    -- | There are also helper functions to simplify writing instances of 'IsHalideBuffer'.
  , bufferFromPtrShapeStrides
  , bufferFromPtrShape

    -- * Managed buffers
  , ManagedHalideBuffer
  , managedFromCpuPtrShapeStrides
  , managedHalideBufferToHalideBuffer
  , freeManagedHalideBuffer

    -- * Internals
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
import Foreign.Marshal.Alloc (alloca, callocBytes, free, mallocBytes)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import GHC.Stable (StablePtr, freeStablePtr, newStablePtr)
import GHC.Stack (HasCallStack)
import GHC.TypeNats
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Halide.Context
import Language.Halide.FunPtr
import Language.Halide.Target
import Language.Halide.Type
import Prelude hiding (min)

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
  :: (Integral a)
  => [a]
  -- ^ Extents
  -> [a]
rowMajorStrides = drop 1 . scanr (*) 1

-- | Get strides corresponding to column-major ordering.
colMajorStrides
  :: (Integral a)
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

newtype ManagedHalideBuffer (n :: Nat) (a :: Type) = ManagedHalideBuffer {unManagedHalideBuffer :: RawHalideBuffer}
  deriving stock (Show, Eq)

importHalide

instance
  (CoercibleCallable f g)
  => CoercibleCallable (Ptr RawHalideBuffer -> f) (Ptr RawHalideBuffer -> g)

instance
  (CoercibleCallable f g, KnownNat n, IsHalideType a)
  => CoercibleCallable (Ptr (HalideBuffer n a) -> f) (Ptr (HalideBuffer n a) -> g)

instance
  (CoercibleCallable f g, KnownNat n, IsHalideType a)
  => CoercibleCallable (Ptr RawHalideBuffer -> f) (Ptr (HalideBuffer n a) -> g)

instance
  (CoercibleCallable f g, KnownNat n, IsHalideType a)
  => CoercibleCallable (Ptr (HalideBuffer n a) -> f) (Ptr RawHalideBuffer -> g)

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

managedFromCpuPtrShapeStrides
  :: forall n a resource
   . (HasCallStack, KnownNat n, IsHalideType a)
  => resource
  -- ^ Resource to keep alive, i.e., the owner of the data
  -> Ptr a
  -- ^ CPU pointer to the data
  -> [Int]
  -- ^ Extents (in number of elements, __not__ in bytes)
  -> [Int]
  -- ^ Strides (in number of elements, __not__ in bytes)
  -> IO (Ptr (ManagedHalideBuffer n a))
managedFromCpuPtrShapeStrides resource cpuPtr shape strides = do
  let dim = zipWith simpleDimension shape strides
      rank = length dim
  -- Check the number of dimensions
  unless (rank == fromIntegral (natVal (Proxy @n)))
    . error
    $ "specified wrong number of dimensions: "
    <> show rank
    <> "; expected "
    <> show (natVal (Proxy @n))
    <> " from the type declaration"
  -- Allocate storage for the ManagedHalideBuffer
  let bufferSize = sizeOf (undefined :: RawHalideBuffer)
      ptrSize = sizeOf (undefined :: Ptr ())
      dimSize = sizeOf (undefined :: HalideDimension)
      paddingSize = bufferSize `mod` ptrSize
      -- We need space for
      -- 1) RawHalideBuffer
      -- 2) StablePtr to the resource
      -- 3) array of HalideDimension of size rank
      memorySize = (bufferSize + paddingSize) + ptrSize + rank * dimSize
  memory <- callocBytes memorySize
  let bufferPtr :: Ptr RawHalideBuffer
      bufferPtr = castPtr memory
      resourcePtr :: Ptr (StablePtr resource)
      resourcePtr = bufferPtr `plusPtr` (bufferSize + paddingSize)
      dimPtr :: Ptr HalideDimension
      dimPtr = resourcePtr `plusPtr` ptrSize
  -- Store the dimensions
  pokeArray dimPtr dim
  -- Store the buffer
  poke bufferPtr
    $ RawHalideBuffer
      { halideBufferDevice = 0
      , halideBufferDeviceInterface = nullPtr
      , halideBufferHost = castPtr cpuPtr
      , halideBufferFlags = 0
      , halideBufferType = halideTypeFor (Proxy :: Proxy a)
      , halideBufferDimensions = fromIntegral rank
      , halideBufferDim = dimPtr
      , halideBufferPadding = nullPtr
      }
  -- Store the resource
  poke resourcePtr =<< newStablePtr resource
  pure memory

managedHalideBufferToHalideBuffer :: Ptr (ManagedHalideBuffer n a) -> Ptr (HalideBuffer n a)
managedHalideBufferToHalideBuffer = castPtr

freeManagedHalideBuffer :: Ptr (ManagedHalideBuffer n a) -> IO ()
freeManagedHalideBuffer memory = do
  let bufferSize = sizeOf (undefined :: RawHalideBuffer)
      paddingSize = bufferSize `mod` sizeOf (undefined :: Ptr ())
      resourcePtr = memory `plusPtr` (bufferSize + paddingSize)
  freeStablePtr =<< peek resourcePtr
  free memory

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
    unless (n == fromIntegral (natVal (Proxy @n)))
      $ error
      $ "specified wrong number of dimensions: "
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
        toEnum
          . fromIntegral
          <$> [CU.exp| bool { $(halide_buffer_t* bufferPtr)->device } |]
      when hasDataOnDevice
        $ error "the Buffer still references data on the device; did you forget to call copyToHost?"
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
withHalideBuffer :: forall n a t b. (IsHalideBuffer t n a) => t -> (Ptr (HalideBuffer n a) -> IO b) -> IO b
withHalideBuffer = withHalideBufferImpl @t @n @a

-- | Storable vectors are one-dimensional buffers. This involves no copying.
instance (IsHalideType a) => IsHalideBuffer (S.Vector a) 1 a where
  withHalideBufferImpl v f =
    S.unsafeWith v $ \dataPtr ->
      bufferFromPtrShape dataPtr [S.length v] f

-- | Storable vectors are one-dimensional buffers. This involves no copying.
instance (IsHalideType a) => IsHalideBuffer (S.MVector RealWorld a) 1 a where
  withHalideBufferImpl v f =
    SM.unsafeWith v $ \dataPtr ->
      bufferFromPtrShape dataPtr [SM.length v] f

-- | Lists can also act as Halide buffers. __Use for testing only.__
instance (IsHalideType a) => IsHalideBuffer [a] 1 a where
  withHalideBufferImpl v = withHalideBuffer (S.fromList v)

-- | Lists can also act as Halide buffers. __Use for testing only.__
instance (IsHalideType a) => IsHalideBuffer [[a]] 2 a where
  withHalideBufferImpl xs f = do
    let d0 = length xs
        d1 = if d0 == 0 then 0 else length (head xs)
        -- we want column-major ordering, so transpose first
        v = S.fromList (List.concat (List.transpose xs))
    when (S.length v /= d0 * d1)
      $ error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
    S.unsafeWith v $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [d0, d1] f

-- | Lists can also act as Halide buffers. __Use for testing only.__
instance (IsHalideType a) => IsHalideBuffer [[[a]]] 3 a where
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
    when (S.length v /= d0 * d1 * d2)
      $ error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
    S.unsafeWith v $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [d0, d1, d2] f

-- | Lists can also act as Halide buffers. __Use for testing only.__
instance (IsHalideType a) => IsHalideBuffer [[[[a]]]] 4 a where
  withHalideBufferImpl xs f = do
    let d0 = length xs
        d1 = if d0 == 0 then 0 else length (head xs)
        d2 = if d1 == 0 then 0 else length (head (head xs))
        d3 = if d2 == 0 then 0 else length (head (head (head xs)))
        -- we want column-major ordering, so transpose first
        v =
          S.fromList
            . concat
            . concat
            . concatMap (fmap List.transpose . List.transpose . fmap List.transpose)
            . List.transpose
            . fmap (List.transpose . fmap List.transpose)
            $ xs
    when (S.length v /= d0 * d1 * d2 * d3)
      $ error "list doesn't have a regular shape (i.e. rows have varying number of elements)"
    S.unsafeWith v $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [d0, d1, d2, d3] f

whenM :: (Monad m) => m Bool -> m () -> m ()
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

freeDeviceMemory :: (HasCallStack) => Ptr RawHalideBuffer -> IO ()
freeDeviceMemory buf = do
  deviceInterface <-
    [CU.exp| const halide_device_interface_t* { $(const halide_buffer_t* buf)->device_interface } |]
  when (deviceInterface == nullPtr)
    $ error "cannot free device memory: device_interface is NULL"
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
    unless (n == fromIntegral (natVal (Proxy @n)))
      $ error
      $ "specified wrong number of dimensions: "
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
        when (onHost && not isDeviceNull)
          . error
          $ "buffer was allocated on host, but its device pointer is not NULL"
          <> "; did you forget a copyToHost in your pipeline?"
        when (not onHost && not isHostNull)
          . error
          $ "buffer was allocated on device, but its host pointer is not NULL"
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

-- | Set the @device_dirty@ flag to the given value.
setDeviceDirty :: Bool -> Ptr RawHalideBuffer -> IO ()
setDeviceDirty (fromIntegral . fromEnum -> b) p =
  [CU.exp| void { $(halide_buffer_t* p)->set_device_dirty($(bool b)) } |]

-- | Do we have changes on the device the have not been copied to the host?
isHostDirty :: Ptr RawHalideBuffer -> IO Bool
isHostDirty p =
  toBool <$> [CU.exp| bool { $(const halide_buffer_t* p)->host_dirty() } |]

-- | Set the @host_dirty@ flag to the given value.
-- setHostDirty :: Bool -> Ptr RawHalideBuffer -> IO ()
-- setHostDirty (fromIntegral . fromEnum -> b) p =
--   [CU.exp| void { $(halide_buffer_t* p)->set_host_dirty($(bool b)) } |]

-- | Copy the underlying memory from device to host.
bufferCopyToHost :: (HasCallStack) => Ptr RawHalideBuffer -> IO ()
bufferCopyToHost p = whenM (isDeviceDirty p) $ do
  raw <- peek p
  when (raw.halideBufferDeviceInterface == nullPtr)
    . error
    $ "device_dirty is set, but device_interface is NULL"
  when (raw.halideBufferHost == nullPtr)
    . error
    $ "host is NULL, did you forget to allocate memory?"
  [CU.block| void {
    auto& buf = *$(halide_buffer_t* p);
    buf.device_interface->copy_to_host(nullptr, &buf);
  } |]
  whenM (isDeviceDirty p)
    . error
    $ "device_dirty is set right after a copy_to_host; something went wrong..."

checkNumberOfDimensions :: forall n. (HasCallStack, KnownNat n) => RawHalideBuffer -> IO ()
checkNumberOfDimensions raw = do
  unless (fromIntegral (natVal (Proxy @n)) == raw.halideBufferDimensions)
    $ error
    $ "type-level and runtime number of dimensions do not match: "
    <> show (natVal (Proxy @n))
    <> " != "
    <> show raw.halideBufferDimensions

-- | Perform an action on a cropped buffer.
withCropped
  :: Ptr (HalideBuffer n a)
  -- ^ buffer
  -> Int
  -- ^ dimension
  -> Int
  -- ^ min
  -> Int
  -- ^ extent
  -> (Ptr (HalideBuffer n a) -> IO b)
  -- ^ what to do
  -> IO b
withCropped
  (castPtr -> src)
  (fromIntegral -> d)
  (fromIntegral -> min)
  (fromIntegral -> extent)
  action = do
    rank <- fromIntegral <$> [CU.exp| int { $(const halide_buffer_t* src)->dimensions } |]
    alloca $ \dst ->
      allocaArray rank $ \dstDim -> do
        [CU.block| void {
          auto const& src = *$(const halide_buffer_t* src);
          auto& dst = *$(halide_buffer_t* dst);
          auto const d = $(int d);

          dst = src;
          dst.dim = $(halide_dimension_t* dstDim);
          memcpy(dst.dim, src.dim, src.dimensions * sizeof(halide_dimension_t));

          if (dst.host != nullptr) {
            auto const shift = $(int min) - src.dim[d].min;
            dst.host += (shift * src.dim[d].stride) * ((src.type.bits + 7) / 8);
          }
          dst.dim[d].min = $(int min);
          dst.dim[d].extent = $(int extent);

          if (src.device != 0 && src.device_interface != nullptr) {
            src.device_interface->device_crop(nullptr, &src, &dst);
          }
        } |]
        action (castPtr dst)

getBufferExtent :: forall n a. (KnownNat n) => Ptr (HalideBuffer n a) -> Int -> IO Int
getBufferExtent (castPtr -> buf) (fromIntegral -> d)
  | d < fromIntegral (natVal (Proxy @n)) =
      fromIntegral <$> [CU.exp| int { $(const halide_buffer_t* buf)->dim[$(int d)].extent } |]
  | otherwise = error "index out of bounds"

peekScalar :: forall a. (HasCallStack, IsHalideType a) => Ptr (HalideBuffer 0 a) -> IO a
peekScalar p = withCopiedToHost p $ do
  raw <- peek (castPtr @_ @RawHalideBuffer p)
  checkNumberOfDimensions @0 raw
  when (raw.halideBufferHost == nullPtr) . error $ "host is NULL"
  peek $ castPtr @_ @a raw.halideBufferHost

-- | Specifies that @a@ can be converted to a list. This is very similar to 'GHC.Exts.IsList' except that
-- we read the list from a @'Ptr'@ rather than converting directly.
-- class IsListPeek a where
--   type ListPeekElem a :: Type
--   peekToList :: HasCallStack => Ptr a -> IO [ListPeekElem a]
type family NestedList (n :: Nat) (a :: Type) where
  NestedList 0 a = a
  NestedList 1 a = [a]
  NestedList 2 a = [[a]]
  NestedList 3 a = [[[a]]]
  NestedList 4 a = [[[[a]]]]
  NestedList 5 a = [[[[[a]]]]]

type family NestedListLevel (a :: Type) :: Nat where
  NestedListLevel [a] = 1 + NestedListLevel a
  NestedListLevel a = 0

type family NestedListType (a :: Type) :: Type where
  NestedListType [a] = NestedListType a
  NestedListType a = a

class
  ( KnownNat n
  , IsHalideType a
  , NestedList n a ~ b
  , NestedListLevel b ~ n
  , NestedListType b ~ a
  ) =>
  IsListPeek n a b
    | n a -> b
    , n b -> a
    , a b -> n
  where
  peekToList :: (HasCallStack) => Ptr (HalideBuffer n a) -> IO b

instance
  (IsHalideType a, NestedListLevel [a] ~ 1, NestedListType [a] ~ a)
  => IsListPeek 1 a [a]
  where
  peekToList p = withCopiedToHost p $ do
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    (HalideDimension min0 extent0 stride0 _) <- peekElemOff (halideBufferDim raw) 0
    let ptr0 = castPtr @_ @a (halideBufferHost raw)
    when (ptr0 == nullPtr) . error $ "host is NULL"
    forM [0 .. extent0 - 1] $ \i0 ->
      peekElemOff ptr0 (fromIntegral (min0 + stride0 * i0))

instance
  (IsHalideType a, NestedListLevel [[a]] ~ 2, NestedListType [[a]] ~ a)
  => IsListPeek 2 a [[a]]
  where
  peekToList p = withCopiedToHost p $ do
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    (HalideDimension min0 extent0 stride0 _) <- peekElemOff (halideBufferDim raw) 0
    (HalideDimension min1 extent1 stride1 _) <- peekElemOff (halideBufferDim raw) 1
    let ptr0 = castPtr @_ @a (halideBufferHost raw)
    when (ptr0 == nullPtr) . error $ "host is NULL"
    forM [0 .. extent0 - 1] $ \i0 -> do
      let ptr1 = ptr0 `advancePtr` fromIntegral (min0 + stride0 * i0)
      forM [0 .. extent1 - 1] $ \i1 ->
        peekElemOff ptr1 (fromIntegral (min1 + stride1 * i1))

instance
  (IsHalideType a, NestedListLevel [[[a]]] ~ 3, NestedListType [[[a]]] ~ a)
  => IsListPeek 3 a [[[a]]]
  where
  peekToList p = withCopiedToHost p $ do
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    (HalideDimension min0 extent0 stride0 _) <- peekElemOff (halideBufferDim raw) 0
    (HalideDimension min1 extent1 stride1 _) <- peekElemOff (halideBufferDim raw) 1
    (HalideDimension min2 extent2 stride2 _) <- peekElemOff (halideBufferDim raw) 2
    let ptr0 = castPtr @_ @a (halideBufferHost raw)
    when (ptr0 == nullPtr) . error $ "host is NULL"
    forM [0 .. extent0 - 1] $ \i0 -> do
      let ptr1 = ptr0 `advancePtr` fromIntegral (min0 + stride0 * i0)
      forM [0 .. extent1 - 1] $ \i1 -> do
        let ptr2 = ptr1 `advancePtr` fromIntegral (min1 + stride1 * i1)
        forM [0 .. extent2 - 1] $ \i2 ->
          peekElemOff ptr2 (fromIntegral (min2 + stride2 * i2))

instance
  (IsHalideType a, NestedListLevel [[[[a]]]] ~ 4, NestedListType [[[[a]]]] ~ a)
  => IsListPeek 4 a [[[[a]]]]
  where
  peekToList p = withCopiedToHost p $ do
    raw <- peek (castPtr @_ @RawHalideBuffer p)
    (HalideDimension min0 extent0 stride0 _) <- peekElemOff (halideBufferDim raw) 0
    (HalideDimension min1 extent1 stride1 _) <- peekElemOff (halideBufferDim raw) 1
    (HalideDimension min2 extent2 stride2 _) <- peekElemOff (halideBufferDim raw) 2
    (HalideDimension min3 extent3 stride3 _) <- peekElemOff (halideBufferDim raw) 3
    let ptr0 = castPtr @_ @a (halideBufferHost raw)
    when (ptr0 == nullPtr) . error $ "host is NULL"
    forM [0 .. extent0 - 1] $ \i0 -> do
      let ptr1 = ptr0 `advancePtr` fromIntegral (min0 + stride0 * i0)
      forM [0 .. extent1 - 1] $ \i1 -> do
        let ptr2 = ptr1 `advancePtr` fromIntegral (min1 + stride1 * i1)
        forM [0 .. extent2 - 1] $ \i2 -> do
          let ptr3 = ptr2 `advancePtr` fromIntegral (min2 + stride2 * i2)
          forM [0 .. extent3 - 1] $ \i3 ->
            peekElemOff ptr3 (fromIntegral (min3 + stride3 * i3))

-- | @withCopiedToHost buf action@ performs the action @action@ ensuring that @buf@ has been
-- copied to the host beforehand. If @buf@ is already on the host, no copying is performed.
withCopiedToHost :: Ptr (HalideBuffer n a) -> IO b -> IO b
withCopiedToHost (castPtr @_ @RawHalideBuffer -> buf) action = do
  raw <- peek buf
  let allocate = when (raw.halideBufferDevice /= 0) $ allocateHostMemory buf
      deallocate = when (raw.halideBufferDevice /= 0) $ freeHostMemory buf
  bracket_ allocate deallocate $ do
    when (raw.halideBufferDevice /= 0) $ do
      setDeviceDirty True buf
      bufferCopyToHost buf
    action
