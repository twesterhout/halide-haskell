{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Language.Halide.Kernel
-- Description : Compiling functions to kernels
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Kernel
  ( mkKernel
  , mkKernel'
  )
where

import Control.Exception (bracket)
import Control.Monad.Primitive (touch)
import Control.Monad.ST (RealWorld)
import Data.IORef
import Data.Kind (Type)
import Data.Primitive.PrimArray (MutablePrimArray)
import qualified Data.Primitive.PrimArray as P
import qualified Data.Primitive.Ptr as P
import Data.Proxy
import Foreign.C.Types (CUIntPtr (..))
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import Foreign.Storable
import GHC.TypeNats
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as CU
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Func
import Language.Halide.Type

importHalide

data ArgvStorage s
  = ArgvStorage
      {-# UNPACK #-} !(MutablePrimArray s (Ptr ()))
      {-# UNPACK #-} !(MutablePrimArray s CUIntPtr)

newArgvStorage :: Int -> IO (ArgvStorage RealWorld)
newArgvStorage n = ArgvStorage <$> P.newPinnedPrimArray n <*> P.newPinnedPrimArray n

setArgvStorage
  :: (All ValidArgument inputs, All ValidArgument outputs)
  => ArgvStorage RealWorld
  -> Arguments inputs
  -> Arguments outputs
  -> IO ()
setArgvStorage (ArgvStorage argv scalarStorage) inputs outputs = do
  let argvPtr = P.mutablePrimArrayContents argv
      scalarStoragePtr = P.mutablePrimArrayContents scalarStorage
      go :: All ValidArgument ts' => Int -> Arguments ts' -> IO Int
      go i Nil = pure i
      go i ((x :: t) ::: xs) = do
        fillSlot
          (castPtr $ argvPtr `P.advancePtr` i)
          (castPtr $ scalarStoragePtr `P.advancePtr` i)
          x
        go (i + 1) xs
  i <- go 0 inputs
  _ <- go i outputs
  touch argv
  touch scalarStorage

-- | Specifies that the type can be used as an argument to a kernel.
class ValidArgument (t :: Type) where
  fillSlot :: Ptr () -> Ptr () -> t -> IO ()

instance IsHalideType t => ValidArgument t where
  fillSlot :: Ptr () -> Ptr () -> t -> IO ()
  fillSlot argv scalarStorage x = do
    poke (castPtr scalarStorage :: Ptr t) x
    poke (castPtr argv :: Ptr (Ptr ())) scalarStorage

instance {-# OVERLAPPING #-} ValidArgument (Ptr CxxUserContext) where
  fillSlot :: Ptr () -> Ptr () -> Ptr CxxUserContext -> IO ()
  fillSlot argv scalarStorage x = do
    poke (castPtr scalarStorage :: Ptr (Ptr CxxUserContext)) x
    poke (castPtr argv :: Ptr (Ptr ())) scalarStorage

instance {-# OVERLAPPING #-} ValidArgument (Ptr (HalideBuffer n a)) where
  fillSlot :: Ptr () -> Ptr () -> Ptr (HalideBuffer n a) -> IO ()
  fillSlot argv _ x = do
    poke (castPtr argv :: Ptr (Ptr (HalideBuffer n a))) x

class ValidParameter (t :: Type) where
  appendToArgList :: Ptr (CxxVector CxxArgument) -> t -> IO ()
  prepareParameter :: IO t

instance IsHalideType a => ValidParameter (Expr a) where
  appendToArgList :: Ptr (CxxVector CxxArgument) -> Expr a -> IO ()
  appendToArgList v expr =
    withScalarParam expr $ \p ->
      [CU.exp| void { $(std::vector<Halide::Argument>* v)->emplace_back(
        $(Halide::Internal::Parameter const* p)->name(),
        Halide::Argument::InputScalar,
        $(Halide::Internal::Parameter const* p)->type(),
        $(Halide::Internal::Parameter const* p)->dimensions(),
        $(Halide::Internal::Parameter const* p)->get_argument_estimates()) } |]
  prepareParameter :: IO (Expr a)
  prepareParameter = ScalarParam <$> newIORef Nothing

instance (KnownNat n, IsHalideType a) => ValidParameter (Func n a) where
  appendToArgList :: Ptr (CxxVector CxxArgument) -> Func n a -> IO ()
  appendToArgList v func =
    withBufferParam func $ \p ->
      [CU.exp| void { $(std::vector<Halide::Argument>* v)->push_back(
        *$(Halide::ImageParam const* p)) } |]
  prepareParameter :: IO (Func n a)
  prepareParameter = BufferParam <$> newIORef Nothing

class PrepareParameters ts where
  prepareParameters :: IO (Arguments ts)

instance PrepareParameters '[] where
  prepareParameters :: IO (Arguments '[])
  prepareParameters = pure Nil

instance (ValidParameter t, PrepareParameters ts) => PrepareParameters (t ': ts) where
  prepareParameters :: IO (Arguments (t : ts))
  prepareParameters = do
    t <- prepareParameter @t
    ts <- prepareParameters @ts
    pure $ t ::: ts

prepareCxxArguments
  :: forall ts b
   . (All ValidParameter ts, KnownNat (Length ts))
  => Arguments ts
  -> (Ptr (CxxVector CxxArgument) -> IO b)
  -> IO b
prepareCxxArguments args action = do
  let count = fromIntegral (natVal (Proxy @(Length ts)))
      allocate =
        [CU.block| std::vector<Halide::Argument>* {
        auto p = new std::vector<Halide::Argument>{};
        p->reserve($(size_t count));
        return p;
      } |]
      destroy p = [CU.exp| void { delete $(std::vector<Halide::Argument>* p) } |]
  bracket allocate destroy $ \v -> do
    let go :: All ValidParameter ts' => Arguments ts' -> IO ()
        go Nil = pure ()
        go (x ::: xs) = appendToArgList v x >> go xs
    go args
    action v

deleteCxxUserContext :: FunPtr (Ptr CxxUserContext -> IO ())
deleteCxxUserContext = [C.funPtr| void deleteUserContext(Halide::JITUserContext* p) { delete p; } |]

wrapCxxUserContext :: Ptr CxxUserContext -> IO (ForeignPtr CxxUserContext)
wrapCxxUserContext = newForeignPtr deleteCxxUserContext

newEmptyCxxUserContext :: IO (ForeignPtr CxxUserContext)
newEmptyCxxUserContext =
  wrapCxxUserContext =<< [CU.exp| Halide::JITUserContext* { new Halide::JITUserContext{} } |]

deleteCxxCallable :: FunPtr (Ptr CxxCallable -> IO ())
deleteCxxCallable = [C.funPtr| void deleteCallable(Halide::Callable* p) { delete p; } |]

wrapCxxCallable :: Ptr CxxCallable -> IO (ForeignPtr CxxCallable)
wrapCxxCallable = newForeignPtr deleteCxxCallable

type family Lowered (t :: [Type]) :: [Type] where
  Lowered '[] = '[]
  Lowered (Expr a ': ts) = (a ': Lowered ts)
  Lowered (Func n a ': ts) = (Ptr (HalideBuffer n a) ': Lowered ts)

mkKernel'
  :: forall ts n a k
   . ( KnownNat n
     , IsHalideType a
     , Length ts ~ k
     , KnownNat k
     , PrepareParameters ts
     , All ValidParameter ts
     , All ValidArgument (Lowered ts)
     )
  => (Arguments ts -> IO (Func n a))
  -> IO (Arguments (Lowered ts) -> Ptr (HalideBuffer n a) -> IO ())
mkKernel' buildFunc = do
  parameters <- prepareParameters @ts
  func <- buildFunc parameters
  callable <-
    prepareCxxArguments parameters $ \v ->
      withFunc func $ \f -> do
        wrapCxxCallable
          =<< handleHalideExceptions
          =<< [CU.tryBlock| Halide::Callable* {
                return handle_halide_exceptions([=]() {
                  return new Halide::Callable{$(Halide::Func* f)->compile_to_callable(
                    *$(const std::vector<Halide::Argument>* v))};
                });
              } |]
  context <- newEmptyCxxUserContext
  let argc = 1 + fromIntegral (natVal (Proxy @k)) + 1
  storage@(ArgvStorage argv scalarStorage) <- newArgvStorage (fromIntegral argc)
  let argvPtr = P.mutablePrimArrayContents argv
      contextPtr = unsafeForeignPtrToPtr context
      callablePtr = unsafeForeignPtrToPtr callable
      kernel args out = do
        setArgvStorage storage (contextPtr ::: args) (out ::: Nil)
        [CU.exp| void {
          handle_halide_exceptions([=]() {
            return $(Halide::Callable* callablePtr)->call_argv_fast(
              $(int argc), $(const void* const* argvPtr));
          })
        } |]
        touch argv
        touch scalarStorage
        touch context
        touch callable
  pure kernel

-- | Convert a function that builds a Halide 'Func' into a normal Haskell
-- function acccepting scalars and 'HalideBuffer's.
mkKernel
  :: forall f kernel k ts n a r
   . ( FunctionArguments f ~ ts
     , FunctionReturn f ~ r
     , UnCurry f ts r
     , r ~ IO (Func n a)
     , KnownNat n
     , IsHalideType a
     , Length ts ~ k
     , KnownNat k
     , PrepareParameters ts
     , All ValidParameter ts
     , All ValidArgument (Lowered ts)
     , Curry (Lowered ts) (Ptr (HalideBuffer n a) -> IO ()) kernel
     )
  => f
  -> IO kernel
mkKernel buildFunc = do
  kernel <- mkKernel' (uncurryG @f @ts @r buildFunc)
  pure (curryG kernel)
