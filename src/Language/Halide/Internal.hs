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
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Halide.Internal
  ( Expr (..),
    mkVar,
    cast,
    Func (..),
    define,
    (!),
    printLoopNest,
    realize1D,
    testKernel1,
    testKernel2,
    Arguments (..),
    setName,
    mkKernel,
    print',
    -- mkFunc,
    -- applyFunc,
    -- defineFunc,
    -- printLoopNest,
    -- realizeOnBuffer,
    -- Typed interface
    -- TypedExpr (..),
    -- TypedFunc (..),
    -- define,
    -- (!),
    -- realizeTypedOnBuffer1D,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, (>=>))
import Control.Monad.Primitive (touch)
import Control.Monad.ST (RealWorld)
import Data.Constraint
import Data.IORef
import Data.Int
import Data.Kind (Type)
import Data.Primitive.PrimArray (MutablePrimArray)
import qualified Data.Primitive.PrimArray as P
import qualified Data.Primitive.Ptr as P
import Data.Primitive.Types (Prim)
import Data.Proxy
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.C.Types (CUIntPtr (..))
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import Foreign.Storable
import GHC.Stack (HasCallStack)
-- import GHC.TypeLits (ErrorMessage ((:<>:)))
-- import qualified GHC.TypeLits as GHC
import GHC.TypeNats
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Type
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection
import qualified Unsafe.Coerce

-- import qualified Language.C.Inline.Context as CC
-- import qualified Language.C.Types as CT
-- import qualified Language.C.Inline.Cpp.Exception as C
-- import qualified Language.C.Inline.Cpp.Exceptions as Legacy

C.context $
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> C.cppTypePairs
      [ ("Halide::Expr", [t|CxxExpr|]),
        ("Halide::Func", [t|CxxFunc|]),
        ("Halide::Param", [t|CxxParam|]),
        ("Halide::ImageParam", [t|CxxImageParam|]),
        ("Halide::Callable", [t|CxxCallable|]),
        ("Halide::JITUserContext", [t|CxxUserContext|]),
        ("Halide::Internal::Parameter", [t|CxxParameter|]),
        ("Halide::Argument", [t|CxxArgument|]),
        ("std::vector", [t|CxxVector|]),
        ("halide_buffer_t", [t|RawHalideBuffer|]),
        ("halide_type_t", [t|HalideType|])
      ]

C.include "<Halide.h>"
C.include "<math.h>"
C.include "<stdio.h>"

data Expr a
  = Expr (ForeignPtr CxxExpr)
  | ScalarParam (IORef (Maybe (ForeignPtr CxxParameter)))

wrapCxxExpr :: Ptr CxxExpr -> IO (Expr a)
wrapCxxExpr = fmap Expr . newForeignPtr deleteCxxExpr

deleteCxxExpr :: FunPtr (Ptr CxxExpr -> IO ())
deleteCxxExpr = [C.funPtr| void deleteExpr(Halide::Expr *x) { delete x; } |]

deleteCxxParameter :: FunPtr (Ptr CxxParameter -> IO ())
deleteCxxParameter = [C.funPtr| void deleteParameter(Halide::Internal::Parameter *p) { delete p; } |]

withExpr :: IsHalideType a => Expr a -> (Ptr CxxExpr -> IO b) -> IO b
withExpr x = withForeignPtr (exprToForeignPtr x)

mkScalarParameter :: forall a. IsHalideType a => Maybe Text -> IO (ForeignPtr CxxParameter)
mkScalarParameter maybeName = do
  with (halideTypeFor (Proxy @a)) $ \t -> do
    let createWithoutName =
          [CU.exp| Halide::Internal::Parameter* {
            new Halide::Internal::Parameter{Halide::Type{*$(halide_type_t* t)}, false, 0} } |]
        createWithName name =
          let s = T.encodeUtf8 name
           in [CU.exp| Halide::Internal::Parameter* {
                new Halide::Internal::Parameter{
                  Halide::Type{*$(halide_type_t* t)},
                  false,
                  0,
                  std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}}
              } |]
    newForeignPtr deleteCxxParameter =<< maybe createWithoutName createWithName maybeName

getScalarParameter ::
  forall a.
  IsHalideType a =>
  Maybe Text ->
  IORef (Maybe (ForeignPtr CxxParameter)) ->
  IO (ForeignPtr CxxParameter)
getScalarParameter name r = do
  readIORef r >>= \case
    Just fp -> pure fp
    Nothing -> do
      fp <- mkScalarParameter @a name
      writeIORef r (Just fp)
      pure fp

forceExpr :: forall a. IsHalideType a => Expr a -> IO (Expr a)
forceExpr x@(Expr _) = pure x
forceExpr (ScalarParam r) = do
  fp <- getScalarParameter @a Nothing r
  withForeignPtr fp $ \p ->
    wrapCxxExpr
      =<< [CU.exp| Halide::Expr* {
            new Halide::Expr{
              Halide::Internal::Variable::make(
                $(Halide::Internal::Parameter* p)->type(),
                $(Halide::Internal::Parameter* p)->name(),
                *$(Halide::Internal::Parameter* p))} } |]

withScalarParam :: forall a b. IsHalideType a => Expr a -> (Ptr CxxParameter -> IO b) -> IO b
withScalarParam (ScalarParam r) action = do
  fp <- getScalarParameter @a Nothing r
  withForeignPtr fp action
withScalarParam (Expr _) _ = error "withScalarParam called on Expr"

exprToForeignPtr :: IsHalideType a => Expr a -> ForeignPtr CxxExpr
exprToForeignPtr x =
  unsafePerformIO $!
    forceExpr x >>= \case
      (Expr fp) -> pure fp
      _ -> error "this cannot happen"

withExpr2 :: IsHalideType a => Expr a -> Expr a -> (Ptr CxxExpr -> Ptr CxxExpr -> IO b) -> IO b
withExpr2 a b f = withExpr a $ \aPtr -> withExpr b $ \bPtr -> f aPtr bPtr

mkVar :: Text -> IO (Expr Int32)
mkVar name =
  wrapCxxExpr
    =<< [CU.exp| Halide::Expr* {
          new Halide::Expr{Halide::Var{
            std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}}} } |]
  where
    s = T.encodeUtf8 name

unaryOp :: IsHalideType a => (Ptr CxxExpr -> IO (Ptr CxxExpr)) -> Expr a -> Expr a
unaryOp f a = unsafePerformIO $! withExpr a f >>= wrapCxxExpr

binaryOp :: IsHalideType a => (Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)) -> Expr a -> Expr a -> Expr a
binaryOp f a b = unsafePerformIO $! withExpr2 a b $ \aPtr bPtr -> f aPtr bPtr >>= wrapCxxExpr

instance (IsHalideType a, Num a) => Num (Expr a) where
  fromInteger :: Integer -> Expr a
  fromInteger x = unsafePerformIO $! wrapCxxExpr =<< toCxxExpr (fromInteger x :: a)
  (+) :: Expr a -> Expr a -> Expr a
  (+) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) + *$(Halide::Expr* b)} } |]
  (-) :: Expr a -> Expr a -> Expr a
  (-) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) - *$(Halide::Expr* b)} } |]
  (*) :: Expr a -> Expr a -> Expr a
  (*) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) * *$(Halide::Expr* b)} } |]
  abs :: Expr a -> Expr a
  abs = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::abs(*$(Halide::Expr* a))} } |]
  negate :: Expr a -> Expr a
  negate = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{ -(*$(Halide::Expr* a))} } |]
  signum :: Expr a -> Expr a
  signum = error "Num instance of (Expr a) does not implement signum"

instance (IsHalideType a, Fractional a) => Fractional (Expr a) where
  (/) :: Expr a -> Expr a -> Expr a
  (/) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) / *$(Halide::Expr* b)} } |]
  fromRational :: Rational -> Expr a
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (Castable a Double, Floating a) => Floating (Expr a) where
  pi :: Expr a
  pi = cast @a @Double . unsafePerformIO $! wrapCxxExpr =<< [CU.exp| Halide::Expr* { new Halide::Expr{M_PI} } |]
  exp :: Expr a -> Expr a
  exp = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::exp(*$(Halide::Expr* a))} } |]
  log :: Expr a -> Expr a
  log = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::log(*$(Halide::Expr* a))} } |]
  sqrt :: Expr a -> Expr a
  sqrt = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::sqrt(*$(Halide::Expr* a))} } |]
  (**) :: Expr a -> Expr a -> Expr a
  (**) = binaryOp $ \a b ->
    [CU.exp| Halide::Expr* { new Halide::Expr{Halide::pow(*$(Halide::Expr* a), *$(Halide::Expr* b))} } |]
  sin :: Expr a -> Expr a
  sin = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::sin(*$(Halide::Expr* a))} } |]
  cos :: Expr a -> Expr a
  cos = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cos(*$(Halide::Expr* a))} } |]
  tan :: Expr a -> Expr a
  tan = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::tan(*$(Halide::Expr* a))} } |]
  asin :: Expr a -> Expr a
  asin = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::asin(*$(Halide::Expr* a))} } |]
  acos :: Expr a -> Expr a
  acos = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::acos(*$(Halide::Expr* a))} } |]
  atan :: Expr a -> Expr a
  atan = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::atan(*$(Halide::Expr* a))} } |]
  sinh :: Expr a -> Expr a
  sinh = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::sinh(*$(Halide::Expr* a))} } |]
  cosh :: Expr a -> Expr a
  cosh = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cosh(*$(Halide::Expr* a))} } |]
  tanh :: Expr a -> Expr a
  tanh = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::tanh(*$(Halide::Expr* a))} } |]
  asinh :: Expr a -> Expr a
  asinh = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::asinh(*$(Halide::Expr* a))} } |]
  acosh :: Expr a -> Expr a
  acosh = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::acosh(*$(Halide::Expr* a))} } |]
  atanh :: Expr a -> Expr a
  atanh = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::atanh(*$(Halide::Expr* a))} } |]

cast :: forall to from. Castable to from => Expr from -> Expr to
cast x = unsafePerformIO $! withExpr x $ castImpl (Proxy @to) (Proxy @from) >=> wrapCxxExpr

data Func (n :: Nat) (a :: Type)
  = Func (ForeignPtr CxxFunc)
  | BufferParam (IORef (Maybe (ForeignPtr CxxImageParam)))

deleteCxxImageParam :: FunPtr (Ptr CxxImageParam -> IO ())
deleteCxxImageParam = [C.funPtr| void deleteImageParam(Halide::ImageParam* p) { delete p; } |]

mkBufferParameter :: forall n a. (KnownNat n, IsHalideType a) => Maybe Text -> IO (ForeignPtr CxxImageParam)
mkBufferParameter maybeName = do
  with (halideTypeFor (Proxy @a)) $ \t -> do
    let d = fromIntegral $ natVal (Proxy @n)
        createWithoutName =
          [CU.exp| Halide::ImageParam* {
            new Halide::ImageParam{Halide::Type{*$(halide_type_t* t)}, $(int d)} } |]
        createWithName name =
          let s = T.encodeUtf8 name
           in [CU.exp| Halide::ImageParam* {
                new Halide::ImageParam{
                      Halide::Type{*$(halide_type_t* t)},
                      $(int d),
                      std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}} } |]
    newForeignPtr deleteCxxImageParam =<< maybe createWithoutName createWithName maybeName

getBufferParameter ::
  forall n a.
  (KnownNat n, IsHalideType a) =>
  Maybe Text ->
  IORef (Maybe (ForeignPtr CxxImageParam)) ->
  IO (ForeignPtr CxxImageParam)
getBufferParameter name r =
  readIORef r >>= \case
    Just fp -> pure fp
    Nothing -> do
      fp <- mkBufferParameter @n @a name
      writeIORef r (Just fp)
      pure fp

withBufferParam ::
  forall n a b.
  (KnownNat n, IsHalideType a) =>
  Func n a ->
  (Ptr CxxImageParam -> IO b) ->
  IO b
withBufferParam (BufferParam r) action = do
  fp <- getBufferParameter @n @a Nothing r
  withForeignPtr fp action
withBufferParam (Func _) _ = error "withBufferParam called on Func"

class Named a where
  setName :: HasCallStack => a -> Text -> IO ()

instance IsHalideType a => Named (Expr a) where
  setName :: Expr a -> Text -> IO ()
  setName (Expr _) _ = error "cannot set the name of an expression that is not a parameter"
  setName (ScalarParam r) name = do
    _ <-
      maybe
        (mkScalarParameter @a (Just name))
        (error "the name of this Expr has already been set")
        =<< readIORef r
    pure ()

instance (KnownNat n, IsHalideType a) => Named (Func n a) where
  setName :: Func n a -> Text -> IO ()
  setName (Func _) _ = error "the name of this Func has already been set"
  setName (BufferParam r) name = do
    _ <-
      maybe
        (mkBufferParameter @n @a (Just name))
        (error "the name of this Func has already been set")
        =<< readIORef r
    pure ()

deleteCxxFunc :: FunPtr (Ptr CxxFunc -> IO ())
deleteCxxFunc = [C.funPtr| void deleteFunc(Halide::Func *x) { delete x; } |]

withFunc :: (KnownNat n, IsHalideType a) => Func n a -> (Ptr CxxFunc -> IO b) -> IO b
withFunc f = withForeignPtr (funcToForeignPtr f)

wrapCxxFunc :: Ptr CxxFunc -> IO (Func n a)
wrapCxxFunc = fmap Func . newForeignPtr deleteCxxFunc

forceFunc :: forall n a. (KnownNat n, IsHalideType a) => Func n a -> IO (Func n a)
forceFunc x@(Func _) = pure x
forceFunc (BufferParam r) = do
  fp <- getBufferParameter @n @a Nothing r
  withForeignPtr fp $ \p ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* {
            new Halide::Func{static_cast<Halide::Func>(*$(Halide::ImageParam* p))} } |]

funcToForeignPtr :: (KnownNat n, IsHalideType a) => Func n a -> ForeignPtr CxxFunc
funcToForeignPtr x =
  unsafePerformIO $!
    forceFunc x >>= \case
      (Func fp) -> pure fp
      _ -> error "this cannot happen"

applyFunc :: ForeignPtr CxxFunc -> [ForeignPtr CxxExpr] -> IO (ForeignPtr CxxExpr)
applyFunc func args =
  withForeignPtr func $ \f ->
    withExprMany args $ \v ->
      newForeignPtr deleteCxxExpr
        =<< [CU.exp| Halide::Expr* {
              new Halide::Expr{(*$(Halide::Func* f))(*$(std::vector<Halide::Expr>* v))} } |]

defineFunc :: Text -> [ForeignPtr CxxExpr] -> ForeignPtr CxxExpr -> IO (ForeignPtr CxxFunc)
defineFunc name args expr = do
  let s = T.encodeUtf8 name
  withExprMany args $ \x ->
    withForeignPtr expr $ \y ->
      newForeignPtr deleteCxxFunc
        =<< [CU.block| Halide::Func* {
              Halide::Func f{std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}};
              f(*$(std::vector<Halide::Expr>* x)) = *$(Halide::Expr* y);
              return new Halide::Func{f};
            } |]

withExprMany :: [ForeignPtr CxxExpr] -> (Ptr (CxxVector CxxExpr) -> IO a) -> IO a
withExprMany xs f = do
  let count = fromIntegral (length xs)
      allocate =
        [CU.block| std::vector<Halide::Expr>* {
          auto v = new std::vector<Halide::Expr>{};
          v->reserve($(size_t count));
          return v;
        } |]
      destroy v = do
        [CU.exp| void { delete $(std::vector<Halide::Expr>* v) } |]
        forM_ xs touchForeignPtr
  bracket allocate destroy $ \v -> do
    forM_ xs $ \fp ->
      let p = unsafeForeignPtrToPtr fp
       in [CU.exp| void { $(std::vector<Halide::Expr>* v)->push_back(*$(Halide::Expr* p)) } |]
    f v

class ValidIndex (a :: Type) (n :: Nat) | a -> n where
  toExprList :: a -> [ForeignPtr CxxExpr]

instance ValidIndex (Expr Int32) 1 where
  toExprList :: Expr Int32 -> [ForeignPtr CxxExpr]
  toExprList a = [exprToForeignPtr a]

instance ValidIndex (Expr Int32, Expr Int32) 2 where
  toExprList :: (Expr Int32, Expr Int32) -> [ForeignPtr CxxExpr]
  toExprList (a, b) = [exprToForeignPtr a, exprToForeignPtr b]

define :: (ValidIndex i n, IsHalideType a) => Text -> i -> Expr a -> IO (Func n a)
define name x y = Func <$> defineFunc name (toExprList x) (exprToForeignPtr y)

infix 9 !

(!) :: (ValidIndex i n, KnownNat n, IsHalideType r) => Func n r -> i -> Expr r
(!) func args = unsafePerformIO $! Expr <$> applyFunc (funcToForeignPtr func) (toExprList args)

printLoopNest :: (KnownNat n, IsHalideType r) => Func n r -> IO ()
printLoopNest func = withFunc func $ \f ->
  [C.exp| void { $(Halide::Func* f)->print_loop_nest() } |]

realize1D :: (Storable a, IsHalideType a) => Func 1 a -> Int -> IO (Vector a)
realize1D func size = do
  buffer <- SM.new size
  withHalideBuffer buffer $ \x -> do
    let b = castPtr x
    withFunc func $ \f ->
      [CU.exp| void {
        $(Halide::Func* f)->realize(
          Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)}) } |]
  S.unsafeFreeze buffer

infixr 5 :::

data Arguments (n :: Nat) (k :: [Type]) where
  Nil :: Arguments 0 '[]
  (:::) :: (KnownNat n, KnownNat (n + 1)) => !t -> !(Arguments n ts) -> Arguments (n + 1) (t ': ts)

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

data ArgvStorage s
  = ArgvStorage
      {-# UNPACK #-} !(MutablePrimArray s (Ptr ()))
      {-# UNPACK #-} !(MutablePrimArray s CUIntPtr)

newArgvStorage :: Int -> IO (ArgvStorage RealWorld)
newArgvStorage n = ArgvStorage <$> P.newPinnedPrimArray n <*> P.newPinnedPrimArray n

setArgvStorage ::
  (All ValidArgument inputs, All ValidArgument outputs) =>
  ArgvStorage RealWorld ->
  Arguments n inputs ->
  Arguments m outputs ->
  IO ()
setArgvStorage (ArgvStorage argv scalarStorage) inputs outputs = do
  let argvPtr = P.mutablePrimArrayContents argv
      scalarStoragePtr = P.mutablePrimArrayContents scalarStorage
      go :: All ValidArgument ts' => Int -> Arguments n' ts' -> IO Int
      go i Nil = pure i
      go i ((x :: t) ::: xs) = do
        putStrLn $ "Filling slot " <> show i
        fillSlot
          (castPtr $ argvPtr `P.advancePtr` i)
          (castPtr $ scalarStoragePtr `P.advancePtr` i)
          x
        go (i + 1) xs
  i <- go 0 inputs
  _ <- go i outputs
  touch argv
  touch scalarStorage

class ValidArgument (t :: Type) where
  fillSlot :: Ptr () -> Ptr () -> t -> IO ()

instance (Prim t, IsHalideType t) => ValidArgument t where
  fillSlot :: Ptr () -> Ptr () -> t -> IO ()
  fillSlot argv scalarStorage x = do
    P.writeOffPtr (castPtr scalarStorage :: Ptr t) 0 x
    print =<< P.readOffPtr (castPtr scalarStorage :: Ptr Float) 0
    P.writeOffPtr (castPtr argv :: Ptr (Ptr ())) 0 scalarStorage

instance {-# OVERLAPPING #-} ValidArgument (Ptr CxxUserContext) where
  fillSlot :: Ptr () -> Ptr () -> Ptr CxxUserContext -> IO ()
  fillSlot argv scalarStorage x = do
    P.writeOffPtr (castPtr scalarStorage :: Ptr (Ptr CxxUserContext)) 0 x
    print =<< P.readOffPtr (castPtr scalarStorage :: Ptr CUIntPtr) 0
    P.writeOffPtr (castPtr argv :: Ptr (Ptr ())) 0 scalarStorage

instance {-# OVERLAPPING #-} ValidArgument (Ptr (HalideBuffer n a)) where
  fillSlot :: Ptr () -> Ptr () -> Ptr (HalideBuffer n a) -> IO ()
  fillSlot argv _ x = do
    P.writeOffPtr (castPtr argv :: Ptr (Ptr (HalideBuffer n a))) 0 x

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
      [CU.exp| void { $(std::vector<Halide::Argument>* v)->push_back(*$(Halide::ImageParam const* p)) } |]
  prepareParameter :: IO (Func n a)
  prepareParameter = BufferParam <$> newIORef Nothing

class PrepareParameters k ts where
  prepareParameters :: IO (Arguments k ts)

instance PrepareParameters 0 '[] where
  prepareParameters :: IO (Arguments 0 '[])
  prepareParameters = pure Nil

instance (ValidParameter t, PrepareParameters (k - 1) ts, KnownNat k, KnownNat (k - 1)) => PrepareParameters k (t ': ts) where
  prepareParameters :: IO (Arguments k (t : ts))
  prepareParameters = do
    t <- prepareParameter @t
    ts <- prepareParameters @(k - 1) @ts
    let proof :: k :~: ((k - 1) + 1)
        proof = Unsafe.Coerce.unsafeCoerce (Refl :: Int :~: Int)
    case proof of
      Refl -> pure $ t ::: ts

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

{-
type family FunctionArgumentTypes (f :: Type) :: [Type] where
  FunctionArgumentTypes (a -> b) = a ': FunctionArgumentTypes b
  FunctionArgumentTypes r = '[]

type family FunctionArgumentCount (f :: Type) :: Nat where
  FunctionArgumentCount (a -> b) = 1 + FunctionArgumentCount b
  FunctionArgumentCount r = 0

type family FunctionReturnType (f :: Type) :: Type where
  FunctionReturnType (a -> b) = FunctionReturnType b
  FunctionReturnType r = r

type family UnCurried f :: Type where
  UnCurried (x1 -> x2 -> x3 -> x4 -> x5 -> r) = Arguments 5 '[x1, x2, x3, x4, x5] -> r
  UnCurried (x1 -> x2 -> x3 -> x4 -> r) = Arguments 4 '[x1, x2, x3, x4] -> r
  UnCurried (x1 -> x2 -> x3 -> r) = Arguments 3 '[x1, x2, x3] -> r
  UnCurried (x1 -> x2 -> r) = Arguments 2 '[x1, x2] -> r
  UnCurried (x1 -> r) = Arguments 1 '[x1] -> r
  UnCurried r = Arguments 0 '[] -> r

-- (UnCurried f ~ (Arguments (FunctionArgumentCount f) (FunctionArgumentTypes f) -> FunctionReturnType f)) =>
class UnCurry f where
  uncurry' :: f -> UnCurried f

class Curry f where
  curry' :: UnCurried f -> f

newtype Return b = Return b

instance UnCurry (Return b) where
  uncurry' f Nil = f

instance UnCurry (x1 -> Return b) where
  uncurry' f (x1 ::: Nil) = f x1

instance UnCurry (x1 -> x2 -> Return b) where
  uncurry' f (x1 ::: x2 ::: Nil) = f x1 x2

instance UnCurry (x1 -> x2 -> x3 -> Return b) where
  uncurry' f (x1 ::: x2 ::: x3 ::: Nil) = f x1 x2 x3

instance UnCurry (x1 -> x2 -> x3 -> x4 -> Return b) where
  uncurry' f (x1 ::: x2 ::: x3 ::: x4 ::: Nil) = f x1 x2 x3 x4
-}

type family Curried f :: Type where
  Curried (Arguments 5 '[x1, x2, x3, x4, x5] -> r) = x1 -> x2 -> x3 -> x4 -> x5 -> r
  Curried (Arguments 4 '[x1, x2, x3, x4] -> r) = x1 -> x2 -> x3 -> x4 -> r
  Curried (Arguments 3 '[x1, x2, x3] -> r) = x1 -> x2 -> x3 -> r
  Curried (Arguments 2 '[x1, x2] -> r) = x1 -> x2 -> r
  Curried (Arguments 1 '[x1] -> r) = x1 -> r
  Curried (Arguments 0 '[] -> r) = r

class Curry f where
  curry' :: f -> Curried f

instance Curry (Arguments 0 '[] -> r) where
  curry' f = f Nil

instance Curry (Arguments 1 '[x1] -> r) where
  curry' f x1 = f (x1 ::: Nil)

instance Curry (Arguments 2 '[x1, x2] -> r) where
  curry' f x1 x2 = f (x1 ::: x2 ::: Nil)

instance Curry (Arguments 3 '[x1, x2, x3] -> r) where
  curry' f x1 x2 x3 = f (x1 ::: x2 ::: x3 ::: Nil)

type family UnCurried f :: Type where
  UnCurried (x1 -> x2 -> x3 -> x4 -> x5 -> r) = Arguments 5 '[x1, x2, x3, x4, x5] -> r
  UnCurried (x1 -> x2 -> x3 -> x4 -> r) = Arguments 4 '[x1, x2, x3, x4] -> r
  UnCurried (x1 -> x2 -> x3 -> r) = Arguments 3 '[x1, x2, x3] -> r
  UnCurried (x1 -> x2 -> r) = Arguments 2 '[x1, x2] -> r
  UnCurried (x1 -> r) = Arguments 1 '[x1] -> r
  UnCurried r = Arguments 0 '[] -> r

class UnCurry f where
  uncurry' :: f -> UnCurried f

instance UnCurry (IO b) where
  uncurry' f Nil = f

instance UnCurry (x1 -> IO b) where
  uncurry' f (x1 ::: Nil) = f x1

instance UnCurry (x1 -> x2 -> IO b) where
  uncurry' f (x1 ::: x2 ::: Nil) = f x1 x2

instance UnCurry (x1 -> x2 -> x3 -> IO b) where
  uncurry' f (x1 ::: x2 ::: x3 ::: Nil) = f x1 x2 x3

instance UnCurry (x1 -> x2 -> x3 -> x4 -> IO b) where
  uncurry' f (x1 ::: x2 ::: x3 ::: x4 ::: Nil) = f x1 x2 x3 x4

mkKernel ::
  forall f kernel k ts n a.
  ( KnownNat k,
    KnownNat (k + 1),
    KnownNat n,
    IsHalideType a,
    All ValidParameter ts,
    All ValidArgument (Lowered ts),
    PrepareParameters k ts,
    UnCurried f ~ (Arguments k ts -> IO (Func n a)),
    UnCurry f,
    kernel ~ (Arguments k (Lowered ts) -> Ptr (HalideBuffer n a) -> IO ()),
    Curry kernel
  ) =>
  f ->
  -- (Arguments k ts -> IO (Func n a)) ->
  IO (Curried kernel)
-- Arguments k (Lowered ts) -> Ptr (HalideBuffer n a) -> IO ())
mkKernel buildFunc = do
  parameters <- prepareParameters @k @ts
  func <- (uncurry' buildFunc) parameters
  -- withFunc func $ \f ->
  --   [CU.exp| void {
  --     $(Halide::Func* f)->trace_stores()
  --   } |]
  callable <-
    prepareCxxArguments parameters $ \v ->
      withFunc func $ \f -> do
        wrapCxxCallable
          =<< [CU.block| Halide::Callable* {
                try {
                  Halide::Callable c = $(Halide::Func* f)->compile_to_callable(*$(const std::vector<Halide::Argument>* v));
                  return new Halide::Callable{std::move(c)};
                } catch (Halide::CompileError& e) {
                  fprintf(stderr, "error: %s\n", e.what());
                  throw e;
                }
              } |]
  context <- newEmptyCxxUserContext
  let argc = 1 + fromIntegral (natVal (Proxy @k)) + 1
  storage@(ArgvStorage argv scalarStorage) <- newArgvStorage (fromIntegral argc)
  -- putStrLn $ "argc = " <> show argc
  let kernel :: kernel
      kernel args out =
        withForeignPtr context $ \contextPtr ->
          withForeignPtr callable $ \callablePtr -> do
            setArgvStorage storage (contextPtr ::: args) (out ::: Nil)
            let argvPtr = P.mutablePrimArrayContents argv
            [CU.exp| void { $(Halide::Callable* callablePtr)->call_argv_fast(
              $(int argc), $(const void* const* argvPtr)) } |]
            -- [CU.exp| void {
            --   $(Halide::Callable* callablePtr)->operator()($(halide_buffer_t* p))
            -- } |]
            touch argv
            touch scalarStorage
  pure (curry' kernel)

prepareCxxArguments ::
  forall k ts b.
  (KnownNat k, All ValidParameter ts) =>
  Arguments k ts ->
  (Ptr (CxxVector CxxArgument) -> IO b) ->
  IO b
prepareCxxArguments args action = do
  let count = fromIntegral (natVal (Proxy @k))
      allocate =
        [CU.block| std::vector<Halide::Argument>* {
        auto p = new std::vector<Halide::Argument>{};
        p->reserve($(size_t count));
        return p;
      } |]
      destroy p = [CU.exp| void { delete $(std::vector<Halide::Argument>* p) } |]
  bracket allocate destroy $ \v -> do
    let go :: All ValidParameter ts' => Arguments k' ts' -> IO ()
        go Nil = pure ()
        go (x ::: xs) = appendToArgList v x >> go xs
    go args
    -- [CU.block| void {
    --   printf("v.size() = %ul\n", $(std::vector<Halide::Argument>* v)->size());
    -- } |]
    action v

-- uncurry' :: f -> Arguments (FunctionArgumentCount f) (FunctionArgumentTypes f) -> FunctionReturnType f
-- uncurry' f Nil = f
-- uncurry' f (x ::: xs) = uncurry (f x) xs

--
--
-- instance UnCurry (x1 -> x2 -> r) (Arguments 2 '[x1, x2] -> r) where
--   uncurry f (x1 ::: x2 ::: Nil) = f x1 x2

-- Expr ~ Halide::Expr
--        Halide::Param<T> or Halide::ImageParam<T, N>
--                            ------------------------
--           Scalar                Buffer halide_buffer_t
--
-- Param<float> scale{"scale"};
--
-- Halide::Func::compile_to_callable(std::vector<Halide::Argument>, ...)
--
-- define "f" i

-- mkKernel $ \(x :: Expr Float) (y :: Func 1 Float) -> do
--   ...
--   -- return Func 2 Float

-- hehe!!!
--
-- declare :: Text -> (t -> IO (Arguments n ts, Func n a)) -> IO (Arguments (n + 1) (Param t : ts), Func n a)
--
-- declare :: Text -> (t -> IO (Func n a)) -> IO (Arguments 1 [Param t], Func n a)
--
-- mkKernel $ do
--   declare "x" $ \(x :: Expr a) -> do
--     ...
--
-- declare "x" $ \(x :: Func 1 Float) ->
--   declare "y" $ \(y :: Func 1 Float) -> do
--     ...
--
--
-- do
--   x <- ...
--   y <- ...
--   pure ()
--
-- do
--   x <- ...
--   pure ()
--
-- withCxxArguments :: All IsParam ts =>
--                     Arguments k ts -> (Ptr (CxxVector CxxArgument) -> IO a) -> IO a
--
--
-- data Expr
-- data P (t :: String) (a :: Type)
--
-- mkKernel $ \(a' :: P "a" Float) (b' :: P "b" Float) -> do
--   let a = Expr a'
--       b = Expr b'

-- data Expr a where
--  Expr :: ForeignPtr CxxExpr -> Expr a
--  Param :: IORef Text -> Expr a
--
-- forceExpr :: Expr a -> IO (Expr a)
-- forceExpr x@(Expr _) = pure x
-- forceExpr (Param name) = do
--   name' <- deRef name
--   constructExprFromParam name'
--
-- mkKernel $ \a b c -> do
--  rename a "a"
--  rename b "b"
--  rename c "c"
--  a <- forceExpr a
--  b <- forceExpr b
--  c <- forceExpr c
--  i <- mkVar "i"
--  j <- mkVar "j"
--  define "out" i j $
--      bool (i == j)
--          (c * (a ! i + b ! i) / d)
--          0
--
--
-- identityMatrix <-
--  mkKernel $
--    declare "a" $ \a ->
--      declare "b" $ \b ->
--        declare "c" $ \c ->
--          declare "d" $ \d -> do
--    $(declareParams ["a", "b", "c", "d"]) do
--      i <- mkVar "i"
--      j <- mkVar "j"
--      define "out" i j $
--          bool (i == j)
--              (c * (a ! i + b ! i) / d)
--              0
-- out <- SM.new (10, 10)
-- identityMatrix (a, b, 1, 2) out
-- print =<< SM.unsafeFreeze out

-- toCallable :: All IsParam ts =>
--               Arguments k ts ->
--               Func n a ->
--               IO (Ptr CxxUserContext -> Arguments n (Lowered ts') -> Ptr (HalideBuffer n a) -> IO ())
--
-- class Callable a b | a -> b
--   curry :: a -> b
--
-- instance Callable (Ptr CxxUserContext -> Arguments 1 '[t] -> a -> IO ()) (Ptr CxxUserContext -> t -> a -> IO ()) where
--   curry f x1 x2 x3 = f x1 (x2 ::: Nil) x3
--
-- instance Callable (Ptr CxxUserContext -> Arguments 2 '[t1, t2] -> a -> IO ()) (Ptr CxxUserContext -> t1 -> t2 -> a -> IO ()) where
--   curry f x1 x2 x3 x4 = f x1 (x2 ::: x3 ::: Nil) x4
--
-- data family GetFirstArg f where
--  GetFirstArg (a -> b) = a
--  GetFirstArg b = TypeError ...
--
-- f
-- (Expr a1 -> Expr a2 -> Expr a3 -> IO b) -> (Arguments 3 [Expr a1, Expr a2, Expr a3] -> IO b)

cxxCompileToCallable :: (KnownNat n, IsHalideType a) => Func n a -> IO (ForeignPtr CxxCallable)
cxxCompileToCallable func =
  withFunc func $ \f -> do
    wrapCxxCallable
      -- NOTE: we're cheating here and compiling a function without arguments
      =<< [CU.exp| Halide::Callable* { new Halide::Callable{
            $(Halide::Func* f)->compile_to_callable({})
          } } |]

testKernel1 :: IO (Arguments 0 '[] -> Ptr (HalideBuffer 1 Float) -> IO ())
testKernel1 = do
  i <- mkVar "i"
  f <- define "f" i $ 2 * cast @Float i
  callable <- cxxCompileToCallable f
  storage@(ArgvStorage argv scalarStorage) <- newArgvStorage 2
  context <- newEmptyCxxUserContext
  let kernel args@Nil p =
        withForeignPtr context $ \contextPtr ->
          withForeignPtr callable $ \callablePtr -> do
            setArgvStorage storage (contextPtr ::: args) (p ::: Nil)
            let argvPtr = P.mutablePrimArrayContents argv
            [CU.exp| void {
                $(Halide::Callable* callablePtr)->call_argv_fast(2, $(const void* const* argvPtr))
              } |]
            -- [CU.exp| void {
            --   $(Halide::Callable* callablePtr)->operator()($(halide_buffer_t* p))
            -- } |]
            touch argv
            touch scalarStorage
  pure kernel

print' :: IsHalideType a => Expr a -> Expr a
print' = unaryOp $ \e -> [CU.exp| Halide::Expr* { new Halide::Expr{print(*$(Halide::Expr* e))} } |]

testKernel2 :: IO ()
testKernel2 = do
  [CU.block| void {
    using namespace Halide;
    Buffer<float> output(10);

    auto c = []() {
      Param<float> a{"a"};
      Func func{"func"};
      Var j{"j"};

      Expr e = a;
      func(j) = e;

      return func.compile_to_callable({a});
    }();

    void* argv[3];
    void* scalar_storage[3];

    JITUserContext empty_context;
    *(JITUserContext**)(scalar_storage + 0) = &empty_context;
    argv[0] = &scalar_storage[0];

    *(float*)(scalar_storage + 1) = 3.0f;
    argv[1] = &scalar_storage[1];

    argv[2] = output.raw_buffer();

    // c(3.0f, output);
    c.call_argv_fast(3, argv);
    for (auto i = 0; i < 10; ++i) {
      printf("%f, ", output(i));
    }
    printf("\n");
  } |]

-- toCxxParameter :: Param f -> IO (Ptr CxxParameter)
-- toCxxParameter = undefined
--
--
-- mkParamExpr1 :: Param (Expr a) -> (Expr a -> IO b) -> IO b
-- mkParamExpr1 x f = do
--   bracket (toCxxParameter x) deleteCxxParameter $ \ptr -> do
--     expr <- wrapCxxExpr =<< [CU.exp| Halide::Expr* { new Halide::Expr{$(Halide::Internal::Parameter* ptr)->scalar_expr()} } |]
--     f expr

-- mkParamFunc1 :: Param (Func n a) -> (Func n a -> IO b) -> IO b
-- mkParamFunc1 x f = do
--   bracket (toCxxParameter x) deleteCxxParameter $ \ptr -> do
--     expr <- wrapCxxExpr =<< [CU.exp| Halide::Func* { new Halide::Func{$(Halide::Internal::Parameter* ptr)->scalar_expr()} } |]
--     f expr

-- instance IsHalideType a => TransformExprToParam (Expr a) where
--   TransformedType (Expr a) =

-- mkKernel :: (IO (Func n b)) ->
--             IO (IsBufferType buffer n b => buffer -> IO ())
-- mkKernel :: (Expr a -> IO (Func n b)) ->
--             IO (IsBufferType buffer n b => a -> buffer -> IO ())

-- mkKernel :: (args -> IO (Func n a))
