{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
    Arguments (..),
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
import Data.Int
import Data.Kind (Type)
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray)
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
import GHC.TypeNats
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Type
import System.IO.Unsafe (unsafePerformIO)

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
        ("Halide::Callable", [t|CxxCallable|]),
        ("Halide::JITUserContext", [t|CxxUserContext|]),
        ("Halide::Internal::Parameter", [t|CxxParameter|]),
        ("std::vector", [t|CxxVector|]),
        ("halide_buffer_t", [t|HalideBuffer|]),
        ("halide_type_t", [t|HalideType|])
      ]

C.include "<Halide.h>"
C.include "<math.h>"

newtype Expr a = Expr (ForeignPtr CxxExpr)

wrapCxxExpr :: Ptr CxxExpr -> IO (Expr a)
wrapCxxExpr = fmap Expr . newForeignPtr deleteCxxExpr

deleteCxxExpr :: FunPtr (Ptr CxxExpr -> IO ())
deleteCxxExpr = [C.funPtr| void deleteExpr(Halide::Expr *x) { delete x; } |]

withExpr :: Expr a -> (Ptr CxxExpr -> IO b) -> IO b
withExpr (Expr fp) = withForeignPtr fp

withExpr2 :: Expr a -> Expr a -> (Ptr CxxExpr -> Ptr CxxExpr -> IO b) -> IO b
withExpr2 a b f = withExpr a $ \aPtr -> withExpr b $ \bPtr -> f aPtr bPtr

mkVar :: Text -> IO (Expr Int32)
mkVar name =
  wrapCxxExpr
    =<< [CU.exp| Halide::Expr* {
          new Halide::Expr{Halide::Var{
            std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}}} } |]
  where
    s = T.encodeUtf8 name

unaryOp :: (Ptr CxxExpr -> IO (Ptr CxxExpr)) -> Expr a -> Expr a
unaryOp f a = unsafePerformIO $! withExpr a f >>= wrapCxxExpr

binaryOp :: (Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)) -> Expr a -> Expr a -> Expr a
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

newtype Func (n :: Nat) (a :: Type) = Func (ForeignPtr CxxFunc)

deleteCxxFunc :: FunPtr (Ptr CxxFunc -> IO ())
deleteCxxFunc = [C.funPtr| void deleteFunc(Halide::Func *x) { delete x; } |]

withFunc :: Func n a -> (Ptr CxxFunc -> IO b) -> IO b
withFunc (Func fp) = withForeignPtr fp

wrapCxxFunc :: Ptr CxxFunc -> IO (Func n a)
wrapCxxFunc = fmap Func . newForeignPtr deleteCxxFunc

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

class Args (a :: Type) (n :: Nat) | a -> n where
  toExprList :: a -> [ForeignPtr CxxExpr]

instance Args (Expr Int32) 1 where
  toExprList :: Expr Int32 -> [ForeignPtr CxxExpr]
  toExprList (Expr a) = [a]

instance Args (Expr Int32, Expr Int32) 2 where
  toExprList :: (Expr Int32, Expr Int32) -> [ForeignPtr CxxExpr]
  toExprList (Expr a, Expr b) = [a, b]

define :: Args args n => Text -> args -> Expr a -> IO (Func n a)
define name x (Expr y) = Func <$> defineFunc name (toExprList x) y

infix 9 !

(!) :: Args args n => Func n r -> args -> Expr r
(!) (Func func) args = unsafePerformIO $! Expr <$> applyFunc func (toExprList args)

printLoopNest :: Func n r -> IO ()
printLoopNest func = withFunc func $ \f ->
  [C.exp| void { $(Halide::Func* f)->print_loop_nest() } |]

realize1D :: (Storable a, IsHalideType a) => Func 1 a -> Int -> IO (Vector a)
realize1D func size = do
  buffer <- SM.new size
  withHalideBuffer buffer $ \x ->
    withFunc func $ \f ->
      [CU.exp| void {
        $(Halide::Func* f)->realize(
          Halide::Pipeline::RealizationArg{$(halide_buffer_t* x)}) } |]
  S.unsafeFreeze buffer

data Param f where
  ScalarParam :: IsHalideType a => Proxy a -> Text -> Param (Expr a)
  BufferParam :: (KnownNat n, IsHalideType a) => Proxy n -> Proxy a -> Text -> Param (Func n a)

mkScalarParam :: forall a. IsHalideType a => Text -> IO (Expr a)
mkScalarParam name = do
  let s = T.encodeUtf8 name
  with (halideTypeFor (Proxy @a)) $ \tp ->
    wrapCxxExpr
      =<< [CU.exp| Halide::Expr* {
        new Halide::Expr{
          Halide::Internal::Parameter{
            Halide::Type{*$(halide_type_t* tp)},
            false,
            0,
            std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}
          }.scalar_expr()}
      } |]

mkBufferParam :: forall n a. (KnownNat n, IsHalideType a) => Text -> IO (Func n a)
mkBufferParam name = do
  let s = T.encodeUtf8 name
      d = fromIntegral $ natVal (Proxy @n)
  with (halideTypeFor (Proxy @a)) $ \tp ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* {
        new Halide::Func{static_cast<Halide::Func>(
          Halide::ImageParam{
            Halide::Type{*$(halide_type_t* tp)},
            $(int d),
            std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}
          })}
      } |]

declareScalarParam :: forall a n b. IsHalideType a => Text -> (Expr a -> IO (Func n b)) -> a -> IO b
declareScalarParam name mkFunc value = do
  x <- mkScalarParam @a name
  f <- mkFunc x
  undefined

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

setArgvStorage :: All ValidArgument ts => ArgvStorage RealWorld -> Arguments n ts -> IO ()
setArgvStorage (ArgvStorage argv scalarStorage) args = do
  let argvPtr = P.mutablePrimArrayContents argv
      scalarStoragePtr = P.mutablePrimArrayContents scalarStorage
      go :: All ValidArgument ts => Int -> Arguments n ts -> IO ()
      go _ Nil = pure ()
      go i ((x :: t) ::: xs) = do
        fillSlot
          (castPtr $ argvPtr `P.advancePtr` i)
          (castPtr $ scalarStoragePtr `P.advancePtr` i)
          x
        go (i + 1) xs
  go 0 args
  touch argv
  touch scalarStorage

class ValidArgument (t :: Type) where
  fillSlot :: Ptr () -> Ptr () -> t -> IO ()

instance (Prim t, IsHalideType t) => ValidArgument t where
  fillSlot argv scalarStorage x = do
    P.writeOffPtr (castPtr scalarStorage :: Ptr t) 0 x
    P.writeOffPtr (castPtr argv :: Ptr (Ptr ())) 0 scalarStorage

instance {-# OVERLAPPING #-} ValidArgument (Ptr CxxUserContext) where
  fillSlot argv scalarStorage x = do
    P.writeOffPtr (castPtr scalarStorage :: Ptr (Ptr CxxUserContext)) 0 x
    P.writeOffPtr (castPtr argv :: Ptr (Ptr ())) 0 scalarStorage

instance {-# OVERLAPPING #-} ValidArgument (Ptr HalideBuffer) where
  fillSlot argv _ x = do
    P.writeOffPtr (castPtr argv :: Ptr (Ptr HalideBuffer)) 0 x

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

cxxCompileToCallable :: Func n a -> IO (ForeignPtr CxxCallable)
cxxCompileToCallable func =
  withFunc func $ \f -> do
    wrapCxxCallable
      =<< [CU.exp| Halide::Callable* { new Halide::Callable{
            $(Halide::Func* f)->compile_to_callable({})
          } } |]

testKernel1 :: IO (Arguments 1 '[Ptr HalideBuffer] -> IO ())
testKernel1 = do
  i <- mkVar "i"
  f <- define "f" i $ 2 * cast @Float i
  callable <- cxxCompileToCallable f
  storage@(ArgvStorage argv scalarStorage) <- newArgvStorage 2
  context <- newEmptyCxxUserContext
  let kernel args@(p ::: Nil) =
        withForeignPtr context $ \contextPtr ->
          withForeignPtr callable $ \callablePtr -> do
            setArgvStorage storage (contextPtr ::: args)
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

-- toCxxParameter :: Param f -> IO (Ptr CxxParameter)
-- toCxxParameter = undefined
--
-- deleteCxxParameter :: Ptr CxxParameter -> IO ()
-- deleteCxxParameter p = [CU.exp| void { delete $(Halide::Internal::Parameter* p) } |]
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