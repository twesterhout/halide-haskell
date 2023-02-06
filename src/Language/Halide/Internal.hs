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
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Halide.Internal
  ( Expr (..),
    mkExpr,
    mkVar,
    cast,
    printed,
    Func (..),
    define,
    update,
    (!),
    printLoopNest,
    realize1D,
    setName,
    mkKernel,
    mkKernel',
    evaluate,
    --
    equal,
    bool,
    IsHalideType,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Control.Monad.Primitive (touch)
import Control.Monad.ST (RealWorld)
import Data.Constraint
import Data.IORef
import Data.Int
import Data.Kind (Type)
import Data.Primitive.PrimArray (MutablePrimArray)
import qualified Data.Primitive.PrimArray as P
import qualified Data.Primitive.Ptr as P
import Data.Proxy
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.C.Types (CDouble, CUIntPtr (..))
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
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Cpp.Exception as CU
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Type
import System.IO.Unsafe (unsafePerformIO)

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

C.verbatim
  "\
  \template <class Func>                               \n\
  \auto handle_halide_exceptions(Func&& func) {        \n\
  \  try {                                             \n\
  \    return func();                                  \n\
  \  } catch(Halide::RuntimeError& e) {                \n\
  \    throw std::runtime_error{e.what()};             \n\
  \  } catch(Halide::CompileError& e) {                \n\
  \    throw std::runtime_error{e.what()};             \n\
  \  } catch(Halide::InternalError& e) {               \n\
  \    throw std::runtime_error{e.what()};             \n\
  \  } catch(Halide::Error& e) {                       \n\
  \    throw std::runtime_error{e.what()};             \n\
  \  }                                                 \n\
  \}                                                   \n\
  \"

-- defineCastableInstances
defineIsHalideTypeInstances

-- instance Castable Bool Bool where
--   castImpl _ _ p = [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cast(Halide::UInt(1), *$(Halide::Expr* p))} } |]

instance IsHalideType Bool where
  halideTypeFor _ = HalideType HalideTypeUInt 1 1
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{cast(Halide::UInt(1), Halide::Expr{$(int b)})} } |]
    where
      b = fromIntegral (fromEnum x)

-- defineCurriedTypeFamily

-- class Curry f where
--   curry' :: f -> Curried f

-- defineCurryInstances

-- defineUnCurriedTypeFamily

-- class UnCurry f where
--   uncurry' :: f -> UnCurried f

-- defineUnCurryInstances

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

mkExpr :: IsHalideType a => a -> Expr a
mkExpr x = unsafePerformIO $! wrapCxxExpr =<< toCxxExpr x

instance (IsHalideType a, Num a) => Num (Expr a) where
  fromInteger :: Integer -> Expr a
  fromInteger x = unsafePerformIO $! wrapCxxExpr =<< toCxxExpr (fromInteger x :: a)
  (+) :: Expr a -> Expr a -> Expr a
  (+) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) + *$(Halide::Expr* b)} } |]
  (-) :: Expr a -> Expr a -> Expr a
  (-) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) - *$(Halide::Expr* b)} } |]
  (*) :: Expr a -> Expr a -> Expr a
  (*) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) * *$(Halide::Expr* b)} } |]

  -- In Halide, unlike in C, abs of a signed integer returns an unsigned integer of the same bit width.
  -- So here, we insert an additional cast to force the resulting type to be the same as the input type.
  abs :: Expr a -> Expr a
  abs =
    cast @a
      . unaryOp (\a -> [CU.exp| Halide::Expr* { new Halide::Expr{Halide::abs(*$(Halide::Expr* a))} } |])
  negate :: Expr a -> Expr a
  negate = unaryOp $ \a -> [CU.exp| Halide::Expr* { new Halide::Expr{ -(*$(Halide::Expr* a))} } |]
  signum :: Expr a -> Expr a
  signum = error "Num instance of (Expr a) does not implement signum"

instance (IsHalideType a, Fractional a) => Fractional (Expr a) where
  (/) :: Expr a -> Expr a -> Expr a
  (/) = binaryOp $ \a b -> [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) / *$(Halide::Expr* b)} } |]
  fromRational :: Rational -> Expr a
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (IsHalideType a, Floating a) => Floating (Expr a) where
  pi :: Expr a
  pi = cast @a @CDouble . unsafePerformIO $! wrapCxxExpr =<< [CU.exp| Halide::Expr* { new Halide::Expr{M_PI} } |]
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

cast :: forall to from. (IsHalideType to, IsHalideType from) => Expr from -> Expr to
cast expr = unsafePerformIO $!
  withExpr expr $ \e ->
    with (halideTypeFor (Proxy @to)) $ \t ->
      wrapCxxExpr
        =<< [CU.exp| Halide::Expr* { new Halide::Expr{
              Halide::cast(Halide::Type{*$(halide_type_t* t)}, *$(Halide::Expr* e))} } |]

printed :: IsHalideType a => Expr a -> Expr a
printed = unaryOp $ \e -> [CU.exp| Halide::Expr* { new Halide::Expr{print(*$(Halide::Expr* e))} } |]

equal :: IsHalideType a => Expr a -> Expr a -> Expr Bool
equal a' b' = unsafePerformIO $!
  withExpr2 a' b' $ \a b ->
    wrapCxxExpr =<< [CU.exp| Halide::Expr* { new Halide::Expr{(*$(Halide::Expr* a)) == (*$(Halide::Expr* b))} } |]

bool :: IsHalideType a => Expr Bool -> Expr a -> Expr a -> Expr a
bool condExpr trueExpr falseExpr = unsafePerformIO $!
  withExpr condExpr $ \p ->
    withExpr2 trueExpr falseExpr $ \t f ->
      wrapCxxExpr
        =<< [CU.exp| Halide::Expr* { new Halide::Expr{Halide::select(*$(Halide::Expr* p), *$(Halide::Expr* t), *$(Halide::Expr* f))} } |]

handleHalideExceptions :: HasCallStack => Either C.CppException a -> IO a
handleHalideExceptions (Right x) = pure x
handleHalideExceptions (Left (C.CppStdException _ msg _)) = error $ T.unpack (T.decodeUtf8 msg)
handleHalideExceptions (Left err) = error $ "Halide error: " <> show err

handleHalideExceptionsM :: HasCallStack => IO (Either C.CppException a) -> IO a
handleHalideExceptionsM action = action >>= handleHalideExceptions

evaluate :: forall a. (HasCallStack, IsHalideType a) => Expr a -> IO a
evaluate expr =
  withExpr expr $ \e -> do
    out <- SM.new 1
    handleHalideExceptionsM $
      withHalideBuffer out $ \buffer ->
        let b = castPtr (buffer :: Ptr (HalideBuffer 1 a))
         in [CU.tryBlock| void {
              handle_halide_exceptions([=]() {
                Halide::Func f;
                Halide::Var i;
                f(i) = *$(Halide::Expr* e);
                f.realize(Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)});
              });
            } |]
    SM.read out 0

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

updateFunc ::
  ForeignPtr CxxFunc ->
  [ForeignPtr CxxExpr] ->
  ForeignPtr CxxExpr ->
  IO ()
updateFunc func args expr = do
  withForeignPtr func $ \f ->
    withExprMany args $ \x ->
      withForeignPtr expr $ \y ->
        [CU.block| void {
          $(Halide::Func* f)->operator()(*$(std::vector<Halide::Expr>* x)) = *$(Halide::Expr* y);
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

update :: (ValidIndex i n, KnownNat n, IsHalideType a) => Func n a -> i -> Expr a -> IO ()
update func x y = updateFunc (funcToForeignPtr func) (toExprList x) (exprToForeignPtr y)

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
  Arguments inputs ->
  Arguments outputs ->
  IO ()
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

prepareCxxArguments ::
  forall ts b.
  (All ValidParameter ts, KnownNat (Length ts)) =>
  Arguments ts ->
  (Ptr (CxxVector CxxArgument) -> IO b) ->
  IO b
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

-- type family UnCurried f :: Type where
--   UnCurried (x1 -> x2 -> x3 -> x4 -> x5 -> r) = Arguments 5 '[x1, x2, x3, x4, x5] -> r
--   UnCurried (x1 -> x2 -> x3 -> x4 -> r) = Arguments 4 '[x1, x2, x3, x4] -> r
--   UnCurried (x1 -> x2 -> x3 -> r) = Arguments 3 '[x1, x2, x3] -> r
--   UnCurried (x1 -> x2 -> r) = Arguments 2 '[x1, x2] -> r
--   UnCurried (x1 -> r) = Arguments 1 '[x1] -> r
--   UnCurried r = Arguments 0 '[] -> r

-- class UnCurry f where
--   uncurry' :: f -> UnCurried f
--
-- instance UnCurry (IO b) where
--   uncurry' f Nil = f
--
-- instance UnCurry (x1 -> IO b) where
--   uncurry' f (x1 ::: Nil) = f x1
--
-- instance UnCurry (x1 -> x2 -> IO b) where
--   uncurry' f (x1 ::: x2 ::: Nil) = f x1 x2
--
-- instance UnCurry (x1 -> x2 -> x3 -> IO b) where
--   uncurry' f (x1 ::: x2 ::: x3 ::: Nil) = f x1 x2 x3
--
-- instance UnCurry (x1 -> x2 -> x3 -> x4 -> IO b) where
--   uncurry' f (x1 ::: x2 ::: x3 ::: x4 ::: Nil) = f x1 x2 x3 x4
--
-- instance UnCurry (x1 -> x2 -> x3 -> x4 -> x5 -> IO b) where
--   uncurry' f (x1 ::: x2 ::: x3 ::: x4 ::: x5 ::: Nil) = f x1 x2 x3 x4 x5

mkKernel' ::
  forall ts n a k.
  ( KnownNat n,
    IsHalideType a,
    Length ts ~ k,
    KnownNat k,
    KnownNat (1 + k),
    PrepareParameters ts,
    All ValidParameter ts,
    All ValidArgument (Lowered ts)
  ) =>
  (Arguments ts -> IO (Func n a)) ->
  IO (Arguments (Lowered ts) -> Ptr (HalideBuffer n a) -> IO ())
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

mkKernel ::
  forall f kernel k ts n a r.
  ( FunctionArguments f ~ ts,
    FunctionReturn f ~ r,
    UnCurry f ts r,
    r ~ IO (Func n a),
    KnownNat n,
    IsHalideType a,
    Length ts ~ k,
    KnownNat k,
    KnownNat (1 + k),
    PrepareParameters ts,
    All ValidParameter ts,
    All ValidArgument (Lowered ts),
    Curry (Lowered ts) (Ptr (HalideBuffer n a) -> IO ()) kernel
  ) =>
  f ->
  IO kernel
mkKernel buildFunc = do
  kernel <- mkKernel' (uncurryG @f @ts @r buildFunc)
  pure (curryG kernel)
