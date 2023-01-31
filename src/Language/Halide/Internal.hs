{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import Data.Int
import Data.Kind (Type)
import Data.Proxy
import Data.Ratio (denominator, numerator)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (FunPtr, Ptr)
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
        ("std::vector", [t|CxxVector|]),
        ("halide_buffer_t", [t|HalideBuffer|]),
        ("halide_type_t", [t|HalideType|])
      ]

C.include "<Halide.h>"
C.include "<math.h>"

newtype Expr a = Expr (ForeignPtr CxxExpr)

plusCxxExpr :: Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)
plusCxxExpr a b = [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) + *$(Halide::Expr* b)} } |]

minusCxxExpr :: Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)
minusCxxExpr a b = [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) - *$(Halide::Expr* b)} } |]

timesCxxExpr :: Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)
timesCxxExpr a b = [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) * *$(Halide::Expr* b)} } |]

absCxxExpr :: Ptr CxxExpr -> IO (Ptr CxxExpr)
absCxxExpr a = [CU.exp| Halide::Expr* { new Halide::Expr{Halide::abs(*$(Halide::Expr* a))} } |]

negateCxxExpr :: Ptr CxxExpr -> IO (Ptr CxxExpr)
negateCxxExpr a = [CU.exp| Halide::Expr* { new Halide::Expr{ -(*$(Halide::Expr* a))} } |]

divideCxxExpr :: Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)
divideCxxExpr a b = [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) / *$(Halide::Expr* b)} } |]

wrapCxxExpr :: Ptr CxxExpr -> IO (Expr a)
wrapCxxExpr = fmap Expr . newForeignPtr deleteCxxExpr

deleteCxxExpr :: FunPtr (Ptr CxxExpr -> IO ())
deleteCxxExpr = [C.funPtr| void deleteExpr(Halide::Expr *x) { delete x; } |]

withExpr :: Expr a -> (Ptr CxxExpr -> IO b) -> IO b
withExpr (Expr fp) = withForeignPtr fp

withExpr2 :: Expr a -> Expr a -> (Ptr CxxExpr -> Ptr CxxExpr -> IO b) -> IO b
withExpr2 a b f = withExpr a $ \aPtr -> withExpr b $ \bPtr -> f aPtr bPtr

-- mkExpr :: IsCxxExpr a => a -> Expr
-- mkExpr x = unsafePerformIO $! wrapCxxExpr =<< toCxxExpr x

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
  (+) = binaryOp plusCxxExpr
  (-) :: Expr a -> Expr a -> Expr a
  (-) = binaryOp minusCxxExpr
  (*) :: Expr a -> Expr a -> Expr a
  (*) = binaryOp timesCxxExpr
  abs :: Expr a -> Expr a
  abs = unaryOp absCxxExpr
  negate :: Expr a -> Expr a
  negate = unaryOp negateCxxExpr
  signum :: Expr a -> Expr a
  signum = error "Num instance of (Expr a) does not implement signum"

instance (IsHalideType a, Fractional a) => Fractional (Expr a) where
  (/) :: Expr a -> Expr a -> Expr a
  (/) = binaryOp divideCxxExpr
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

-- createCxxFunc :: Maybe Text -> IO (Ptr CxxFunc)
-- createCxxFunc Nothing = [CU.exp| Halide::Func* { new Halide::Func{} } |]
-- createCxxFunc (Just name) =
--   [CU.exp| Halide::Func* {
--     new Halide::Func{std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}}
--   } |]
--   where
--     s = T.encodeUtf8 name

deleteCxxFunc :: FunPtr (Ptr CxxFunc -> IO ())
deleteCxxFunc = [C.funPtr| void deleteFunc(Halide::Func *x) { delete x; } |]

-- wrapCxxFunc :: Ptr CxxFunc -> IO (Func n a)
-- wrapCxxFunc = fmap Func . newForeignPtr deleteCxxFunc

-- mkFunc :: Maybe Text -> IO Func
-- mkFunc name = wrapCxxFunc =<< createCxxFunc name

withFunc :: Func n a -> (Ptr CxxFunc -> IO b) -> IO b
withFunc (Func fp) = withForeignPtr fp

-- newtype FuncRef = FuncRef (ForeignPtr CxxFuncRef)

-- deleteCxxFuncRef :: FunPtr (Ptr CxxFuncRef -> IO ())
-- deleteCxxFuncRef = [C.funPtr| void deleteFuncRef(Halide::FuncRef *x) { delete x; } |]

-- wrapCxxFuncRef :: Ptr CxxFuncRef -> IO FuncRef
-- wrapCxxFuncRef = fmap FuncRef . newForeignPtr deleteCxxFuncRef

-- withFuncRef :: FuncRef -> (Ptr CxxFuncRef -> IO a) -> IO a
-- withFuncRef (FuncRef fp) = withForeignPtr fp

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

-- class ArgList a where
--   type FuncType a b :: Type
--   type ConstraintType a (c :: Type -> Constraint) :: Constraint

-- define :: Text -> a -> TypedExpr b -> IO (TypedFunc (FuncType a b))
-- (!) :: TypedFunc (FuncType a b) -> a -> TypedExpr b

instance Args (Expr Int32) 1 where
  toExprList :: Expr Int32 -> [ForeignPtr CxxExpr]
  toExprList (Expr a) = [a]

instance Args (Expr Int32, Expr Int32) 2 where
  toExprList :: (Expr Int32, Expr Int32) -> [ForeignPtr CxxExpr]
  toExprList (Expr a, Expr b) = [a, b]

-- instance (IsHalideType a, IsHalideType b, IsHalideType c) => ArgList (Expr a, Expr b, Expr c) where
--   type FuncType (Expr a, Expr b, Expr c) d = a -> b -> c -> d
--   type ConstraintType (Expr a, Expr b, Expr c) k = (k a, k b, k c)
--   toExprList :: (Expr a, Expr b, Expr c) -> [ForeignPtr CxxExpr]
--   toExprList (Expr a, Expr b, Expr c) = [a, b, c]

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

{-
-- define :: Text -> Expr a                   -> Expr b -> IO (Func (a -> b))
-- define :: Text -> (Expr a, Expr b)         -> Expr c -> IO (Func (a -> b -> c))
-- define :: Text -> (Expr a, Expr b, Expr c) -> Expr d -> IO (Func (a -> b -> c -> d))
--
-- (!) :: Func (a -> b)           -> Expr a                   -> Expr b
-- (!) :: Func (a -> b -> c)      -> (Expr a, Expr b)         -> Expr c
-- (!) :: Func (a -> b -> c -> d) -> (Expr a, Expr b, Expr c) -> Expr d
--
-- do
--   (i :: Expr Int32) <- mkVar "i"
--   f <- define "f" i $ i + i + 2
--   printLoopNest f
--
--
defineFunc :: FuncRef -> Expr -> IO ()
defineFunc func expr =
  withFuncRef func $ \f -> withExpr expr $ \e ->
    [CU.block| void {
      *$(Halide::FuncRef* f) = *$(Halide::Expr* e); } |]

data Realization

data Buffer

realizeOnBuffer :: IsHalideBuffer a => Func -> a -> IO ()
realizeOnBuffer func buffer =
  withFunc func $ \f ->
    withHalideBuffer buffer $ \x ->
      [CU.exp| void {
        $(Halide::Func* f)->realize(
          Halide::Pipeline::RealizationArg{$(halide_buffer_t* x)}) } |]

-}

-- createCxxExprVector :: [Expr] -> IO (Ptr CxxExprVector)
-- createCxxExprVector xs = do
--   [CU.block| std::vector<Halide::Expr>* {
--
--
--   } |]

--
-- mkFunc :: Maybe Text -> Func
-- mkFunc name = unsafePerformIO $! wrapCxxFunc =<< createCxxFunc name
--
-- withFunc :: Func -> (Ptr CxxFunc -> IO a) -> IO a
-- withFunc (Func fp) = withForeignPtr fp