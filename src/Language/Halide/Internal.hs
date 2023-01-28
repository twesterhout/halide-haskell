{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Control.Monad (forM_, (<=<), (>=>))
import Control.Monad.ST (RealWorld)
import Data.Bits (Bits (bit), toIntegralSized)
import Data.Constraint
import Data.Int
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import Data.Vector.Storable.Mutable (MVector)
import qualified Data.Vector.Storable.Mutable as SM
import Data.Word
import Foreign.C.Types (CDouble (..), CFloat (..))
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable
import GHC.TypeNats
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Internal.Context
import Language.Halide.Type
import Language.Haskell.TH (Foreign)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (vFmt)

-- import qualified Language.C.Inline.Context as CC
-- import qualified Language.C.Types as CT
-- import qualified Language.C.Inline.Cpp.Exception as C
-- import qualified Language.C.Inline.Cpp.Exceptions as Legacy

data CxxVector a

C.context $
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> C.cppTypePairs
      [ ("Halide::Expr", [t|CxxExpr|]),
        ("Halide::Func", [t|CxxFunc|]),
        ("Halide::FuncRef", [t|CxxFuncRef|]),
        ("std::vector", [t|CxxVector|]),
        ("halide_buffer_t", [t|HalideBuffer|])
      ]

C.include "<Halide.h>"

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
negateCxxExpr a = [CU.exp| Halide::Expr* { new Halide::Expr{-(*$(Halide::Expr* a))} } |]

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

instance (IsHalideType a, Num a) => Num (Expr a) where
  fromInteger :: Integer -> Expr a
  fromInteger x = unsafePerformIO $! wrapCxxExpr =<< toCxxExpr (fromInteger x :: a)
  (+) :: Expr a -> Expr a -> Expr a
  a + b = unsafePerformIO $! withExpr2 a b $ \aPtr bPtr -> plusCxxExpr aPtr bPtr >>= wrapCxxExpr
  (-) :: Expr a -> Expr a -> Expr a
  a - b = unsafePerformIO $! withExpr2 a b $ \aPtr bPtr -> minusCxxExpr aPtr bPtr >>= wrapCxxExpr
  (*) :: Expr a -> Expr a -> Expr a
  a * b = unsafePerformIO $! withExpr2 a b $ \aPtr bPtr -> timesCxxExpr aPtr bPtr >>= wrapCxxExpr
  abs :: Expr a -> Expr a
  abs a = unsafePerformIO $! withExpr a $ absCxxExpr >=> wrapCxxExpr
  negate :: Expr a -> Expr a
  negate a = unsafePerformIO $! withExpr a $ negateCxxExpr >=> wrapCxxExpr

-- defineCastFromTo "int32_t" "float"
instance Castable Int32 Float where
  castImpl :: proxy Int32 -> proxy Float -> Ptr CxxExpr -> IO (Ptr CxxExpr)
  castImpl _ _ x = [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cast<int32_t>(*$(Halide::Expr* x))} } |]

cast :: forall to from. Castable from to => Expr from -> Expr to
cast x = unsafePerformIO $! withExpr x $ castImpl (Proxy @from) (Proxy @to) >=> wrapCxxExpr

newtype Func (n :: Nat) (a :: Type) = Func (ForeignPtr CxxFunc)

createCxxFunc :: Maybe Text -> IO (Ptr CxxFunc)
createCxxFunc Nothing = [CU.exp| Halide::Func* { new Halide::Func{} } |]
createCxxFunc (Just name) =
  [CU.exp| Halide::Func* {
    new Halide::Func{std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}} 
  } |]
  where
    s = T.encodeUtf8 name

deleteCxxFunc :: FunPtr (Ptr CxxFunc -> IO ())
deleteCxxFunc = [C.funPtr| void deleteFunc(Halide::Func *x) { delete x; } |]

wrapCxxFunc :: Ptr CxxFunc -> IO (Func n a)
wrapCxxFunc = fmap Func . newForeignPtr deleteCxxFunc

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