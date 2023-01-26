{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Halide.Internal
  ( Expr (..),
    mkExpr,
    mkVar,
    Func (..),
    mkFunc,
    applyFunc,
    defineFunc,
    printLoopNest,
    realizeOnBuffer,
    -- Typed interface
    TypedExpr (..),
    TypedFunc (..),
    define,
    (!),
    realizeTypedOnBuffer1D,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, (<=<))
import Control.Monad.ST (RealWorld)
import Data.Bits (Bits (bit), toIntegralSized)
import Data.Constraint
import Data.Int
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Vector.Storable.Mutable (MVector)
import Data.Word
import Foreign.C.Types (CDouble (..), CFloat (..))
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr (FunPtr, Ptr)
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Internal.Context
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

newtype Expr = Expr (ForeignPtr CxxExpr)

defineExprConstructors

plusCxxExpr :: Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)
plusCxxExpr a b = [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) + *$(Halide::Expr* b)} } |]

wrapCxxExpr :: Ptr CxxExpr -> IO Expr
wrapCxxExpr = fmap Expr . newForeignPtr deleteCxxExpr

deleteCxxExpr :: FunPtr (Ptr CxxExpr -> IO ())
deleteCxxExpr = [C.funPtr| void deleteExpr(Halide::Expr *x) { delete x; } |]

withExpr :: Expr -> (Ptr CxxExpr -> IO a) -> IO a
withExpr (Expr fp) = withForeignPtr fp

mkExpr :: IsCxxExpr a => a -> Expr
mkExpr x = unsafePerformIO $! wrapCxxExpr =<< toCxxExpr x

mkVar :: Text -> IO Expr
mkVar name =
  wrapCxxExpr
    =<< [CU.exp| Halide::Expr* {
          new Halide::Expr{Halide::Var{
            std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}}} } |]
  where
    s = T.encodeUtf8 name

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger x = case toIntegralSized x of
    Just (n :: Int64) -> mkExpr n
    Nothing -> error $ "integer overflow when converting " <> show x <> " to Expr"
  (+) :: Expr -> Expr -> Expr
  a + b = unsafePerformIO $!
    withExpr a $ \aPtr ->
      withExpr b $
        wrapCxxExpr <=< plusCxxExpr aPtr

newtype Func = Func (ForeignPtr CxxFunc)

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

wrapCxxFunc :: Ptr CxxFunc -> IO Func
wrapCxxFunc = fmap Func . newForeignPtr deleteCxxFunc

mkFunc :: Maybe Text -> IO Func
mkFunc name = wrapCxxFunc =<< createCxxFunc name

withFunc :: Func -> (Ptr CxxFunc -> IO a) -> IO a
withFunc (Func fp) = withForeignPtr fp

newtype FuncRef = FuncRef (ForeignPtr CxxFuncRef)

deleteCxxFuncRef :: FunPtr (Ptr CxxFuncRef -> IO ())
deleteCxxFuncRef = [C.funPtr| void deleteFuncRef(Halide::FuncRef *x) { delete x; } |]

wrapCxxFuncRef :: Ptr CxxFuncRef -> IO FuncRef
wrapCxxFuncRef = fmap FuncRef . newForeignPtr deleteCxxFuncRef

withFuncRef :: FuncRef -> (Ptr CxxFuncRef -> IO a) -> IO a
withFuncRef (FuncRef fp) = withForeignPtr fp

applyFunc :: Func -> [Expr] -> IO FuncRef
applyFunc func args =
  withFunc func $ \f ->
    withExprMany args $ \v ->
      wrapCxxFuncRef
        =<< [CU.exp| Halide::FuncRef* {
              new Halide::FuncRef{(*$(Halide::Func* f))(*$(std::vector<Halide::Expr>* v))} } |]

withExprMany :: [Expr] -> (Ptr (CxxVector CxxExpr) -> IO a) -> IO a
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
        forM_ xs $ \(Expr fp) -> touchForeignPtr fp
  bracket allocate destroy $ \v -> do
    forM_ xs $ \(Expr fp) ->
      let p = unsafeForeignPtrToPtr fp
       in [CU.exp| void { $(std::vector<Halide::Expr>* v)->push_back(*$(Halide::Expr* p)) } |]
    f v

newtype TypedExpr a = TypedExpr Expr

newtype TypedFunc a = TypedFunc Func

instance (Num a, IsCxxExpr a) => Num (TypedExpr a) where
  fromInteger :: Integer -> TypedExpr a
  fromInteger x = TypedExpr (mkExpr (fromInteger x :: a))
  (+) :: TypedExpr a -> TypedExpr a -> TypedExpr a
  (TypedExpr a) + (TypedExpr b) = unsafePerformIO $!
    withExpr a $ \aPtr ->
      withExpr b $
        fmap TypedExpr . wrapCxxExpr <=< plusCxxExpr aPtr

class ArgList a where
  type FuncType a b :: Type
  type ConstraintType a (c :: Type -> Constraint) :: Constraint
  toExprList :: a -> [Expr]

-- define :: Text -> a -> TypedExpr b -> IO (TypedFunc (FuncType a b))
-- (!) :: TypedFunc (FuncType a b) -> a -> TypedExpr b

instance ArgList (TypedExpr a) where
  type FuncType (TypedExpr a) b = a -> b
  type ConstraintType (TypedExpr a) c = c a
  toExprList :: TypedExpr a -> [Expr]
  toExprList (TypedExpr a) = [a]

instance ArgList (TypedExpr a, TypedExpr b) where
  type FuncType (TypedExpr a, TypedExpr b) c = a -> b -> c
  type ConstraintType (TypedExpr a, TypedExpr b) c = (c a, c b)
  toExprList :: (TypedExpr a, TypedExpr b) -> [Expr]
  toExprList (TypedExpr a, TypedExpr b) = [a, b]

define :: ArgList x => Text -> x -> TypedExpr b -> IO (TypedFunc (FuncType x b))
define name x (TypedExpr y) = do
  f <- mkFunc (Just name)
  r <- applyFunc f (toExprList x)
  defineFunc r y
  pure $ TypedFunc f

infix 9 !

(!) :: ArgList x => TypedFunc (FuncType x y) -> x -> TypedExpr y
(!) (TypedFunc func) args =
  unsafePerformIO . fmap TypedExpr $
    withFunc func $ \f ->
      withExprMany (toExprList args) $ \v ->
        wrapCxxExpr
          =<< [CU.exp| Halide::Expr* {
              new Halide::Expr{(*$(Halide::Func* f))(*$(std::vector<Halide::Expr>* v))} } |]

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

printLoopNest :: Func -> IO ()
printLoopNest func = withFunc func $ \f ->
  [C.exp| void { $(Halide::Func* f)->print_loop_nest() } |]

data Realization

data Buffer

realizeOnBuffer :: IsHalideBuffer a => Func -> a -> IO ()
realizeOnBuffer func buffer =
  withFunc func $ \f ->
    withHalideBuffer buffer $ \x ->
      [CU.exp| void {
        $(Halide::Func* f)->realize(
          Halide::Pipeline::RealizationArg{$(halide_buffer_t* x)}) } |]

realizeTypedOnBuffer1D :: (Storable a, IsHalideType a) => TypedFunc (Int32 -> a) -> MVector RealWorld a -> IO ()
realizeTypedOnBuffer1D (TypedFunc func) buffer =
  withFunc func $ \f ->
    withHalideBuffer buffer $ \x ->
      [CU.exp| void {
        $(Halide::Func* f)->realize(
          Halide::Pipeline::RealizationArg{$(halide_buffer_t* x)}) } |]

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