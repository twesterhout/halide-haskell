{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Language.Halide.Expr
-- Description : Scalar expressions
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Expr
  ( Expr (..)
  , Var
  , RVar
  , VarOrRVar
  , ReductionDomain (..)
  , Int32
  , mkExpr
  , mkVar
  , mkRVar
  , mkRDom
  , toRVars
  , cast
  -- , eq
  -- , neq
  -- , lt
  -- , lte
  -- , gt
  -- , gte
  , and
  , or
  , min
  , max
  , div
  , mod
  , clamp
  , ifThenElse
  , select
  , undef
    -- | For debugging, it's often useful to observe the value of an expression when it's evaluated. If you
    -- have a complex expression that does not depend on any buffers or indices, you can 'evaluate' it.
  , evaluate
    -- | However, often an expression is only used within a definition of a pipeline, and it's impossible to
    -- call 'evaluate' on it. In such cases, it can be wrapped with 'printed' to indicate to Halide that the
    -- value of the expression should be dumped to screen when it's computed.
  , printed
  , printedWhen
  , toIntImm
  , setScalarEstimate
  , HalideEq (..)
  , HalideOrd (..)

    -- * Internal
  , exprToForeignPtr
  , cxxConstructExpr
  -- , wrapCxxExpr
  , wrapCxxRVar
  , wrapCxxVarOrRVar
  , wrapCxxParameter
  , asExpr
  , asVar
  , asRVar
  , asVarOrRVar
  , asScalarParam
  , asVectorOf
  , mkScalarParameter
  , withMany
  , binaryOp
  , unaryOp
  , checkType
  , testWriteToStderr
  , Solo (..)
  , IsTuple (..)
  , FromTuple
  , ToTuple
  , UniformTuple
  , UniformTupleProperties
  , proveUniformTupleProperties
  , IndexType
  , IndexTypeProperties
  , proveIndexTypeProperties
  , HasIndexType
  )
where

import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Constraint
import Data.IORef
import Data.Int (Int32)
import Data.Kind
import Data.Proxy
import Data.Ratio (denominator, numerator)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding qualified as T
import Data.Tuple
import Data.Vector.Storable.Mutable qualified as SM
import Foreign.ForeignPtr
import Foreign.Marshal (alloca, allocaArray, peekArray, toBool, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.RedundantConstraints
import Language.Halide.Type
import Language.Halide.Utils
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection
import Unsafe.Coerce
import Prelude hiding (Eq (..), Ord (..), and, div, max, min, mod, or)
import Prelude qualified

-- | A scalar expression in Halide.
--
-- To have a nice experience writing arithmetic expressions in terms of @Expr@s, we want to derive 'Num',
-- 'Floating' etc. instances for @Expr@. Unfortunately, that means that we encode v'Expr', v'Var', v'RVar',
-- and v'ScalarParam' by the same type, and passing an @Expr@ to a function that expects a @Var@ will produce
-- a runtime error.
data Expr a
  = -- | Scalar expression.
    Expr (ForeignPtr CxxExpr)
  | -- | Index variable.
    Var (ForeignPtr CxxVar)
  | -- | Reduction variable.
    RVar (ForeignPtr CxxRVar)
  | -- | Scalar parameter.
    --
    -- The 'IORef' is initialized with 'Nothing' and filled in on the first
    -- call to 'asExpr'.
    ScalarParam (IORef (Maybe (ForeignPtr CxxParameter)))

-- | A single-dimensional span.
data Range = Range {rangeMin :: !(Expr Int32), rangeExtent :: !(Expr Int32)}

-- | Haskell counterpart of @Halide::Range@.
data CxxRange

-- | Haskell counterpart of @Halide::RDom@.
data CxxRDom

importHalide

instanceCxxConstructible "Halide::Expr"
instanceCxxConstructible "Halide::Var"
instanceCxxConstructible "Halide::RVar"
instanceCxxConstructible "Halide::VarOrRVar"
instanceCxxConstructible "Halide::Range"
instanceCxxConstructible "Halide::RDom"

defineIsHalideTypeInstances

instanceHasCxxVector "Halide::Expr"
instanceHasCxxVector "Halide::Var"
instanceHasCxxVector "Halide::RVar"
instanceHasCxxVector "Halide::VarOrRVar"
instanceHasCxxVector "Halide::Range"

-- instanceCxxConstructible "Halide::Var"
-- instanceCxxConstructible "Halide::RVar"
-- instanceCxxConstructible "Halide::VarOrRVar"

instance IsHalideType Bool where
  halideTypeFor _ = HalideType HalideTypeUInt 1 1
  toCxxExpr (fromIntegral . fromEnum -> x) =
    cxxConstruct $ \ptr ->
      [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{cast(Halide::UInt(1), Halide::Expr{$(int x)})} } |]

-- | Type family that maps @'Arguments' ts@ to the corresponding tuple type.
type family ToTuple t = s | s -> t where
  ToTuple '[] = ()
  ToTuple '[Expr a1] = Expr a1
  ToTuple '[a1, a2] = (a1, a2)
  ToTuple '[a1, a2, a3] = (a1, a2, a3)
  ToTuple '[a1, a2, a3, a4] = (a1, a2, a3, a4)
  ToTuple '[a1, a2, a3, a4, a5] = (a1, a2, a3, a4, a5)
  ToTuple '[a1, a2, a3, a4, a5, a6] = (a1, a2, a3, a4, a5, a6)
  ToTuple '[a1, a2, a3, a4, a5, a6, a7] = (a1, a2, a3, a4, a5, a6, a7)
  ToTuple '[a1, a2, a3, a4, a5, a6, a7, a8] = (a1, a2, a3, a4, a5, a6, a7, a8)
  ToTuple '[a1, a2, a3, a4, a5, a6, a7, a8, a9] = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  ToTuple '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10] = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)

-- | Type family that maps tuples to the corresponding @'Arguments' ts@ type. This is essentially the inverse
-- of 'ToTuple'.
type family FromTuple t = s | s -> t where
  FromTuple () = '[]
  FromTuple (Expr a1) = '[Expr a1]
  FromTuple (a1, a2) = '[a1, a2]
  FromTuple (a1, a2, a3) = '[a1, a2, a3]
  FromTuple (a1, a2, a3, a4) = '[a1, a2, a3, a4]
  FromTuple (a1, a2, a3, a4, a5) = '[a1, a2, a3, a4, a5]
  FromTuple (a1, a2, a3, a4, a5, a6) = '[a1, a2, a3, a4, a5, a6]
  FromTuple (a1, a2, a3, a4, a5, a6, a7) = '[a1, a2, a3, a4, a5, a6, a7]
  FromTuple (a1, a2, a3, a4, a5, a6, a7, a8) = '[a1, a2, a3, a4, a5, a6, a7, a8]
  FromTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = '[a1, a2, a3, a4, a5, a6, a7, a8, a9]
  FromTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10]

-- | Specifies that there is an isomorphism between a type @a@ and a tuple @t@.
--
-- We use this class to convert between 'Arguments' and normal tuples.
class (ToTuple a ~ t, FromTuple t ~ a) => IsTuple a t | a -> t, t -> a where
  toTuple :: Arguments a -> t
  fromTuple :: t -> Arguments a

-- | Generates a tuple of @n@ elements of type @t@.
type family UniformTuple (n :: Nat) (t :: Type) = (tuple :: Type) | tuple -> n where
  UniformTuple 0 t = ()
  UniformTuple 1 t = Expr t
  UniformTuple 2 t = (Expr t, Expr t)
  UniformTuple 3 t = (Expr t, Expr t, Expr t)
  UniformTuple 4 t = (Expr t, Expr t, Expr t, Expr t)
  UniformTuple 5 t = (Expr t, Expr t, Expr t, Expr t, Expr t)
  UniformTuple 6 t = (Expr t, Expr t, Expr t, Expr t, Expr t, Expr t)
  UniformTuple 7 t = (Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t)
  UniformTuple 8 t = (Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t)
  UniformTuple 9 t = (Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t)
  UniformTuple 10 t = (Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t, Expr t)

type UniformTupleProperties n a =
  ( IsTuple (FromTuple (UniformTuple n a)) (UniformTuple n a)
  , All ((~) (Expr a)) (FromTuple (UniformTuple n a))
  )

type HasIndexType n = (KnownNat n, n <= 10)

proveUniformTupleProperties :: forall n a. (KnownNat n, n <= 10) :- UniformTupleProperties n a
proveUniformTupleProperties = Sub $
  case fromIntegral (natVal (Proxy @n)) :: Int of
    0 -> unsafeCoerce $ Dict @(UniformTupleProperties 0 a)
    1 -> unsafeCoerce $ Dict @(UniformTupleProperties 1 a)
    2 -> unsafeCoerce $ Dict @(UniformTupleProperties 2 a)
    3 -> unsafeCoerce $ Dict @(UniformTupleProperties 3 a)
    4 -> unsafeCoerce $ Dict @(UniformTupleProperties 4 a)
    5 -> unsafeCoerce $ Dict @(UniformTupleProperties 5 a)
    6 -> unsafeCoerce $ Dict @(UniformTupleProperties 6 a)
    7 -> unsafeCoerce $ Dict @(UniformTupleProperties 7 a)
    8 -> unsafeCoerce $ Dict @(UniformTupleProperties 8 a)
    9 -> unsafeCoerce $ Dict @(UniformTupleProperties 9 a)
    10 -> unsafeCoerce $ Dict @(UniformTupleProperties 10 a)
    _ -> error "cannot happen"
{-# NOINLINE proveUniformTupleProperties #-}

class CanPeekUniformTuple n where
  peekUniformTupleImpl :: (CxxConstructible b) => (Ptr b -> IO (Expr a)) -> Ptr b -> IO (UniformTuple n a)

instance CanPeekUniformTuple 0 where
  peekUniformTupleImpl _ _ = pure ()

instance CanPeekUniformTuple 1 where
  peekUniformTupleImpl f = f

instance CanPeekUniformTuple 2 where
  peekUniformTupleImpl f (p :: Ptr b) = (,) <$> f p <*> f (p `plusPtr` cxxSizeOf @b)

instance CanPeekUniformTuple 3 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,) <$> f p <*> f (p `plusPtr` cxxSizeOf @b) <*> f (p `plusPtr` (2 * cxxSizeOf @b))

instance CanPeekUniformTuple 4 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,,)
      <$> f (p `plusPtr` (0 * cxxSizeOf @b))
      <*> f (p `plusPtr` (1 * cxxSizeOf @b))
      <*> f (p `plusPtr` (2 * cxxSizeOf @b))
      <*> f (p `plusPtr` (3 * cxxSizeOf @b))

instance CanPeekUniformTuple 5 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,,,)
      <$> f (p `plusPtr` (0 * cxxSizeOf @b))
      <*> f (p `plusPtr` (1 * cxxSizeOf @b))
      <*> f (p `plusPtr` (2 * cxxSizeOf @b))
      <*> f (p `plusPtr` (3 * cxxSizeOf @b))
      <*> f (p `plusPtr` (4 * cxxSizeOf @b))

instance CanPeekUniformTuple 6 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,,,,)
      <$> f (p `plusPtr` (0 * cxxSizeOf @b))
      <*> f (p `plusPtr` (1 * cxxSizeOf @b))
      <*> f (p `plusPtr` (2 * cxxSizeOf @b))
      <*> f (p `plusPtr` (3 * cxxSizeOf @b))
      <*> f (p `plusPtr` (4 * cxxSizeOf @b))
      <*> f (p `plusPtr` (5 * cxxSizeOf @b))

instance CanPeekUniformTuple 7 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,,,,,)
      <$> f (p `plusPtr` (0 * cxxSizeOf @b))
      <*> f (p `plusPtr` (1 * cxxSizeOf @b))
      <*> f (p `plusPtr` (2 * cxxSizeOf @b))
      <*> f (p `plusPtr` (3 * cxxSizeOf @b))
      <*> f (p `plusPtr` (4 * cxxSizeOf @b))
      <*> f (p `plusPtr` (5 * cxxSizeOf @b))
      <*> f (p `plusPtr` (6 * cxxSizeOf @b))

instance CanPeekUniformTuple 8 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,,,,,,)
      <$> f (p `plusPtr` (0 * cxxSizeOf @b))
      <*> f (p `plusPtr` (1 * cxxSizeOf @b))
      <*> f (p `plusPtr` (2 * cxxSizeOf @b))
      <*> f (p `plusPtr` (3 * cxxSizeOf @b))
      <*> f (p `plusPtr` (4 * cxxSizeOf @b))
      <*> f (p `plusPtr` (5 * cxxSizeOf @b))
      <*> f (p `plusPtr` (6 * cxxSizeOf @b))
      <*> f (p `plusPtr` (7 * cxxSizeOf @b))

instance CanPeekUniformTuple 9 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,,,,,,,)
      <$> f (p `plusPtr` (0 * cxxSizeOf @b))
      <*> f (p `plusPtr` (1 * cxxSizeOf @b))
      <*> f (p `plusPtr` (2 * cxxSizeOf @b))
      <*> f (p `plusPtr` (3 * cxxSizeOf @b))
      <*> f (p `plusPtr` (4 * cxxSizeOf @b))
      <*> f (p `plusPtr` (5 * cxxSizeOf @b))
      <*> f (p `plusPtr` (6 * cxxSizeOf @b))
      <*> f (p `plusPtr` (7 * cxxSizeOf @b))
      <*> f (p `plusPtr` (8 * cxxSizeOf @b))

instance CanPeekUniformTuple 10 where
  peekUniformTupleImpl f (p :: Ptr b) =
    (,,,,,,,,,)
      <$> f (p `plusPtr` (0 * cxxSizeOf @b))
      <*> f (p `plusPtr` (1 * cxxSizeOf @b))
      <*> f (p `plusPtr` (2 * cxxSizeOf @b))
      <*> f (p `plusPtr` (3 * cxxSizeOf @b))
      <*> f (p `plusPtr` (4 * cxxSizeOf @b))
      <*> f (p `plusPtr` (5 * cxxSizeOf @b))
      <*> f (p `plusPtr` (6 * cxxSizeOf @b))
      <*> f (p `plusPtr` (7 * cxxSizeOf @b))
      <*> f (p `plusPtr` (8 * cxxSizeOf @b))
      <*> f (p `plusPtr` (9 * cxxSizeOf @b))

peekUniformTuple
  :: forall n b a
   . (KnownNat n, n <= 10, CxxConstructible b)
  => (Ptr b -> IO (Expr a))
  -> Ptr b
  -> IO (UniformTuple n a)
peekUniformTuple f p
  | Just Refl <- sameNat (Proxy @0) (Proxy @n) = peekUniformTupleImpl @0 f p
  | Just Refl <- sameNat (Proxy @1) (Proxy @n) = peekUniformTupleImpl @1 f p
  | Just Refl <- sameNat (Proxy @2) (Proxy @n) = peekUniformTupleImpl @2 f p
  | Just Refl <- sameNat (Proxy @3) (Proxy @n) = peekUniformTupleImpl @3 f p
  | Just Refl <- sameNat (Proxy @4) (Proxy @n) = peekUniformTupleImpl @4 f p
  | Just Refl <- sameNat (Proxy @5) (Proxy @n) = peekUniformTupleImpl @5 f p
  | Just Refl <- sameNat (Proxy @6) (Proxy @n) = peekUniformTupleImpl @6 f p
  | Just Refl <- sameNat (Proxy @7) (Proxy @n) = peekUniformTupleImpl @7 f p
  | Just Refl <- sameNat (Proxy @8) (Proxy @n) = peekUniformTupleImpl @8 f p
  | Just Refl <- sameNat (Proxy @9) (Proxy @n) = peekUniformTupleImpl @9 f p
  | Just Refl <- sameNat (Proxy @10) (Proxy @n) = peekUniformTupleImpl @10 f p
  | otherwise = error "cannot happen"
  where
    _ = keepRedundantConstraint @(n <= 10)

type IndexType n = UniformTuple n Int32

type IndexTypeProperties n =
  ( IsTuple (FromTuple (IndexType n)) (IndexType n)
  , All ((~) (Expr Int32)) (FromTuple (IndexType n))
  )

proveIndexTypeProperties :: forall n. (KnownNat n, n <= 10) :- IndexTypeProperties n
proveIndexTypeProperties = Sub $
  case proveUniformTupleProperties @n @Int32 of
    Sub Dict -> Dict

instance IsTuple '[] () where
  toTuple Nil = ()
  fromTuple () = Nil

instance IsTuple '[Expr a1] (Expr a1) where
  toTuple (a1 ::: Nil) = a1
  {-# INLINE toTuple #-}
  fromTuple a1 = a1 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2] (a1, a2) where
  toTuple (a1 ::: a2 ::: Nil) = (a1, a2)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2) = a1 ::: a2 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3] (a1, a2, a3) where
  toTuple (a1 ::: a2 ::: a3 ::: Nil) = (a1, a2, a3)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3) = a1 ::: a2 ::: a3 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3, a4] (a1, a2, a3, a4) where
  toTuple (a1 ::: a2 ::: a3 ::: a4 ::: Nil) = (a1, a2, a3, a4)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3, a4) = a1 ::: a2 ::: a3 ::: a4 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3, a4, a5] (a1, a2, a3, a4, a5) where
  toTuple (a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: Nil) = (a1, a2, a3, a4, a5)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3, a4, a5) = a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3, a4, a5, a6] (a1, a2, a3, a4, a5, a6) where
  toTuple (a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: Nil) = (a1, a2, a3, a4, a5, a6)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3, a4, a5, a6) = a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3, a4, a5, a6, a7] (a1, a2, a3, a4, a5, a6, a7) where
  toTuple (a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: Nil) = (a1, a2, a3, a4, a5, a6, a7)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3, a4, a5, a6, a7) = a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3, a4, a5, a6, a7, a8] (a1, a2, a3, a4, a5, a6, a7, a8) where
  toTuple (a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: a8 ::: Nil) = (a1, a2, a3, a4, a5, a6, a7, a8)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3, a4, a5, a6, a7, a8) = a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: a8 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3, a4, a5, a6, a7, a8, a9] (a1, a2, a3, a4, a5, a6, a7, a8, a9) where
  toTuple (a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: a8 ::: a9 ::: Nil) = (a1, a2, a3, a4, a5, a6, a7, a8, a9)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9) = a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: a8 ::: a9 ::: Nil
  {-# INLINE fromTuple #-}

instance IsTuple '[a1, a2, a3, a4, a5, a6, a7, a8, a9, a10] (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) where
  toTuple (a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: a8 ::: a9 ::: a10 ::: Nil) = (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
  {-# INLINE toTuple #-}
  fromTuple (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10) = a1 ::: a2 ::: a3 ::: a4 ::: a5 ::: a6 ::: a7 ::: a8 ::: a9 ::: a10 ::: Nil
  {-# INLINE fromTuple #-}

-- instance IsTuple (Arguments '[Expr a]) (Expr a) where
--   toTuple (x ::: Nil) = x
--   fromTuple () = Nil

-- | A v'Var'.
type Var = Expr Int32

-- | An v'RVar'.
type RVar = Expr Int32

-- | Either v'Var' or v'RVar'.
type VarOrRVar = Expr Int32

-- | A multi-dimensional box -- cartesian product of the 'Range's.
newtype Region = Region [Range]
  deriving stock (Show)

-- | An @n@-dimensional reduction domain.
newtype ReductionDomain (n :: Nat) = ReductionDomain (ForeignPtr CxxRDom)

-- | Create a scalar expression from a Haskell value.
mkExpr :: (IsHalideType a) => a -> Expr a
mkExpr x = unsafePerformIO $! Expr <$> toCxxExpr x

-- | Create a named index variable.
mkVar :: Text -> IO (Expr Int32)
mkVar (T.encodeUtf8 -> s) = fmap Var . cxxConstruct $ \ptr ->
  [CU.exp| void {
    new ($(Halide::Var* ptr)) Halide::Var{std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}} } |]

-- withRange :: Range -> (Ptr CxxRange -> IO a) -> IO a
-- withRange r action =
--   asExpr r.rangeMin $ \minPtr ->
--     asExpr r.rangeExtent $ \extentPtr -> do
--       fp <-
--         cxxConstruct $ \destPtr ->
--           [CU.exp| void { new ($(Halide::Range* destPtr)) Halide::Range{
--             *$(const Halide::Expr* minPtr), *$(const Halide::Expr* extentPtr)} } |]
--       withForeignPtr fp action

-- | Create a reduction domain. Use 'asRVar' to cast it into an index.
--
-- For more information about reduction variables, see [@Halide::RDom@](https://halide-lang.org/docs/class_halide_1_1_r_dom.html).
mkRDom
  :: forall n
   . (HasIndexType n)
  => Text
  -- ^ name
  -> IndexType n
  -- ^ mins
  -> IndexType n
  -- ^ extents
  -> IO (ReductionDomain n)
  -- ^ reduction variables
mkRDom (T.encodeUtf8 -> name) mins extents = fmap ReductionDomain $
  case proveIndexTypeProperties @n of
    Sub Dict ->
      asVectorOf @((~) (Expr Int32)) asExpr (fromTuple mins) $ \mins' ->
        asVectorOf @((~) (Expr Int32)) asExpr (fromTuple extents) $ \extents' ->
          cxxConstruct $ \destPtr ->
            [CU.block| void {
              auto const& mins = *$(const std::vector<Halide::Expr>* mins');
              auto const& extents = *$(const std::vector<Halide::Expr>* extents');
              std::vector<Halide::Range> region;
              for (auto i = size_t{0}; i < mins.size(); ++i) {
                region.emplace_back(mins.at(i), extents.at(i));
              }
              new ($(Halide::RDom* destPtr)) Halide::RDom{
                region, std::string{$bs-ptr:name, static_cast<size_t>($bs-len:name)}};
            } |]

withCxxRDom :: ReductionDomain n -> (Ptr CxxRDom -> IO a) -> IO a
withCxxRDom (ReductionDomain fp) = withForeignPtr fp

-- | Cast a reduction domain into a multi-dimensional index that can be used to
-- perform multi-dimensional reductions.
toRVars :: forall n. (HasIndexType n) => ReductionDomain n -> IO (IndexType n)
toRVars rdom = do
  let allocate =
        withCxxRDom rdom $ \rdom' ->
          [CU.block| std::vector<Halide::RVar>* {
            auto const& rdom = *$(const Halide::RDom* rdom');
            std::vector<Halide::RVar> rvars;
            rvars.reserve(rdom.dimensions());
            for (auto i = 0; i < rdom.dimensions(); ++i) {
              rvars.push_back(rdom[i]);
            }
            return new std::vector<Halide::RVar>{std::move(rvars)};
          } |]
  bracket allocate deleteCxxVector $ \v -> do
    n <- cxxVectorSize v
    unless (n == fromIntegral (natVal (Proxy @n))) $ error "wrong vector length"
    ptr <- cxxVectorData v
    peekUniformTuple @n peekRVar ptr

setScalarEstimate
  :: (IsHalideType a)
  => a
  -- ^ Estimate
  -> Expr a
  -- ^ Parameter
  -> IO ()
setScalarEstimate estimate param =
  asScalarParam param $ \param' ->
    asExpr (mkExpr estimate) $ \estimate' ->
      [CU.exp| void {
        $(Halide::Internal::Parameter* param')->set_estimate(*$(const Halide::Expr* estimate'))
      } |]

--   withMany withRange ranges $ \regionPtr -> do
--     forM [0 .. n - 1] $ \i ->
--       cxxConstructExpr $ \destPtr ->
--         let srcPtr = ptr ``
-- cxxVectorToList
--   :: (CxxConstructible a, HasCxxVector a)
--   => (Ptr a -> Ptr a -> IO ())
--   -- ^ Copy constructor: dest src
--   -> Ptr (CxxVector a)
--   -> IO [a]
-- cxxVectorToList construct v = do
--   n <- cxxVectorSize v
--   ptr <- cxxVectorData v
--   forM [0 .. n - 1] $ \i ->
--     cxxConstruct $
--       construct undefined

--     forM [0 .. k] $ \i ->
--       pure ()
--     wrapCxxRVar
--       =<< [CU.exp| Halide::RVar* {
--             new Halide::RVar{static_cast<Halide::RVar>(Halide::RDom{
--               *$(const Halide::Expr* min'),
--               *$(const Halide::Expr* extent'),
--               std::string{$bs-ptr:name, static_cast<size_t>($bs-len:name)}
--               })}
--           } |]
--
--   undefined

-- | Create a named reduction variable.
--
-- For more information about reduction variables, see [@Halide::RDom@](https://halide-lang.org/docs/class_halide_1_1_r_dom.html).
mkRVar
  :: Text
  -- ^ name
  -> Expr Int32
  -- ^ min index
  -> Expr Int32
  -- ^ extent
  -> IO (Expr Int32)
mkRVar name start extent = do
  rdom <- mkRDom name start extent
  toRVars rdom

-- asExpr start $ \min' ->
--   asExpr extent $ \extent' ->
--     wrapCxxRVar
--       =<< [CU.exp| Halide::RVar* {
--             new Halide::RVar{static_cast<Halide::RVar>(Halide::RDom{
--               *$(const Halide::Expr* min'),
--               *$(const Halide::Expr* extent'),
--               std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}
--               })}
--           } |]
-- where
--   s = T.encodeUtf8 name

-- | Return an undef value of the given type.
--
-- For more information, see [@Halide::undef@](https://halide-lang.org/docs/namespace_halide.html#a9389bcacbed602df70eae94826312e03).
undef :: forall a. (IsHalideType a) => Expr a
undef = unsafePerformIO $
  with (halideTypeFor (Proxy @a)) $ \tp ->
    cxxConstructExpr $ \ptr ->
      [CU.exp| void {
        new ($(Halide::Expr* ptr))
          Halide::Expr{Halide::undef(Halide::Type{*$(const halide_type_t* tp)})} } |]
{-# NOINLINE undef #-}

-- | Cast a scalar expression to a different type.
--
-- Use TypeApplications with this function, e.g. @cast \@Float x@.
cast :: forall to from. (IsHalideType to, IsHalideType from) => Expr from -> Expr to
cast expr = unsafePerformIO $
  asExpr expr $ \e ->
    with (halideTypeFor (Proxy @to)) $ \t ->
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          Halide::cast(Halide::Type{*$(halide_type_t* t)}, *$(Halide::Expr* e))} } |]

-- | Print all expressions to stdout when the result is evaluates. The first expression is returned.
--
-- This is useful for debugging Halide pipelines.
--
-- This function is similar to 'Text.Printf.printf' in that it accepts a variable number of arguments,
-- i.e the following is valid:
--
-- @
-- let x :: Expr Float
--     x = 1
--  in printed (sin x) ("<- sin(" :: Text) x (")" :: Text)
-- @
--
-- @:: Text@ specifications are only needed if you have the @OverloadedStrings@ extension enabled.
--
-- Arguments to @printed@ can be @'Expr' a@, 'String', or 'Text'.
printed :: forall a t. (IsHalideType a, PrintedType t (Expr a)) => Expr a -> t
printed = printedWhen @a @t (mkExpr True)

printedWhen :: forall a t. (IsHalideType a, PrintedType t (Expr a)) => Expr Bool -> Expr a -> t
printedWhen cond x = unsafePerformIO $ do
  v <- newCxxVector Nothing
  appendToPrintArgs v x
  pure $ printedWhenImpl @t @(Expr a) cond v

class PrintedType t r where
  printedWhenImpl :: Expr Bool -> Ptr (CxxVector CxxExpr) -> t

instance (IsHalideType a, r ~ Expr a) => PrintedType (Expr a) r where
  printedWhenImpl cond v = unsafePerformIO $
    asExpr cond $ \cond' ->
      cxxConstructExpr $ \expr ->
        [CU.exp| void { new ($(Halide::Expr* expr)) Halide::Expr{Halide::print_when(
          *$(const Halide::Expr* cond'), *$(const std::vector<Halide::Expr>* v))} } |]
  {-# NOINLINE printedWhenImpl #-}

instance (PrintedArg a, PrintedType t r) => PrintedType (a -> t) r where
  printedWhenImpl cond v x = unsafePerformIO $ do
    appendToPrintArgs v x
    pure (printedWhenImpl @t @r cond v)
  {-# NOINLINE printedWhenImpl #-}

class PrintedArg a where
  appendToPrintArgs :: Ptr (CxxVector CxxExpr) -> a -> IO ()

instance (IsHalideType a) => PrintedArg (Expr a) where
  appendToPrintArgs v expr =
    asExpr expr $ \expr' ->
      [CU.exp| void { $(std::vector<Halide::Expr>* v)->push_back(*$(const Halide::Expr* expr')) } |]

instance PrintedArg Text where
  appendToPrintArgs v (T.encodeUtf8 -> msg) =
    [CU.exp| void { $(std::vector<Halide::Expr>* v)->emplace_back(
      std::string{$bs-ptr:msg, static_cast<size_t>($bs-len:msg)}) } |]

instance PrintedArg String where
  appendToPrintArgs v (pack -> msg) = appendToPrintArgs v msg

infix 4 `eq`, `neq`, `lt`, `lte`, `gt`, `gte`
infix 4 ==, /=
infix 4 <, >, <=, >=

class HalideEq a r where
  (==) :: a -> a -> r
  (/=) :: a -> a -> r

class (HalideEq a r) => HalideOrd a r where
  (<) :: a -> a -> r
  (<=) :: a -> a -> r
  (>) :: a -> a -> r
  (>=) :: a -> a -> r

instance (Prelude.Eq a, r ~ Bool) => HalideEq a r where
  (==) = (Prelude.==)
  (/=) = (Prelude./=)

instance {-# OVERLAPPING #-} (IsHalideType a, r ~ Expr Bool) => HalideEq (Expr a) r where
  (==) = eq
  (/=) = neq

instance (Prelude.Ord a, r ~ Bool) => HalideOrd a r where
  (<) = (Prelude.<)
  (<=) = (Prelude.<=)
  (>) = (Prelude.>)
  (>=) = (Prelude.>=)

instance {-# OVERLAPPING #-} (IsHalideType a, r ~ Expr Bool) => HalideOrd (Expr a) r where
  (<) = lt
  (<=) = lte
  (>) = gt
  (>=) = gte

-- | '==' but lifted to return an 'Expr'.
eq :: (IsHalideType a) => Expr a -> Expr a -> Expr Bool
eq = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) == (*$(Halide::Expr* b))} } |]

-- | '/=' but lifted to return an 'Expr'.
neq :: (IsHalideType a) => Expr a -> Expr a -> Expr Bool
neq = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) != (*$(Halide::Expr* b))} } |]

-- | '<' but lifted to return an 'Expr'.
lt :: (IsHalideType a) => Expr a -> Expr a -> Expr Bool
lt = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) < (*$(Halide::Expr* b))} } |]

-- | '<=' but lifted to return an 'Expr'.
lte :: (IsHalideType a) => Expr a -> Expr a -> Expr Bool
lte = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) <= (*$(Halide::Expr* b))} } |]

-- | '>' but lifted to return an 'Expr'.
gt :: (IsHalideType a) => Expr a -> Expr a -> Expr Bool
gt = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) > (*$(Halide::Expr* b))} } |]

-- | '>=' but lifted to return an 'Expr'.
gte :: (IsHalideType a) => Expr a -> Expr a -> Expr Bool
gte = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) >= (*$(Halide::Expr* b))} } |]

infixr 3 `and`
infixr 2 `or`

-- | '&&' but lifted to return an 'Expr'.
and :: Expr Bool -> Expr Bool -> Expr Bool
and = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) && (*$(Halide::Expr* b))} } |]

-- | '||' but lifted to return an 'Expr'.
or :: Expr Bool -> Expr Bool -> Expr Bool
or = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    (*$(Halide::Expr* a)) || (*$(Halide::Expr* b))} } |]

-- | 'Prelude.min' but lifted to return an 'Expr'.
min :: (IsHalideType a) => Expr a -> Expr a -> Expr a
min = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    Halide::min(*$(Halide::Expr* a), *$(Halide::Expr* b))} } |]

-- | 'Prelude.max' but lifted to return an 'Expr'.
max :: (IsHalideType a) => Expr a -> Expr a -> Expr a
max = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    Halide::max(*$(Halide::Expr* a), *$(Halide::Expr* b))} } |]

-- | Divide two integers, rounding towards zero.
div :: forall a. (IsHalideType a, Integral a) => Expr a -> Expr a -> Expr a
div = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    Halide::div_round_to_zero(*$(Halide::Expr* a), *$(Halide::Expr* b))} } |]
  where
    _ = keepRedundantConstraint (Proxy @(Integral a))

-- | Compute the remainder of dividing two integers, when division is rounding toward zero.
mod :: forall a. (IsHalideType a, Integral a) => Expr a -> Expr a -> Expr a
mod = binaryOp $ \a b ptr ->
  [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
    Halide::mod_round_to_zero(*$(Halide::Expr* a), *$(Halide::Expr* b))} } |]
  where
    _ = keepRedundantConstraint (Proxy @(Integral a))

-- | Clamps an expression to lie within the given bounds.
clamp :: (IsHalideType a) => Expr a -> Expr a -> Expr a -> Expr a
clamp lower upper expr = unsafePerformIO $
  asExpr lower $ \l ->
    asExpr upper $ \u ->
      asExpr expr $ \x ->
        cxxConstructExpr $ \ptr ->
          [CU.exp| void {
            new ($(Halide::Expr* ptr)) Halide::Expr{
              Halide::clamp(*$(Halide::Expr const* x),
                *$(Halide::Expr const* l), *$(Halide::Expr const* u))} } |]

-- | 'ifThenElse cond a b' is the analogue of @if cond then a else b@, but
-- lifted to work with 'Expr' types.
--
-- See also the [RebindableSyntax](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rebindable_syntax.html#extension-RebindableSyntax) extension.
ifThenElse :: (IsHalideType a) => Expr Bool -> Expr a -> Expr a -> Expr a
ifThenElse condExpr trueExpr falseExpr = unsafePerformIO $
  asExpr condExpr $ \p ->
    asExpr trueExpr $ \t ->
      asExpr falseExpr $ \f ->
        cxxConstructExpr $ \ptr ->
          [CU.exp| void {
            new ($(Halide::Expr* ptr)) Halide::Expr{
              Halide::select(*$(Halide::Expr* p),
                *$(Halide::Expr* t), *$(Halide::Expr* f))} } |]

select :: (IsHalideType a) => [(Expr Bool, Expr a)] -> Expr a -> Expr a
select branches final = go branches
  where
    go [] = final
    go ((cond, value) : rest) = ifThenElse cond value (go rest)

-- | Evaluate a scalar expression.
--
-- It should contain no parameters. If it does contain parameters, an exception will be thrown.
evaluate :: forall a. (IsHalideType a) => Expr a -> IO a
evaluate expr =
  asExpr expr $ \e -> do
    out <- SM.new 1
    withHalideBuffer out $ \buffer -> do
      let b = castPtr (buffer :: Ptr (HalideBuffer 1 a))
      [C.throwBlock| void {
        handle_halide_exceptions([=]() {
          Halide::Func f;
          Halide::Var i;
          f(i) = *$(Halide::Expr* e);
          f.realize(Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)});
        });
      } |]
    SM.read out 0

-- | Convert expression to integer immediate.
--
-- Tries to extract the value of an expression if it is a compile-time constant. If the expression
-- isn't known at compile-time of the Halide pipeline, returns 'Nothing'.
toIntImm :: (IsHalideType a) => Expr a -> Maybe Int
toIntImm expr = unsafePerformIO $
  asExpr expr $ \expr' -> do
    intPtr <-
      [CU.block| const int64_t* {
        auto expr = *$(const Halide::Expr* expr');
        Halide::Internal::IntImm const* node = expr.as<Halide::Internal::IntImm>();
        if (node == nullptr) return nullptr;
        return &node->value;
      } |]
    if intPtr == nullPtr
      then pure Nothing
      else Just . fromIntegral <$> peek intPtr

instance (IsHalideType a) => Show (Expr a) where
  show (Expr expr) =
    unpack . unsafePerformIO $! do
      withForeignPtr expr $ \x ->
        peekAndDeleteCxxString
          =<< [CU.exp| std::string* { to_string_via_iostream(*$(const Halide::Expr* x)) } |]
  show (Var var) = unpack . unsafePerformIO $ do
    withForeignPtr var $ \x ->
      peekAndDeleteCxxString
        =<< [CU.block| std::string* {
              return to_string_via_iostream(*$(const Halide::Var* x));
            } |]
  show (RVar rvar) = unpack . unsafePerformIO $ do
    withForeignPtr rvar $ \x ->
      peekAndDeleteCxxString
        =<< [CU.exp| std::string* { to_string_via_iostream(*$(const Halide::RVar* x)) } |]
  show (ScalarParam r) = unpack . unsafePerformIO $ do
    maybeParam <- readIORef r
    case maybeParam of
      Just fp ->
        withForeignPtr fp $ \x ->
          peekAndDeleteCxxString
            =<< [CU.exp| std::string* {
                  new std::string{$(const Halide::Internal::Parameter* x)->name()} } |]
      Nothing -> pure "ScalarParam"

instance (IsHalideType a, Num a) => Num (Expr a) where
  fromInteger :: Integer -> Expr a
  fromInteger x = mkExpr (fromInteger x :: a)
  (+) :: Expr a -> Expr a -> Expr a
  (+) = binaryOp $ \a b ptr ->
    [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{*$(Halide::Expr* a) + *$(Halide::Expr* b)} } |]
  (-) :: Expr a -> Expr a -> Expr a
  (-) = binaryOp $ \a b ptr ->
    [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{*$(Halide::Expr* a) - *$(Halide::Expr* b)} } |]
  (*) :: Expr a -> Expr a -> Expr a
  (*) = binaryOp $ \a b ptr ->
    [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{*$(Halide::Expr* a) * *$(Halide::Expr* b)} } |]

  abs :: Expr a -> Expr a
  abs = unaryOp $ \a ptr ->
    -- If the type is unsigned, then abs does nothing Also note that for signed
    -- integers, in Halide abs returns the unsigned version, so we manually
    -- cast it back.
    [CU.block| void {
      if ($(Halide::Expr* a)->type().is_uint()) {
        new ($(Halide::Expr* ptr)) Halide::Expr{*$(Halide::Expr* a)};
      }
      else {
        new ($(Halide::Expr* ptr)) Halide::Expr{
          Halide::cast($(Halide::Expr* a)->type(), Halide::abs(*$(Halide::Expr* a)))};
      }
    } |]
  negate :: Expr a -> Expr a
  negate = unaryOp $ \a ptr ->
    [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{ -(*$(Halide::Expr* a))} } |]
  signum :: Expr a -> Expr a
  signum = error "Num instance of (Expr a) does not implement signum"

instance (IsHalideType a, Fractional a) => Fractional (Expr a) where
  (/) :: Expr a -> Expr a -> Expr a
  (/) = binaryOp $ \a b ptr ->
    [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{*$(Halide::Expr* a) / *$(Halide::Expr* b)} } |]
  fromRational :: Rational -> Expr a
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance (IsHalideType a, Floating a) => Floating (Expr a) where
  pi :: Expr a
  pi = cast @a @Double $! mkExpr (pi :: Double)
  exp :: Expr a -> Expr a
  exp = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::exp(*$(Halide::Expr* a))} } |]
  log :: Expr a -> Expr a
  log = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::log(*$(Halide::Expr* a))} } |]
  sqrt :: Expr a -> Expr a
  sqrt = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::sqrt(*$(Halide::Expr* a))} } |]
  (**) :: Expr a -> Expr a -> Expr a
  (**) = binaryOp $ \a b ptr ->
    [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::pow(*$(Halide::Expr* a), *$(Halide::Expr* b))} } |]
  sin :: Expr a -> Expr a
  sin = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::sin(*$(Halide::Expr* a))} } |]
  cos :: Expr a -> Expr a
  cos = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::cos(*$(Halide::Expr* a))} } |]
  tan :: Expr a -> Expr a
  tan = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::tan(*$(Halide::Expr* a))} } |]
  asin :: Expr a -> Expr a
  asin = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::asin(*$(Halide::Expr* a))} } |]
  acos :: Expr a -> Expr a
  acos = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::acos(*$(Halide::Expr* a))} } |]
  atan :: Expr a -> Expr a
  atan = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::atan(*$(Halide::Expr* a))} } |]
  sinh :: Expr a -> Expr a
  sinh = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::sinh(*$(Halide::Expr* a))} } |]
  cosh :: Expr a -> Expr a
  cosh = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::cosh(*$(Halide::Expr* a))} } |]
  tanh :: Expr a -> Expr a
  tanh = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::tanh(*$(Halide::Expr* a))} } |]
  asinh :: Expr a -> Expr a
  asinh = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::asinh(*$(Halide::Expr* a))} } |]
  acosh :: Expr a -> Expr a
  acosh = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::acosh(*$(Halide::Expr* a))} } |]
  atanh :: Expr a -> Expr a
  atanh = unaryOp $ \a ptr -> [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{Halide::atanh(*$(Halide::Expr* a))} } |]

deriving stock instance Show Range

-- | Wrap a raw @Halide::Expr@ pointer in a Haskell value.
--
-- __Note:__ This function checks the runtime type of the expression.
-- wrapCxxExpr :: forall a. (HasCallStack, IsHalideType a) => Ptr CxxExpr -> IO (Expr a)
-- wrapCxxExpr p = do
--   checkType @a p
--   Expr <$> newForeignPtr deleter p
--   where
--     deleter = [C.funPtr| void deleteExpr(Halide::Expr *p) { delete p; } |]
cxxConstructExpr :: forall a. (HasCallStack, IsHalideType a) => (Ptr CxxExpr -> IO ()) -> IO (Expr a)
cxxConstructExpr construct = do
  fp <- cxxConstruct construct
  withForeignPtr fp (checkType @a)
  pure (Expr fp)

-- | Wrap a raw @Halide::RVar@ pointer in a Haskell value.
--
-- __Note:__ v'RVar' objects correspond to expressions of type 'Int32'.
wrapCxxRVar :: Ptr CxxRVar -> IO (Expr Int32)
wrapCxxRVar = fmap RVar . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteExpr(Halide::RVar *p) { delete p; } |]

wrapCxxVarOrRVar :: Ptr CxxVarOrRVar -> IO (Expr Int32)
wrapCxxVarOrRVar p = do
  isRVar <- toBool <$> [CU.exp| bool { $(const Halide::VarOrRVar* p)->is_rvar } |]
  expr <-
    if isRVar
      then wrapCxxRVar =<< [CU.exp| Halide::RVar* { new Halide::RVar{$(const Halide::VarOrRVar* p)->rvar} } |]
      else fmap Var . cxxConstruct $ \ptr ->
        [CU.exp| void { new ($(Halide::Var* ptr)) Halide::Var{$(const Halide::VarOrRVar* p)->var} } |]
  [CU.exp| void { delete $(const Halide::VarOrRVar* p) } |]
  pure expr

class HasHalideType a where
  getHalideType :: a -> IO HalideType

instance HasHalideType (Expr a) where
  getHalideType (Expr fp) =
    withForeignPtr fp $ \e -> alloca $ \t -> do
      [CU.block| void {
        *$(halide_type_t* t) = static_cast<halide_type_t>(
          $(Halide::Expr* e)->type()); } |]
      peek t
  getHalideType (Var fp) =
    withForeignPtr fp $ \e -> alloca $ \t -> do
      [CU.block| void {
        *$(halide_type_t* t) = static_cast<halide_type_t>(
          static_cast<Halide::Expr>(*$(Halide::Var* e)).type()); } |]
      peek t
  getHalideType (RVar fp) =
    withForeignPtr fp $ \e -> alloca $ \t -> do
      [CU.block| void {
        *$(halide_type_t* t) = static_cast<halide_type_t>(
          static_cast<Halide::Expr>(*$(Halide::RVar* e)).type()); } |]
      peek t
  getHalideType _ = error "not implemented"

instance HasHalideType (Ptr CxxExpr) where
  getHalideType e =
    alloca $ \t -> do
      [CU.block| void {
        *$(halide_type_t* t) = static_cast<halide_type_t>($(Halide::Expr* e)->type()); } |]
      peek t

instance HasHalideType (Ptr CxxVar) where
  getHalideType _ = pure $ halideTypeFor (Proxy @Int32)

instance HasHalideType (Ptr CxxRVar) where
  getHalideType _ = pure $ halideTypeFor (Proxy @Int32)

instance HasHalideType (Ptr CxxParameter) where
  getHalideType p =
    alloca $ \t -> do
      [CU.block| void {
        *$(halide_type_t* t) = static_cast<halide_type_t>($(Halide::Internal::Parameter* p)->type()); } |]
      peek t

-- | Wrap a raw @Halide::Internal::Parameter@ pointer in a Haskell value.
--
-- __Note:__ v'Var' objects correspond to expressions of type 'Int32'.
wrapCxxParameter :: Ptr CxxParameter -> IO (ForeignPtr CxxParameter)
wrapCxxParameter = newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteParameter(Halide::Internal::Parameter *p) { delete p; } |]

-- | Helper function to assert that the runtime type of the expression matches it's
-- compile-time type.
--
-- Essentially, given an @(x :: 'Expr' a)@, we check that @x.type()@ in C++ is equal to
-- @'halideTypeFor' (Proxy \@a)@ in Haskell.
checkType :: forall a t. (HasCallStack, IsHalideType a, HasHalideType t) => t -> IO ()
checkType x = do
  let hsType = halideTypeFor (Proxy @a)
  cxxType <- getHalideType x
  unless (cxxType == hsType) . error $
    "Type mismatch: C++ Expr has type "
      <> show cxxType
      <> ", but its Haskell counterpart has type "
      <> show hsType

mkScalarParameter :: forall a. (IsHalideType a) => Maybe Text -> IO (ForeignPtr CxxParameter)
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
    p <- maybe createWithoutName createWithName maybeName
    checkType @a p
    wrapCxxParameter p

getScalarParameter
  :: forall a
   . (IsHalideType a)
  => Maybe Text
  -> IORef (Maybe (ForeignPtr CxxParameter))
  -> IO (ForeignPtr CxxParameter)
getScalarParameter name r = do
  readIORef r >>= \case
    Just fp -> pure fp
    Nothing -> do
      fp <- mkScalarParameter @a name
      writeIORef r (Just fp)
      pure fp

-- | Make sure that the expression is fully constructed. That means that if we
-- are dealing with a 'ScalarParam' rather than an 'Expr', we force the
-- construction of the underlying @Halide::Internal::Parameter@ and convert it
-- to an 'Expr'.
forceExpr :: forall a. (HasCallStack, IsHalideType a) => Expr a -> IO (Expr a)
forceExpr x@(Expr _) = pure x
forceExpr (Var fp) =
  withForeignPtr fp $ \varPtr ->
    cxxConstructExpr $ \ptr ->
      [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
        static_cast<Halide::Expr>(*$(Halide::Var* varPtr))} } |]
forceExpr (RVar fp) =
  withForeignPtr fp $ \rvarPtr ->
    cxxConstructExpr $ \ptr ->
      [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
        static_cast<Halide::Expr>(*$(Halide::RVar* rvarPtr))} } |]
forceExpr (ScalarParam r) =
  getScalarParameter @a Nothing r >>= \fp -> withForeignPtr fp $ \paramPtr ->
    cxxConstructExpr $ \ptr ->
      [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
        Halide::Internal::Variable::make(
          $(Halide::Internal::Parameter* paramPtr)->type(),
          $(Halide::Internal::Parameter* paramPtr)->name(),
          *$(Halide::Internal::Parameter* paramPtr))} } |]

-- | Use the underlying @Halide::Expr@ in an 'IO' action.
asExpr :: (IsHalideType a) => Expr a -> (Ptr CxxExpr -> IO b) -> IO b
asExpr x = withForeignPtr (exprToForeignPtr x)

-- | Allows applying 'asExpr', 'asVar', 'asRVar', and 'asVarOrRVar' to multiple arguments.
--
-- Example usage:
--
-- > asVectorOf @((~) (Expr Int32)) asVarOrRVar (fromTuple args) $ \v -> do
-- >   withFunc func $ \f ->
-- >     [C.throwBlock| void { $(Halide::Func* f)->reorder(
-- >                             *$(std::vector<Halide::VarOrRVar>* v)); } |]
asVectorOf
  :: forall c k ts a
   . (All c ts, HasCxxVector k)
  => (forall t b. (c t) => t -> (Ptr k -> IO b) -> IO b)
  -> Arguments ts
  -> (Ptr (CxxVector k) -> IO a)
  -> IO a
asVectorOf asPtr args action =
  bracket (newCxxVector Nothing) deleteCxxVector (go args)
  where
    go
      :: (All c ts')
      => Arguments ts'
      -> Ptr (CxxVector k)
      -> IO a
    go Nil v = action v
    go (x ::: xs) v = asPtr x $ \p -> cxxVectorPushBack v p >> go xs v

withMany
  :: forall k t a
   . (HasCxxVector k)
  => (t -> (Ptr k -> IO a) -> IO a)
  -> [t]
  -> (Ptr (CxxVector k) -> IO a)
  -> IO a
withMany asPtr args action =
  bracket (newCxxVector Nothing) deleteCxxVector (go args)
  where
    go [] v = action v
    go (x : xs) v = asPtr x $ \p -> cxxVectorPushBack v p >> go xs v

peekRVar :: Ptr CxxRVar -> IO RVar
peekRVar p =
  wrapCxxRVar
    =<< [CU.exp| Halide::RVar* { new Halide::RVar{*$(const Halide::RVar* p)} } |]

-- | Use the underlying @Halide::Var@ in an 'IO' action.
asVar :: (HasCallStack) => Expr Int32 -> (Ptr CxxVar -> IO b) -> IO b
asVar (Var fp) = withForeignPtr fp
asVar _ = error "the expression is not a Var"

-- | Use the underlying @Halide::RVar@ in an 'IO' action.
asRVar :: (HasCallStack) => Expr Int32 -> (Ptr CxxRVar -> IO b) -> IO b
asRVar (RVar fp) = withForeignPtr fp
asRVar _ = error "the expression is not an RVar"

-- | Use the underlying v'Var' or v'RVar' as @Halide::VarOrRVar@ in an 'IO' action.
asVarOrRVar :: (HasCallStack) => VarOrRVar -> (Ptr CxxVarOrRVar -> IO b) -> IO b
asVarOrRVar x action = case x of
  Var fp ->
    let allocate p = [CU.exp| Halide::VarOrRVar* { new Halide::VarOrRVar{*$(Halide::Var* p)} } |]
     in withForeignPtr fp (run . allocate)
  RVar fp ->
    let allocate p = [CU.exp| Halide::VarOrRVar* { new Halide::VarOrRVar{*$(Halide::RVar* p)} } |]
     in withForeignPtr fp (run . allocate)
  _ -> error "the expression is not a Var or an RVar"
  where
    destroy p = [CU.exp| void { delete $(Halide::VarOrRVar* p) } |]
    run allocate = bracket allocate destroy action

-- | Use the underlying @Halide::RVar@ in an 'IO' action.
asScalarParam :: forall a b. (HasCallStack, IsHalideType a) => Expr a -> (Ptr CxxParameter -> IO b) -> IO b
asScalarParam (ScalarParam r) action = do
  fp <- getScalarParameter @a Nothing r
  withForeignPtr fp action
asScalarParam _ _ = error "the expression is not a ScalarParam"

-- | Get the underlying 'ForeignPtr CxxExpr'.
exprToForeignPtr :: (IsHalideType a) => Expr a -> ForeignPtr CxxExpr
exprToForeignPtr x =
  unsafePerformIO $!
    forceExpr x >>= \case
      (Expr fp) -> pure fp
      _ -> error "this cannot happen"

-- | Lift a unary function working with @Halide::Expr@ to work with 'Expr'.
unaryOp :: (IsHalideType a) => (Ptr CxxExpr -> Ptr CxxExpr -> IO ()) -> Expr a -> Expr a
unaryOp f a = unsafePerformIO $
  asExpr a $ \aPtr ->
    cxxConstructExpr $ \destPtr ->
      f aPtr destPtr

-- | Lift a binary function working with @Halide::Expr@ to work with 'Expr'.
binaryOp
  :: (IsHalideType a, IsHalideType b, IsHalideType c)
  => (Ptr CxxExpr -> Ptr CxxExpr -> Ptr CxxExpr -> IO ())
  -> Expr a
  -> Expr b
  -> Expr c
binaryOp f a b = unsafePerformIO $
  asExpr a $ \aPtr -> asExpr b $ \bPtr ->
    cxxConstructExpr $ \destPtr ->
      f aPtr bPtr destPtr

testWriteToStderr :: IO ()
testWriteToStderr = do
  [CU.block| void {
    Halide::Expr expr{123};
    std::ostringstream out;
    out << expr;
    std::cerr << "Output: '" << out.str() << "'" << std::endl;
  } |]
