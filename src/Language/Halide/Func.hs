{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Language.Halide.Func
-- Description : Functions / Arrays
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Func
  ( Func (..)
  , setName
  , define
  , update
  , (!)
  , printLoopNest
  , realize1D
  , TailStrategy (..)
  , vectorize
  , unroll

    -- ** Internal
  , ValidIndex (toExprList)
  , withFunc
  , withBufferParam
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_)
import Data.IORef
import Data.Int (Int32)
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal (with)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Type
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (tail)

importHalide

-- | A function in Halide. Conceptually, it can be thought of as a lazy
-- @n@-dimensional buffer of type @a@.
--
-- This is a wrapper around @Halide::Func@ C++ type.
data Func (n :: Nat) (a :: Type)
  = Func (ForeignPtr CxxFunc)
  | -- | A buffer parameter to a function. See 'Expr' for more the reason, why
    -- we use IORef here.
    BufferParam (IORef (Maybe (ForeignPtr CxxImageParam)))

-- | Different ways to handle a tail case in a split when the split factor does
-- not provably divide the extent.
--
-- This is the Haskell counterpart of [@Halide::TailStrategy@](https://halide-lang.org/docs/namespace_halide.html#a6c6557df562bd7850664e70fdb8fea0f).
data TailStrategy
  = -- | Round up the extent to be a multiple of the split factor.
    --
    -- Not legal for RVars, as it would change the meaning of the algorithm.
    --
    -- * Pros: generates the simplest, fastest code.
    -- * Cons: if used on a stage that reads from the input or writes to the
    -- output, constrains the input or output size to be a multiple of the
    -- split factor.
    TailRoundUp
  | -- | Guard the inner loop with an if statement that prevents evaluation
    -- beyond the original extent.
    --
    -- Always legal. The if statement is treated like a boundary condition, and
    -- factored out into a loop epilogue if possible.
    --
    -- * Pros: no redundant re-evaluation; does not constrain input our output sizes.
    -- * Cons: increases code size due to separate tail-case handling;
    -- vectorization will scalarize in the tail case to handle the if
    -- statement.
    TailGuardWithIf
  | -- | Guard the loads and stores in the loop with an if statement that
    -- prevents evaluation beyond the original extent.
    --
    -- Always legal. The if statement is treated like a boundary condition, and
    -- factored out into a loop epilogue if possible.
    -- * Pros: no redundant re-evaluation; does not constrain input or output
    -- sizes.
    -- * Cons: increases code size due to separate tail-case handling.
    TailPredicate
  | -- | Guard the loads in the loop with an if statement that prevents
    -- evaluation beyond the original extent.
    --
    -- Only legal for innermost splits. Not legal for RVars, as it would change
    -- the meaning of the algorithm. The if statement is treated like a
    -- boundary condition, and factored out into a loop epilogue if possible.
    -- * Pros: does not constrain input sizes, output size constraints are
    -- simpler than full predication.
    -- * Cons: increases code size due to separate tail-case handling,
    -- constrains the output size to be a multiple of the split factor.
    TailPredicateLoads
  | -- | Guard the stores in the loop with an if statement that prevents
    -- evaluation beyond the original extent.
    --
    -- Only legal for innermost splits. Not legal for RVars, as it would change
    -- the meaning of the algorithm. The if statement is treated like a
    -- boundary condition, and factored out into a loop epilogue if possible.
    -- * Pros: does not constrain output sizes, input size constraints are
    -- simpler than full predication.
    -- * Cons: increases code size due to separate tail-case handling,
    -- constraints the input size to be a multiple of the split factor.
    TailPredicateStores
  | -- | Prevent evaluation beyond the original extent by shifting the tail
    -- case inwards, re-evaluating some points near the end.
    --
    -- Only legal for pure variables in pure definitions. If the inner loop is
    -- very simple, the tail case is treated like a boundary condition and
    -- factored out into an epilogue.
    --
    -- This is a good trade-off between several factors. Like 'TailRoundUp', it
    -- supports vectorization well, because the inner loop is always a fixed
    -- size with no data-dependent branching. It increases code size slightly
    -- for inner loops due to the epilogue handling, but not for outer loops
    -- (e.g. loops over tiles). If used on a stage that reads from an input or
    -- writes to an output, this stategy only requires that the input/output
    -- extent be at least the split factor, instead of a multiple of the split
    -- factor as with 'TailRoundUp'.
    TailShiftInwards
  | -- | For pure definitions use 'TailShiftInwards'.
    --
    -- For pure vars in update definitions use 'TailRoundUp'. For RVars in update
    -- definitions use 'TailGuardWithIf'.
    TailAuto
  deriving stock (Eq, Ord, Show)

instance Enum TailStrategy where
  fromEnum =
    fromIntegral . \case
      TailRoundUp -> [CU.pure| int { static_cast<int>(Halide::TailStrategy::RoundUp) } |]
      TailGuardWithIf -> [CU.pure| int { static_cast<int>(Halide::TailStrategy::GuardWithIf) } |]
      TailPredicate -> [CU.pure| int { static_cast<int>(Halide::TailStrategy::Predicate) } |]
      TailPredicateLoads -> [CU.pure| int { static_cast<int>(Halide::TailStrategy::PredicateLoads) } |]
      TailPredicateStores -> [CU.pure| int { static_cast<int>(Halide::TailStrategy::PredicateStores) } |]
      TailShiftInwards -> [CU.pure| int { static_cast<int>(Halide::TailStrategy::ShiftInwards) } |]
      TailAuto -> [CU.pure| int { static_cast<int>(Halide::TailStrategy::Auto) } |]
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::TailStrategy::RoundUp) } |] = TailRoundUp
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::TailStrategy::GuardWithIf) } |] = TailGuardWithIf
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::TailStrategy::Predicate) } |] = TailPredicate
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::TailStrategy::PredicateLoads) } |] = TailPredicateLoads
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::TailStrategy::PredicateStores) } |] = TailPredicateStores
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::TailStrategy::ShiftInwards) } |] = TailShiftInwards
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::TailStrategy::Auto) } |] = TailAuto
    | otherwise = error $ "invalid TailStrategy: " <> show k

-- | Split a dimension by the given factor, then vectorize the inner dimension.
--
-- This is how you vectorize a loop of unknown size. The variable to be
-- vectorized should be the innermost one. After this call, var refers to the
-- outer dimension of the split.
vectorize
  :: (KnownNat n, IsHalideType a)
  => TailStrategy
  -> Func n a
  -> Expr Int32
  -- ^ Variable to vectorize
  -> Expr Int32
  -- ^ Split factor
  -> IO ()
vectorize strategy func var factor =
  withFunc func $ \f ->
    asVarOrRVar var $ \x ->
      asExpr factor $ \n ->
        handleHalideExceptionsM
          [C.tryBlock| void {
            $(Halide::Func* f)->vectorize(*$(Halide::VarOrRVar* x), *$(Halide::Expr* n),
                                          static_cast<Halide::TailStrategy>($(int tail)));
          } |]
  where
    tail = fromIntegral (fromEnum strategy)

unroll
  :: (KnownNat n, IsHalideType a)
  => TailStrategy
  -> Func n a
  -> Expr Int32
  -- ^ Variable to vectorize
  -> Expr Int32
  -- ^ Split factor
  -> IO ()
unroll strategy func var factor =
  withFunc func $ \f ->
    asVarOrRVar var $ \x ->
      asExpr factor $ \n ->
        handleHalideExceptionsM
          [C.tryBlock| void {
            $(Halide::Func* f)->unroll(*$(Halide::VarOrRVar* x), *$(Halide::Expr* n),
                                       static_cast<Halide::TailStrategy>($(int tail)));
          } |]
  where
    tail = fromIntegral (fromEnum strategy)

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

getBufferParameter
  :: forall n a
   . (KnownNat n, IsHalideType a)
  => Maybe Text
  -> IORef (Maybe (ForeignPtr CxxImageParam))
  -> IO (ForeignPtr CxxImageParam)
getBufferParameter name r =
  readIORef r >>= \case
    Just fp -> pure fp
    Nothing -> do
      fp <- mkBufferParameter @n @a name
      writeIORef r (Just fp)
      pure fp

-- | Same as 'withFunc', but ensures that we're dealing with 'BufferParam' instead of a 'Func'.
withBufferParam
  :: forall n a b
   . (HasCallStack, KnownNat n, IsHalideType a)
  => Func n a
  -> (Ptr CxxImageParam -> IO b)
  -> IO b
withBufferParam (BufferParam r) action = do
  fp <- getBufferParameter @n @a Nothing r
  withForeignPtr fp action
withBufferParam (Func _) _ = error "withBufferParam called on Func"

instance (KnownNat n, IsHalideType a) => Named (Func n a) where
  setName :: Func n a -> Text -> IO ()
  setName (Func _) _ = error "the name of this Func has already been set"
  setName (BufferParam r) name = do
    readIORef r >>= \case
      Just _ -> error "the name of this Func has already been set"
      Nothing -> do
        fp <- mkBufferParameter @n @a (Just name)
        writeIORef r (Just fp)

deleteCxxFunc :: FunPtr (Ptr CxxFunc -> IO ())
deleteCxxFunc = [C.funPtr| void deleteFunc(Halide::Func *x) { delete x; } |]

-- | Get the underlying pointer to @Halide::Func@ and invoke an 'IO' action with it.
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

applyFunc :: IsHalideType a => ForeignPtr CxxFunc -> [ForeignPtr CxxExpr] -> IO (Expr a)
applyFunc func args =
  withForeignPtr func $ \f ->
    withExprMany args $ \v ->
      wrapCxxExpr
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

updateFunc
  :: ForeignPtr CxxFunc
  -> [ForeignPtr CxxExpr]
  -> ForeignPtr CxxExpr
  -> IO ()
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

-- | Specifies that a type can be used as an index to a Halide function.
class ValidIndex (a :: Type) (n :: Nat) | a -> n where
  toExprList :: a -> [ForeignPtr CxxExpr]

instance ValidIndex (Expr Int32) 1 where
  toExprList :: Expr Int32 -> [ForeignPtr CxxExpr]
  toExprList a = [exprToForeignPtr a]

instance ValidIndex (Expr Int32, Expr Int32) 2 where
  toExprList :: (Expr Int32, Expr Int32) -> [ForeignPtr CxxExpr]
  toExprList (a, b) = [exprToForeignPtr a, exprToForeignPtr b]

-- | Define a Halide function.
--
-- @define "f" i e@ defines a Halide function called "f" such that @f[i] = e@.
define :: (ValidIndex i n, IsHalideType a) => Text -> i -> Expr a -> IO (Func n a)
define name x y = Func <$> defineFunc name (toExprList x) (exprToForeignPtr y)

-- | Create an update definition for a Halide function.
--
-- @update f i e@ creates an update definition for @f@ that performs @f[i] = e@.
update :: (ValidIndex i n, KnownNat n, IsHalideType a) => Func n a -> i -> Expr a -> IO ()
update func x y = updateFunc (funcToForeignPtr func) (toExprList x) (exprToForeignPtr y)

infix 9 !

-- | Apply a Halide function. Conceptually, @f ! i@ is equivalent to @f[i]@, i.e.
-- indexing into a lazy array.
(!) :: (ValidIndex i n, KnownNat n, IsHalideType r) => Func n r -> i -> Expr r
(!) func args = unsafePerformIO $ applyFunc (funcToForeignPtr func) (toExprList args)

-- | Write out the loop nests specified by the schedule for this function.
--
-- Helpful for understanding what a schedule is doing.
--
-- For more info, see
-- [@Halide::Func::print_loop_nest@](https://halide-lang.org/docs/class_halide_1_1_func.html#a03f839d9e13cae4b87a540aa618589ae)
printLoopNest :: (KnownNat n, IsHalideType r) => Func n r -> IO ()
printLoopNest func = withFunc func $ \f ->
  [C.exp| void { $(Halide::Func* f)->print_loop_nest() } |]

-- | Evaluate this function over a one-dimensional domain and return the
-- resulting buffer or buffers.
realize1D
  :: IsHalideType a
  => Func 1 a
  -- ^ Function to evaluate
  -> Int
  -- ^ @size@ of the domain. The function will be evaluated on @[0, ..., size -1]@
  -> IO (Vector a)
realize1D func size = do
  buffer <- SM.new size
  withHalideBuffer buffer $ \x -> do
    let b = castPtr x
    withFunc func $ \f ->
      [CU.exp| void {
        $(Halide::Func* f)->realize(
          Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)}) } |]
  S.unsafeFreeze buffer
