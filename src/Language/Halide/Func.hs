{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
-- {-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Language.Halide.Func
-- Description : Functions / Arrays
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Func
  ( -- * Defining pipelines
    Func (..)
  , FuncTy (..)
  , Stage (..)
  , Function
  , Parameter
  , SomeFunc (..)
  , withSomeFunc
  , foldSomeFunc
  , buffer
  , scalar
  , define
  , defineSomeFunc
  , (!)
  , (!!)
  , realizeOnTarget
  , realize

    -- * Scheduling
  , Schedulable (..)
  , TailStrategy (..)

    -- ** 'Func'-specific
  , computeRoot
  , getStage
  , getLoopLevel
  , getLoopLevelAtStage
  , asUsed
  , asUsedBy
  , copyToDevice
  , copyToHost
  , storeAt
  , computeAt
  , dim
  , estimate
  , bound
  , getArgs
  -- , deepCopy

    -- * Update definitions
  , update
  , updateSomeFunc
  , hasUpdateDefinitions
  , getUpdateStage

    -- * Debugging
  , prettyLoopNest

    -- * Internal
  , asBufferParam
  , withFunc
  , withCxxFunc
  , withBufferParam
  , wrapCxxFunc
  , CxxStage
  , wrapCxxStage
  , withCxxStage
  )
where

import Control.Exception (bracket)
import Control.Monad (forM)
import Data.Constraint
import Data.Functor ((<&>))
import Data.IORef
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Foreign.ForeignPtr
import Foreign.Marshal (toBool, with)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Dimension
import Language.Halide.Expr
import Language.Halide.LoopLevel
import Language.Halide.Target
import Language.Halide.Type
import Language.Halide.Utils
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce
import Prelude hiding (Eq (..), min, tail, (!!))
import Prelude qualified

-- | Haskell counterpart of [Halide::Stage](https://halide-lang.org/docs/class_halide_1_1_stage.html).
data CxxStage

importHalide

-- | A function in Halide. Conceptually, it can be thought of as a lazy
-- @n@-dimensional buffer of type @a@.
--
-- Here, @a@ is most often @'Expr' t@ for a type @t@ that is an instance of 'IsHalideType'.
-- However, one can also define @Func@s that return multiple values. In this case, @a@ will
-- be a tuple of 'Expr's.
--
-- This is a wrapper around the [@Halide::Func@](https://halide-lang.org/docs/class_halide_1_1_func.html)
-- C++ type.
data Func (t :: FuncTy) (n :: Nat) (a :: Type) where
  Func :: {-# UNPACK #-} !(ForeignPtr CxxFunc) -> Func 'FuncTy n a
  Param :: IsHalideType a => {-# UNPACK #-} !(IORef (Maybe (ForeignPtr CxxImageParam))) -> Func 'ParamTy n (Expr a)

data SomeFunc t a where
  SomeFunc :: HasIndexType n => !(Func t n a) -> SomeFunc t a

withSomeFunc :: (forall n. HasIndexType n => Func t n a -> r) -> SomeFunc t a -> r
withSomeFunc f (SomeFunc x) = f x

foldSomeFunc :: SomeFunc t a -> (forall n. HasIndexType n => Func t n a -> r) -> r
foldSomeFunc x f = withSomeFunc f x

-- | Function type. It can either be 'FuncTy' which means that we have defined the function ourselves,
-- or 'ParamTy' which means that it's a parameter to our pipeline.
data FuncTy = FuncTy | ParamTy
  deriving stock (Show, Prelude.Eq, Ord)

-- | Synonym for the most commonly used function type.
type Function n a = Func 'FuncTy n (Expr a)

-- | Synonym for the most commonly used parameter type.
type Parameter n a = Func 'ParamTy n (Expr a)

-- | A single definition of a t'Func'.
newtype Stage (n :: Nat) (a :: Type) = Stage (ForeignPtr CxxStage)

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
  deriving stock (Prelude.Eq, Ord, Show)

-- | Common scheduling functions
class KnownNat n => Schedulable f (n :: Nat) (a :: Type) where
  -- | Vectorize the dimension.
  vectorize :: VarOrRVar -> f n a -> IO (f n a)

  -- | Unroll the dimension.
  unroll :: VarOrRVar -> f n a -> IO (f n a)

  -- | Reorder variables to have the given nesting order, from innermost out.
  --
  -- Note that @variables@ should only contain variables that belong to the function.
  -- If this is not the case, a runtime error will be thrown.
  reorder
    :: [VarOrRVar]
    -- ^ variables
    -> f n a
    -- ^ function or stage
    -> IO (f n a)

  -- | Split a dimension into inner and outer subdimensions with the given names, where the inner dimension
  -- iterates from @0@ to @factor-1@.
  --
  -- The inner and outer subdimensions can then be dealt with using the other scheduling calls. It's okay
  -- to reuse the old variable name as either the inner or outer variable. The first argument specifies
  -- how the tail should be handled if the split factor does not provably divide the extent.
  split :: TailStrategy -> VarOrRVar -> (VarOrRVar, VarOrRVar) -> Expr Int32 -> f n a -> IO (f n a)

  -- | Join two dimensions into a single fused dimenion.
  --
  -- The fused dimension covers the product of the extents of the inner and outer dimensions given.
  fuse :: (VarOrRVar, VarOrRVar) -> VarOrRVar -> f n a -> IO (f n a)

  -- | Mark the dimension to be traversed serially
  serial :: VarOrRVar -> f n a -> IO (f n a)

  -- | Mark the dimension to be traversed in parallel
  parallel :: VarOrRVar -> f n a -> IO (f n a)

  -- | Issue atomic updates for this Func.
  atomic
    :: Bool
    -- ^ whether to override the associativity test
    -> f n a
    -> IO (f n a)

  specialize :: Expr Bool -> f n a -> IO (Stage n a)
  specializeFail :: Text -> f n a -> IO ()
  gpuBlocks :: (KnownNat k, 1 <= k, k <= 3) => DeviceAPI -> IndexType k -> f n a -> IO (f n a)
  gpuThreads :: (KnownNat k, 1 <= k, k <= 3) => DeviceAPI -> IndexType k -> f n a -> IO (f n a)
  gpuLanes :: DeviceAPI -> VarOrRVar -> f n a -> IO (f n a)

  -- | Schedule the iteration over this stage to be fused with another stage from outermost loop to a
  -- given LoopLevel.
  --
  -- For more info, see [Halide::Stage::compute_with](https://halide-lang.org/docs/class_halide_1_1_stage.html#a82a2ae25a009d6a2d52cb407a25f0a5b).
  computeWith :: LoopAlignStrategy -> f n a -> LoopLevel t -> IO ()

-- | GHC is not able to automatically prove the transitivity property for type-level naturals. We help GHC out 😀.
proveTransitivityOfLessThanEqual :: (KnownNat k, KnownNat l, KnownNat m, k <= l, l <= m) => Dict (k <= m)
proveTransitivityOfLessThanEqual = unsafeCoerce $ Dict @(1 <= 2)

instance KnownNat n => Schedulable Stage n a where
  vectorize var stage = do
    withCxxStage stage $ \stage' ->
      asVarOrRVar var $ \var' ->
        [C.throwBlock| void {
          handle_halide_exceptions([=](){
            $(Halide::Stage* stage')->vectorize(*$(const Halide::VarOrRVar* var'));
          });
        } |]
    pure stage
  unroll var stage = do
    withCxxStage stage $ \stage' ->
      asVarOrRVar var $ \var' ->
        [C.throwBlock| void {
          handle_halide_exceptions([=](){
            $(Halide::Stage* stage')->unroll(*$(const Halide::VarOrRVar* var'));
          });
        } |]
    pure stage
  reorder args stage = do
    withMany asVarOrRVar args $ \args' -> do
      withCxxStage stage $ \stage' ->
        [C.throwBlock| void {
          handle_halide_exceptions([=]() {
            $(Halide::Stage* stage')->reorder(
              *$(const std::vector<Halide::VarOrRVar>* args')); 
          });
        } |]
    pure stage
  split tail old (outer, inner) factor stage = do
    withCxxStage stage $ \stage' ->
      asVarOrRVar old $ \old' ->
        asVarOrRVar outer $ \outer' ->
          asVarOrRVar inner $ \inner' ->
            asExpr factor $ \factor' ->
              [C.throwBlock| void {
                handle_halide_exceptions([=](){
                  $(Halide::Stage* stage')->split(
                    *$(const Halide::VarOrRVar* old'),
                    *$(const Halide::VarOrRVar* outer'),
                    *$(const Halide::VarOrRVar* inner'),
                    *$(const Halide::Expr* factor'),
                    static_cast<Halide::TailStrategy>($(int t)));
                });
              } |]
    pure stage
    where
      t = fromIntegral . fromEnum $ tail
  fuse (outer, inner) fused stage = do
    withCxxStage stage $ \stage' ->
      asVarOrRVar outer $ \outer' ->
        asVarOrRVar inner $ \inner' ->
          asVarOrRVar fused $ \fused' ->
            [C.throwBlock| void {
              handle_halide_exceptions([=](){
                $(Halide::Stage* stage')->fuse(
                  *$(const Halide::VarOrRVar* outer'),
                  *$(const Halide::VarOrRVar* inner'),
                  *$(const Halide::VarOrRVar* fused'));
              });
            } |]
    pure stage
  serial var stage = do
    withCxxStage stage $ \stage' ->
      asVarOrRVar var $ \var' ->
        [C.throwBlock| void {
          handle_halide_exceptions([=](){
            $(Halide::Stage* stage')->serial(*$(const Halide::VarOrRVar* var'));
          });
        } |]
    pure stage
  parallel var stage = do
    withCxxStage stage $ \stage' ->
      asVarOrRVar var $ \var' ->
        [C.throwBlock| void {
          handle_halide_exceptions([=](){
            $(Halide::Stage* stage')->parallel(*$(const Halide::VarOrRVar* var'));
          });
        } |]
    pure stage
  atomic (fromIntegral . fromEnum -> override) stage = do
    withCxxStage stage $ \stage' ->
      [C.throwBlock| void {
        handle_halide_exceptions([=](){
          $(Halide::Stage* stage')->atomic($(bool override));
        });
      } |]
    pure stage

  specialize cond stage = do
    withCxxStage stage $ \stage' ->
      asExpr cond $ \cond' ->
        wrapCxxStage
          =<< [C.throwBlock| Halide::Stage* {
                return handle_halide_exceptions([=](){
                  return new Halide::Stage{$(Halide::Stage* stage')->specialize(
                    *$(const Halide::Expr* cond'))};
                });
              } |]
  specializeFail (T.encodeUtf8 -> s) stage =
    withCxxStage stage $ \stage' ->
      [C.throwBlock| void {
        return handle_halide_exceptions([=](){
          $(Halide::Stage* stage')->specialize_fail(
            std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)});
        });
      } |]
  gpuBlocks :: forall k. (KnownNat k, 1 <= k, k <= 3) => DeviceAPI -> IndexType k -> Stage n a -> IO (Stage n a)
  gpuBlocks (fromIntegral . fromEnum -> api :: C.CInt) vars stage =
    case proveTransitivityOfLessThanEqual @k @3 @10 of
      Dict -> case proveIndexTypeProperties @k of
        Sub Dict ->
          withCxxStage stage $ \stage' ->
            asVectorOf @((~) (Expr Int32)) asVarOrRVar (fromTuple vars) $ \vars' -> do
              [C.throwBlock| void {
                handle_halide_exceptions([=](){
                  auto const& vars = *$(const std::vector<Halide::VarOrRVar>* vars');
                  auto& stage = *$(Halide::Stage* stage');
                  auto const device = static_cast<Halide::DeviceAPI>($(int api));
                  switch (vars.size()) {
                    case 1: stage.gpu_blocks(vars.at(0), device);
                            break;
                    case 2: stage.gpu_blocks(vars.at(0), vars.at(1), device);
                            break;
                    case 3: stage.gpu_blocks(vars.at(0), vars.at(1), vars.at(2), device);
                            break;
                    default: throw std::runtime_error{"unexpected number of arguments in gpuBlocks"};
                  }
                });
              } |]
              pure stage
  gpuThreads :: forall k. (KnownNat k, 1 <= k, k <= 3) => DeviceAPI -> IndexType k -> Stage n a -> IO (Stage n a)
  gpuThreads (fromIntegral . fromEnum -> api :: C.CInt) vars stage =
    case proveTransitivityOfLessThanEqual @k @3 @10 of
      Dict -> case proveIndexTypeProperties @k of
        Sub Dict -> do
          withCxxStage stage $ \stage' ->
            asVectorOf @((~) (Expr Int32)) asVarOrRVar (fromTuple vars) $ \vars' -> do
              [C.throwBlock| void {
                handle_halide_exceptions([=](){
                  auto const& vars = *$(const std::vector<Halide::VarOrRVar>* vars');
                  auto& stage = *$(Halide::Stage* stage');
                  auto const device = static_cast<Halide::DeviceAPI>($(int api));
                  switch (vars.size()) {
                    case 1: stage.gpu_threads(vars.at(0), device);
                            break;
                    case 2: stage.gpu_threads(vars.at(0), vars.at(1), device);
                            break;
                    case 3: stage.gpu_threads(vars.at(0), vars.at(1), vars.at(2), device);
                            break;
                    default: throw std::runtime_error{"unexpected number of arguments in gpuThreads"};
                  }
                });
              } |]
          pure stage
  gpuLanes (fromIntegral . fromEnum -> api) var stage = do
    withCxxStage stage $ \stage' ->
      asVarOrRVar var $ \var' ->
        [C.throwBlock| void {
          handle_halide_exceptions([=](){
            $(Halide::Stage* stage')->gpu_lanes(
              *$(const Halide::VarOrRVar* var'),
              static_cast<Halide::DeviceAPI>($(int api)));
          });
        } |]
    pure stage
  computeWith (fromIntegral . fromEnum -> align) stage level = do
    withCxxStage stage $ \stage' ->
      withCxxLoopLevel level $ \level' ->
        [C.throwBlock| void {
          handle_halide_exceptions([=]() {
            $(Halide::Stage* stage')->compute_with(
              *$(const Halide::LoopLevel* level'),
              static_cast<Halide::LoopAlignStrategy>($(int align)));
          });
        } |]

viaStage1
  :: KnownNat n
  => (a -> Stage n b -> IO (Stage n b))
  -> a
  -> Func t n b
  -> IO (Func t n b)
viaStage1 f a1 func = do
  _ <- f a1 =<< getStage func
  pure func

viaStage2
  :: (KnownNat n)
  => (a1 -> a2 -> Stage n b -> IO (Stage n b))
  -> a1
  -> a2
  -> Func t n b
  -> IO (Func t n b)
viaStage2 f a1 a2 func = do
  _ <- f a1 a2 =<< getStage func
  pure func

{-
viaStage3
  :: (KnownNat n, IsHalideType b)
  => (a1 -> a2 -> a3 -> Stage n b -> IO (Stage n b))
  -> a1
  -> a2
  -> a3
  -> Func t n b
  -> IO (Func t n b)
viaStage3 f a1 a2 a3 func = do
  _ <- f a1 a2 a3 =<< getStage func
  pure func
-}

viaStage4
  :: (KnownNat n)
  => (a1 -> a2 -> a3 -> a4 -> Stage n b -> IO (Stage n b))
  -> a1
  -> a2
  -> a3
  -> a4
  -> Func t n b
  -> IO (Func t n b)
viaStage4 f a1 a2 a3 a4 func = do
  _ <- f a1 a2 a3 a4 =<< getStage func
  pure func

instance KnownNat n => Schedulable (Func t) n a where
  vectorize = viaStage1 vectorize
  unroll = viaStage1 unroll
  reorder = viaStage1 reorder
  split = viaStage4 split
  fuse = viaStage2 fuse
  serial = viaStage1 serial
  parallel = viaStage1 parallel
  atomic = viaStage1 atomic
  specialize cond func = getStage func >>= specialize cond
  specializeFail msg func = getStage func >>= specializeFail msg
  gpuBlocks = viaStage2 gpuBlocks
  gpuThreads = viaStage2 gpuThreads
  gpuLanes = viaStage2 gpuLanes
  computeWith a f l = getStage f >>= \f' -> computeWith a f' l

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

-- | Statically declare the range over which the function will be evaluated in the general case.
--
-- This provides a basis for the auto scheduler to make trade-offs and scheduling decisions.
-- The auto generated schedules might break when the sizes of the dimensions are very different from the
-- estimates specified. These estimates are used only by the auto scheduler if the function is a pipeline output.
estimate
  :: KnownNat n
  => Expr Int32
  -- ^ index variable
  -> Expr Int32
  -- ^ @min@ estimate
  -> Expr Int32
  -- ^ @extent@ estimate
  -> Func t n a
  -> IO ()
estimate var start extent func =
  withFunc func $ \f ->
    asVar var $ \i ->
      asExpr start $ \minExpr ->
        asExpr extent $ \extentExpr ->
          [CU.exp| void {
            $(Halide::Func* f)->set_estimate(
              *$(Halide::Var* i), *$(Halide::Expr* minExpr), *$(Halide::Expr* extentExpr)) } |]

-- | Statically declare the range over which a function should be evaluated.
--
-- This can let Halide perform some optimizations. E.g. if you know there are going to be 4 color channels,
-- you can completely vectorize the color channel dimension without the overhead of splitting it up.
-- If bounds inference decides that it requires more of this function than the bounds you have stated,
-- a runtime error will occur when you try to run your pipeline.
bound
  :: KnownNat n
  => Expr Int32
  -- ^ index variable
  -> Expr Int32
  -- ^ @min@ estimate
  -> Expr Int32
  -- ^ @extent@ estimate
  -> Func t n a
  -> IO ()
bound var start extent func =
  withFunc func $ \f ->
    asVar var $ \i ->
      asExpr start $ \minExpr ->
        asExpr extent $ \extentExpr ->
          [CU.exp| void {
            $(Halide::Func* f)->bound(
              *$(Halide::Var* i), *$(Halide::Expr* minExpr), *$(Halide::Expr* extentExpr)) } |]

-- | Get the index arguments of the function.
--
-- The returned list contains exactly @n@ elements.
getArgs :: KnownNat n => Func t n a -> IO [Var]
getArgs func =
  withFunc func $ \func' -> do
    let allocate =
          [CU.exp| std::vector<Halide::Var>* { 
            new std::vector<Halide::Var>{$(const Halide::Func* func')->args()} } |]
        destroy v = [CU.exp| void { delete $(std::vector<Halide::Var>* v) } |]
    bracket allocate destroy $ \v -> do
      n <- [CU.exp| size_t { $(const std::vector<Halide::Var>* v)->size() } |]
      forM [0 .. n - 1] $ \i ->
        fmap Var . cxxConstruct $ \ptr ->
          [CU.exp| void {
            new ($(Halide::Var* ptr)) Halide::Var{$(const std::vector<Halide::Var>* v)->at($(size_t i))} } |]

-- | Compute all of this function once ahead of time.
--
-- See [Halide::Func::compute_root](https://halide-lang.org/docs/class_halide_1_1_func.html#a29df45a4a16a63eb81407261a9783060) for more info.
computeRoot :: KnownNat n => Func t n a -> IO (Func t n a)
computeRoot func = do
  withFunc func $ \f ->
    [C.throwBlock| void { handle_halide_exceptions([=](){ $(Halide::Func* f)->compute_root(); }); } |]
  pure func

-- | Creates and returns a new identity Func that wraps this Func.
--
-- During compilation, Halide replaces all calls to this Func done by 'f' with calls to the wrapper.
-- If this Func is already wrapped for use in 'f', will return the existing wrapper.
--
-- For more info, see [Halide::Func::in](https://halide-lang.org/docs/class_halide_1_1_func.html#a9d619f2d0111ea5bf640781d1324d050).
asUsedBy
  :: (KnownNat n, KnownNat m)
  => Func t1 n a
  -> Func 'FuncTy m b
  -> IO (Func 'FuncTy n a)
asUsedBy g f =
  withFunc g $ \gPtr -> withFunc f $ \fPtr ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* {
            new Halide::Func{$(Halide::Func* gPtr)->in(*$(Halide::Func* fPtr))} } |]

-- | Create and return a global identity wrapper, which wraps all calls to this Func by any other Func.
--
-- If a global wrapper already exists, returns it. The global identity wrapper is only used by callers
-- for which no custom wrapper has been specified.
asUsed :: KnownNat n => Func t n a -> IO (Func 'FuncTy n a)
asUsed f =
  withFunc f $ \fPtr ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* { new Halide::Func{$(Halide::Func* fPtr)->in()} } |]

-- | Declare that this function should be implemented by a call to @halide_buffer_copy@ with the given
-- target device API.
--
-- Asserts that the @Func@ has a pure definition which is a simple call to a single input, and no update
-- definitions. The wrapper @Func@s returned by 'asUsed' are suitable candidates. Consumes all pure variables,
-- and rewrites the @Func@ to have an extern definition that calls @halide_buffer_copy@.
copyToDevice :: KnownNat n => DeviceAPI -> Func t n a -> IO (Func t n a)
copyToDevice deviceApi func = do
  withFunc func $ \f ->
    [C.throwBlock| void {
      handle_halide_exceptions([=](){
        $(Halide::Func* f)->copy_to_device(static_cast<Halide::DeviceAPI>($(int api)));
      });
    } |]
  pure func
  where
    api = fromIntegral . fromEnum $ deviceApi

-- | Same as @'copyToDevice' 'DeviceHost'@
copyToHost :: KnownNat n => Func t n a -> IO (Func t n a)
copyToHost = copyToDevice DeviceHost

mkBufferParameter
  :: forall n a. (KnownNat n, IsHalideType a) => Maybe Text -> IO (ForeignPtr CxxImageParam)
mkBufferParameter maybeName = do
  with (halideTypeFor (Proxy @a)) $ \t -> do
    let d = fromIntegral $ natVal (Proxy @n)
        createWithoutName =
          [CU.exp| Halide::ImageParam* {
            new Halide::ImageParam{Halide::Type{*$(halide_type_t* t)}, $(int d)} } |]
        deleter = [C.funPtr| void deleteImageParam(Halide::ImageParam* p) { delete p; } |]
        createWithName name =
          let s = T.encodeUtf8 name
           in [CU.exp| Halide::ImageParam* {
                new Halide::ImageParam{
                      Halide::Type{*$(halide_type_t* t)},
                      $(int d),
                      std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}} } |]
    newForeignPtr deleter =<< maybe createWithoutName createWithName maybeName

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

-- | Same as 'withFunc', but ensures that we're dealing with 'Param' instead of a 'Func'.
withBufferParam
  :: forall n a b
   . (HasCallStack, KnownNat n, IsHalideType a)
  => Func 'ParamTy n (Expr a)
  -> (Ptr CxxImageParam -> IO b)
  -> IO b
withBufferParam (Param r) action =
  getBufferParameter @n @a Nothing r >>= flip withForeignPtr action

-- | Get the underlying pointer to @Halide::Func@ and invoke an 'IO' action with it.
withFunc :: KnownNat n => Func t n a -> (Ptr CxxFunc -> IO b) -> IO b
withFunc f action = case f of
  Func fp -> withForeignPtr fp action
  p@(Param _) -> forceFunc p >>= \(Func fp) -> withForeignPtr fp action

withCxxFunc :: KnownNat n => Func 'FuncTy n a -> (Ptr CxxFunc -> IO b) -> IO b
withCxxFunc (Func fp) = withForeignPtr fp

wrapCxxFunc :: Ptr CxxFunc -> IO (Func 'FuncTy n a)
wrapCxxFunc = fmap Func . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteFunc(Halide::Func *x) { delete x; } |]

forceFunc :: forall t n a. KnownNat n => Func t n (Expr a) -> IO (Func 'FuncTy n (Expr a))
forceFunc = \case
  x@(Func _) -> pure x
  (Param r) -> do
    fp <- getBufferParameter @n @a Nothing r
    withForeignPtr fp $ \p ->
      wrapCxxFunc
        =<< [CU.exp| Halide::Func* {
              new Halide::Func{static_cast<Halide::Func>(*$(Halide::ImageParam* p))} } |]

class IsFuncDefinition d where
  definitionToExprList :: d -> [ForeignPtr CxxExpr]
  exprListToDefinition :: [ForeignPtr CxxExpr] -> d

instance IsHalideType a => IsFuncDefinition (Expr a) where
  definitionToExprList = pure . exprToForeignPtr
  exprListToDefinition [x1] = unsafePerformIO $ withForeignPtr x1 (checkType @a) >> pure (Expr x1)
  exprListToDefinition _ = error "should never happen"

instance (IsHalideType a1, IsHalideType a2) => IsFuncDefinition (Expr a1, Expr a2) where
  definitionToExprList (x1, x2) = [exprToForeignPtr x1, exprToForeignPtr x2]
  exprListToDefinition [x1, x2] = unsafePerformIO $ do
    withForeignPtr x1 (checkType @a1)
    withForeignPtr x2 (checkType @a2)
    pure (Expr x1, Expr x2)
  exprListToDefinition _ = error "should never happen"

instance (IsHalideType a1, IsHalideType a2, IsHalideType a3) => IsFuncDefinition (Expr a1, Expr a2, Expr a3) where
  definitionToExprList (x1, x2, x3) = [exprToForeignPtr x1, exprToForeignPtr x2, exprToForeignPtr x3]
  exprListToDefinition [x1, x2, x3] = unsafePerformIO $ do
    withForeignPtr x1 (checkType @a1)
    withForeignPtr x2 (checkType @a2)
    withForeignPtr x3 (checkType @a3)
    pure (Expr x1, Expr x2, Expr x3)
  exprListToDefinition _ = error "should never happen"

-- | Define a Halide function.
--
-- @define "f" i e@ defines a Halide function called "f" such that @f[i] = e@.
--
-- Here, @i@ is an @n@-element tuple of t'Var', i.e. the following are all valid:
--
-- >>> [x, y, z] <- mapM mkVar ["x", "y", "z"]
-- >>> f1 <- define "f1" x (0 :: Expr Float)
-- >>> f2 <- define "f2" (x, y) (0 :: Expr Float)
-- >>> f3 <- define "f3" (x, y, z) (0 :: Expr Float)
define
  :: forall n d
   . (HasIndexType n, IsFuncDefinition d)
  => Text
  -> IndexType n
  -> d
  -> IO (Func 'FuncTy n d)
define name args definition =
  case proveIndexTypeProperties @n of
    Sub Dict -> asVectorOf @((~) (Expr Int32)) asVar (fromTuple args) $ \x ->
      defineDynamic name x definition

defineSomeFunc :: forall d. IsFuncDefinition d => Text -> [Var] -> d -> IO (SomeFunc 'FuncTy d)
defineSomeFunc name args definition = do
  let rank = length args
  case someNatVal (fromIntegral rank) of
    Just (SomeNat (Proxy :: Proxy n)) ->
      case cmpNat (Proxy @n) (Proxy @10) of
        LTI -> withMany asVar args $ \argsPtr -> SomeFunc <$> defineDynamic @n @d name argsPtr definition
        EQI -> withMany asVar args $ \argsPtr -> SomeFunc <$> defineDynamic @n @d name argsPtr definition
        GTI -> error $ "too many indices: " <> show rank <> "; at most 10-dimensional indices are supported"
    _ -> error $ "failed to lift the value (" <> show rank <> " :: Int) to kind Nat"

defineDynamic
  :: forall n d
   . (HasIndexType n, IsFuncDefinition d)
  => Text
  -> Ptr (CxxVector CxxVar)
  -> d
  -> IO (Func 'FuncTy n d)
defineDynamic (T.encodeUtf8 -> s) x definition =
  withMany withForeignPtr (definitionToExprList definition) $ \v ->
    wrapCxxFunc
      =<< [CU.block| Halide::Func* {
            Halide::Func f{std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}};
            auto const& args = *$(const std::vector<Halide::Var>* x);
            auto const& def = *$(const std::vector<Halide::Expr>* v);
            if (def.size() == 1) {
              f(args) = def.at(0);
            }
            else {
              f(args) = Halide::Tuple{def};
            }
            return new Halide::Func{f};
          } |]

-- | Create an update definition for a Halide function.
--
-- @update f i e@ creates an update definition for @f@ that performs @f[i] = e@.
update
  :: forall n d
   . (HasIndexType n, IsFuncDefinition d)
  => Func 'FuncTy n d
  -> IndexType n
  -> d
  -> IO ()
update func args definition =
  case proveIndexTypeProperties @n of
    Sub Dict -> asVectorOf @((~) (Expr Int32)) asExpr (fromTuple args) $ \index ->
      updateDynamic func index definition

updateSomeFunc
  :: forall d
   . IsFuncDefinition d
  => SomeFunc 'FuncTy d
  -> [Expr Int32]
  -> d
  -> IO ()
updateSomeFunc (SomeFunc func) args definition =
  withMany asExpr args $ \index ->
    updateDynamic func index definition

updateDynamic
  :: forall n d
   . (HasIndexType n, IsFuncDefinition d)
  => Func 'FuncTy n d
  -> Ptr (CxxVector CxxExpr)
  -> d
  -> IO ()
updateDynamic func index definition =
  withFunc func $ \f ->
    withMany withForeignPtr (definitionToExprList definition) $ \value ->
      [C.throwBlock| void {
        handle_halide_exceptions([=](){
          auto& f = *$(Halide::Func* f);
          auto const& index = *$(const std::vector<Halide::Expr>* index);
          auto const& value = *$(const std::vector<Halide::Expr>* value);
          if (value.size() == 1) {
            f(index) = value.at(0);
          }
          else {
            f(index) = Halide::Tuple{value};
          }
        });
      } |]

infix 9 !
infix 9 !!

withExprIndices :: forall n a. HasIndexType n => IndexType n -> (Ptr (CxxVector CxxExpr) -> IO a) -> IO a
withExprIndices indices action =
  case proveIndexTypeProperties @n of
    Sub Dict -> asVectorOf @((~) (Expr Int32)) asExpr (fromTuple indices) $ \x ->
      action x

indexFunc :: forall n a t. HasIndexType n => Func t n a -> Ptr (CxxVector CxxExpr) -> IO [ForeignPtr CxxExpr]
indexFunc func x =
  withFunc func $ \f -> do
    let allocate =
          [CU.block| std::vector<Halide::Expr>* {
            Halide::FuncRef ref = $(Halide::Func* f)->operator()(*$(std::vector<Halide::Expr>* x));
            std::vector<Halide::Expr> v;
            if (ref.size() == 1) {
              v.push_back(static_cast<Halide::Expr>(ref));
            }
            else {
              for (auto i = size_t{0}; i < ref.size(); ++i) {
                v.push_back(ref[i]);
              }
            }
            return new std::vector<Halide::Expr>{std::move(v)};
          } |]
    bracket allocate deleteCxxVector $ \v -> do
      size <- fromIntegral <$> cxxVectorSize v
      forM [0 .. size - 1] $ \i ->
        cxxConstruct $ \ptr ->
          [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
                $(const std::vector<Halide::Expr>* v)->at($(size_t i))} } |]

-- | Apply a Halide function. Conceptually, @f ! i@ is equivalent to @f[i]@, i.e.
-- indexing into a lazy array.
(!) :: (HasIndexType n, IsFuncDefinition a) => Func t n a -> IndexType n -> a
(!) func args =
  unsafePerformIO $
    withExprIndices args (indexFunc func) <&> exprListToDefinition

(!!) :: IsFuncDefinition a => SomeFunc t a -> [Expr Int32] -> a
(!!) (SomeFunc func) args =
  unsafePerformIO $
    withMany asExpr args (indexFunc func) <&> exprListToDefinition

-- | Get a particular dimension of a pipeline parameter.
dim
  :: forall n a
   . (HasCallStack, KnownNat n)
  => Int
  -> Func 'ParamTy n (Expr a)
  -> IO Dimension
dim k func@(Param _)
  | 0 <= k && k < fromIntegral (natVal (Proxy @n)) =
      let n = fromIntegral k
       in withBufferParam func $ \f ->
            wrapCxxDimension
              =<< [CU.exp| Halide::Internal::Dimension* {
                    new Halide::Internal::Dimension{$(Halide::ImageParam* f)->dim($(int n))} } |]
  | otherwise =
      error $
        "invalid dimension index: "
          <> show k
          <> "; Func is "
          <> show (natVal (Proxy @n))
          <> "-dimensional"

-- | Get the loop nests specified by the schedule for this function.
--
-- Helpful for understanding what a schedule is doing.
--
-- For more info, see
-- [@Halide::Func::print_loop_nest@](https://halide-lang.org/docs/class_halide_1_1_func.html#a03f839d9e13cae4b87a540aa618589ae)
prettyLoopNest :: KnownNat n => Func t n r -> IO Text
prettyLoopNest func = withFunc func $ \f ->
  peekAndDeleteCxxString
    =<< [C.throwBlock| std::string* {
          return handle_halide_exceptions([=]() {
            return new std::string{Halide::Internal::print_loop_nest(
              std::vector<Halide::Internal::Function>{$(Halide::Func* f)->function()})};
          });
        } |]

-- | Similar to 'realizeOnTarget' except that the pipeline is run on 'hostTarget'.
realize
  :: forall n a t b
   . (KnownNat n, IsHalideType a)
  => Func t n (Expr a)
  -- ^ Function to evaluate
  -> [Int]
  -- ^ Domain over which to evaluate
  -> (Ptr (HalideBuffer n a) -> IO b)
  -- ^ What to do with the buffer afterwards. Note that the buffer is allocated only temporary,
  -- so do not return it directly.
  -> IO b
realize = realizeOnTarget hostTarget

-- | Evaluate this function over a rectangular domain.
--
-- If your target is a GPU, this function will not automatically copy data back from the GPU.
realizeOnTarget
  :: forall n a t b
   . (KnownNat n, IsHalideType a)
  => Target
  -- ^ Target on which to run the pipeline
  -> Func t n (Expr a)
  -- ^ Function to evaluate
  -> [Int]
  -- ^ Domain over which to evaluate
  -> (Ptr (HalideBuffer n a) -> IO b)
  -- ^ What to do with the buffer afterwards. Note that the buffer is allocated only temporary,
  -- so do not return it directly.
  -> IO b
realizeOnTarget target func shape action =
  withFunc func $ \func' ->
    withCxxTarget target $ \target' ->
      allocaBuffer target shape $ \buf -> do
        let raw = castPtr buf
        [C.throwBlock| void {
          handle_halide_exceptions([=](){
            $(Halide::Func* func')->realize($(halide_buffer_t* raw), *$(const Halide::Target* target'));
          });
        } |]
        action buf

-- | A view pattern to specify the name of a buffer argument.
--
-- Example usage:
--
-- >>> :{
-- _ <- compile $ \(buffer "src" -> src) -> do
--   i <- mkVar "i"
--   define "dest" i $ (src ! i :: Expr Float)
-- :}
--
-- or if we want to specify the dimension and type, we can use type applications:
--
-- >>> :{
-- _ <- compile $ \(buffer @1 @Float "src" -> src) -> do
--   i <- mkVar "i"
--   define "dest" i $ src ! i
-- :}
buffer :: forall n a. (KnownNat n, IsHalideType a) => Text -> Func 'ParamTy n (Expr a) -> Func 'ParamTy n (Expr a)
buffer name p@(Param r) = unsafePerformIO $ do
  _ <- getBufferParameter @n @a (Just name) r
  pure p

-- | Similar to 'buffer', but for scalar parameters.
--
-- Example usage:
--
-- >>> :{
-- _ <- compile $ \(scalar @Float "a" -> a) -> do
--   i <- mkVar "i"
--   define "dest" i $ a
-- :}
scalar :: forall a. IsHalideType a => Text -> Expr a -> Expr a
scalar name (ScalarParam r) = unsafePerformIO $ do
  readIORef r >>= \case
    Just _ -> error "the name of this Expr has already been set"
    Nothing -> do
      fp <- mkScalarParameter @a (Just name)
      writeIORef r (Just fp)
  pure (ScalarParam r)
scalar _ _ = error "cannot set the name of an expression that is not a parameter"

wrapCxxStage :: Ptr CxxStage -> IO (Stage n a)
wrapCxxStage = fmap Stage . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteStage(Halide::Stage* p) { delete p; } |]

withCxxStage :: Stage n a -> (Ptr CxxStage -> IO b) -> IO b
withCxxStage (Stage fp) = withForeignPtr fp

-- | Get the pure stage of a 'Func' for the purposes of scheduling it.
getStage :: KnownNat n => Func t n a -> IO (Stage n a)
getStage func =
  withFunc func $ \func' ->
    [CU.exp| Halide::Stage* { new Halide::Stage{static_cast<Halide::Stage>(*$(Halide::Func* func'))} } |]
      >>= wrapCxxStage

-- | Return 'True' when the function has update definitions, 'False' otherwise.
hasUpdateDefinitions :: KnownNat n => Func t n a -> IO Bool
hasUpdateDefinitions func =
  withFunc func $ \func' ->
    toBool <$> [CU.exp| bool { $(const Halide::Func* func')->has_update_definition() } |]

-- | Get a handle to an update step for the purposes of scheduling it.
getUpdateStage :: KnownNat n => Int -> Func 'FuncTy n a -> IO (Stage n a)
getUpdateStage k func =
  withFunc func $ \func' ->
    let k' = fromIntegral k
     in [CU.exp| Halide::Stage* { new Halide::Stage{$(Halide::Func* func')->update($(int k'))} } |]
          >>= wrapCxxStage

-- | Identify the loop nest corresponding to some dimension of some function.
getLoopLevelAtStage
  :: KnownNat n
  => Func t n a
  -> Expr Int32
  -> Int
  -- ^ update index
  -> IO (LoopLevel 'LockedTy)
getLoopLevelAtStage func var stageIndex =
  withFunc func $ \f -> asVarOrRVar var $ \i -> do
    (SomeLoopLevel level) <-
      wrapCxxLoopLevel
        =<< [C.throwBlock| Halide::LoopLevel* {
              return handle_halide_exceptions([=](){
                return new Halide::LoopLevel{*$(const Halide::Func* f),
                                             *$(const Halide::VarOrRVar* i),
                                             $(int k)};
              });
            } |]
    case level of
      LoopLevel _ -> pure level
      _ -> error $ "getLoopLevelAtStage: got " <> show level <> ", but expected a LoopLevel 'LockedTy"
  where
    k = fromIntegral stageIndex

-- | Same as 'getLoopLevelAtStage' except that the stage is @-1@.
getLoopLevel :: KnownNat n => Func t n a -> Expr Int32 -> IO (LoopLevel 'LockedTy)
getLoopLevel f i = getLoopLevelAtStage f i (-1)

-- | Allocate storage for this function within a particular loop level.
--
-- Scheduling storage is optional, and can be used to separate the loop level at which storage is allocated
-- from the loop level at which computation occurs to trade off between locality and redundant work.
--
-- For more info, see [Halide::Func::store_at](https://halide-lang.org/docs/class_halide_1_1_func.html#a417c08f8aa3a5cdf9146fba948b65193).
storeAt :: KnownNat n => Func 'FuncTy n a -> LoopLevel t -> IO (Func 'FuncTy n a)
storeAt func level = do
  withFunc func $ \f ->
    withCxxLoopLevel level $ \l ->
      [CU.exp| void { $(Halide::Func* f)->store_at(*$(const Halide::LoopLevel* l)) } |]
  pure func

-- | Schedule a function to be computed within the iteration over a given loop level.
--
-- For more info, see [Halide::Func::compute_at](https://halide-lang.org/docs/class_halide_1_1_func.html#a800cbcc3ca5e3d3fa1707f6e1990ec83).
computeAt :: KnownNat n => Func 'FuncTy n a -> LoopLevel t -> IO (Func 'FuncTy n a)
computeAt func level = do
  withFunc func $ \f ->
    withCxxLoopLevel level $ \l ->
      [CU.exp| void { $(Halide::Func* f)->compute_at(*$(const Halide::LoopLevel* l)) } |]
  pure func

-- | Wrap a buffer into a t'Func'.
--
-- Suppose, we are defining a pipeline that adds together two vectors, and we'd like to call 'realize' to
-- evaluate it directly, how do we pass the vectors to the t'Func'? 'asBufferParam' allows to do exactly this.
--
-- > asBuffer [1, 2, 3] $ \a ->
-- >   asBuffer [4, 5, 6] $ \b -> do
-- >     i <- mkVar "i"
-- >     f <- define "vectorAdd" i $ a ! i + b ! i
-- >     realize f [3] $ \result ->
-- >       print =<< peekToList f
asBufferParam
  :: forall n a t b
   . IsHalideBuffer t n a
  => t
  -- ^ Object to treat as a buffer
  -> (Func 'ParamTy n (Expr a) -> IO b)
  -- ^ What to do with the __temporary__ buffer
  -> IO b
asBufferParam arr action =
  withHalideBuffer @n @a arr $ \arr' -> do
    param <- mkBufferParameter @n @a Nothing
    withForeignPtr param $ \param' ->
      let buf = (castPtr arr' :: Ptr RawHalideBuffer)
       in [CU.block| void {
            $(Halide::ImageParam* param')->set(Halide::Buffer<>{*$(const halide_buffer_t* buf)});
          } |]
    action . Param =<< newIORef (Just param)
