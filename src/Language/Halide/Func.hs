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
  , buffer
  , scalar
  , define
  , (!)
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
  , hasUpdateDefinitions
  , getUpdateStage

    -- * Debugging
  , prettyLoopNest
  , realize1D

    -- * Internal
  , withFunc
  , withBufferParam
  , wrapCxxFunc
  , CxxStage
  , wrapCxxStage
  , withCxxStage
  )
where

import Control.Exception (bracket)
import Control.Monad (forM)
import Control.Monad.ST (RealWorld)
import Data.IORef
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.ForeignPtr
import Foreign.Marshal (toBool, with)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Stack (HasCallStack)
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Dimension
import Language.Halide.Expr
import Language.Halide.LoopLevel
import Language.Halide.Target
import Language.Halide.Type
import Language.Halide.Utils
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (min, tail)

-- | Haskell counterpart of [Halide::Stage](https://halide-lang.org/docs/class_halide_1_1_stage.html).
data CxxStage

importHalide

-- | A function in Halide. Conceptually, it can be thought of as a lazy
-- @n@-dimensional buffer of type @a@.
--
-- This is a wrapper around @Halide::Func@ C++ type.
data Func (t :: FuncTy) (n :: Nat) (a :: Type) where
  Func :: {-# UNPACK #-} !(ForeignPtr CxxFunc) -> Func 'FuncTy n a
  Param :: {-# UNPACK #-} !(IORef (Maybe (ForeignPtr CxxImageParam))) -> Func 'ParamTy n a

-- | Function type. It can either be 'FuncTy' which means that we have defined the function ourselves,
-- or 'ParamTy' which means that it's a parameter to our pipeline.
data FuncTy = FuncTy | ParamTy
  deriving stock (Show, Eq, Ord)

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
  deriving stock (Eq, Ord, Show)

-- | Either v'Var' or v'RVar'
type VarOrRVar = Expr Int32

-- | Specifies that @i@ is a tuple of @'Expr' Int32@.
--
-- @ts@ are deduced from @i@, so you don't have to specify them explicitly.
type IndexTuple i ts = (IsTuple (Arguments ts) i, All ((~) (Expr Int32)) ts)

-- | Common scheduling functions
class (KnownNat n, IsHalideType a) => Schedulable f n a where
  -- | Vectorize the dimension.
  vectorize :: VarOrRVar -> f n a -> IO (f n a)

  -- | Unroll the dimension.
  unroll :: VarOrRVar -> f n a -> IO (f n a)

  -- | Reorder variables to have the given nesting order, from innermost out.
  reorder :: [VarOrRVar] -> f n a -> IO (f n a)

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

  specialize :: Expr Bool -> f n a -> IO (Stage n a)
  specializeFail :: Text -> f n a -> IO ()
  gpuBlocks :: (IndexTuple i ts, 1 <= Length ts, Length ts <= 3) => DeviceAPI -> i -> f n a -> IO (f n a)
  gpuThreads :: (IndexTuple i ts, 1 <= Length ts, Length ts <= 3) => DeviceAPI -> i -> f n a -> IO (f n a)

  -- | Schedule the iteration over this stage to be fused with another stage from outermost loop to a
  -- given LoopLevel.
  --
  -- For more info, see [Halide::Stage::compute_with](https://halide-lang.org/docs/class_halide_1_1_stage.html#a82a2ae25a009d6a2d52cb407a25f0a5b).
  computeWith :: LoopAlignStrategy -> f n a -> LoopLevel t -> IO ()

instance (KnownNat n, IsHalideType a) => Schedulable Stage n a where
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
  gpuBlocks (fromIntegral . fromEnum -> api) vars stage = do
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
  gpuThreads (fromIntegral . fromEnum -> api) vars stage = do
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
  :: (KnownNat n, IsHalideType b)
  => (a -> Stage n b -> IO (Stage n b))
  -> a
  -> Func t n b
  -> IO (Func t n b)
viaStage1 f a1 func = do
  _ <- f a1 =<< getStage func
  pure func

viaStage2
  :: (KnownNat n, IsHalideType b)
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
  :: (KnownNat n, IsHalideType b)
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

instance (KnownNat n, IsHalideType a) => Schedulable (Func t) n a where
  vectorize = viaStage1 vectorize
  unroll = viaStage1 unroll
  reorder = viaStage1 reorder
  split = viaStage4 split
  fuse = viaStage2 fuse
  serial = viaStage1 serial
  parallel = viaStage1 parallel
  specialize cond func = getStage func >>= specialize cond
  specializeFail msg func = getStage func >>= specializeFail msg
  gpuBlocks = viaStage2 gpuBlocks
  gpuThreads = viaStage2 gpuThreads
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

-- vectorize
--   :: (KnownNat n, IsHalideType a)
--   => TailStrategy
--   -> Func t n a
--   -> Expr Int32
--   -- ^ Variable to vectorize
--   -> Expr Int32
--   -- ^ Split factor
--   -> IO ()
-- vectorize strategy func var factor =
--   withFunc func $ \f ->
--     asVarOrRVar var $ \x ->
--       asExpr factor $ \n ->
--         [C.throwBlock| void {
--           $(Halide::Func* f)->vectorize(*$(Halide::VarOrRVar* x), *$(Halide::Expr* n),
--                                         static_cast<Halide::TailStrategy>($(int tail)));
--         } |]
--   where
--     tail = fromIntegral (fromEnum strategy)

-- | Split a dimension by the given factor, then unroll the inner dimension.
--
-- This is how you unroll a loop of unknown size by some constant factor. After
-- this call, @var@ refers to the outer dimension of the split.
-- unroll
--   :: (KnownNat n, IsHalideType a)
--   => TailStrategy
--   -> Func t n a
--   -> Expr Int32
--   -- ^ Variable @var@ to vectorize
--   -> Expr Int32
--   -- ^ Split factor
--   -> IO ()
-- unroll strategy func var factor =
--   withFunc func $ \f ->
--     asVarOrRVar var $ \x ->
--       asExpr factor $ \n ->
--         [C.throwBlock| void {
--           $(Halide::Func* f)->unroll(*$(Halide::VarOrRVar* x), *$(Halide::Expr* n),
--                                      static_cast<Halide::TailStrategy>($(int tail)));
--         } |]
--   where
--     tail = fromIntegral (fromEnum strategy)

-- | Reorder variables to have the given nesting order, from innermost out.
-- reorder
--   :: forall t n a i ts
--    . ( IsTuple (Arguments ts) i
--      , All ((~) (Expr Int32)) ts
--      , Length ts ~ n
--      , KnownNat n
--      , IsHalideType a
--      )
--   => Func t n a
--   -> i
--   -> IO ()
-- reorder func args =
--   asVectorOf @((~) (Expr Int32)) asVarOrRVar (fromTuple args) $ \v -> do
--     withFunc func $ \f ->
--       [C.throwBlock| void { $(Halide::Func* f)->reorder(*$(std::vector<Halide::VarOrRVar>* v)); } |]

-- | Statically declare the range over which the function will be evaluated in the general case.
--
-- This provides a basis for the auto scheduler to make trade-offs and scheduling decisions.
-- The auto generated schedules might break when the sizes of the dimensions are very different from the
-- estimates specified. These estimates are used only by the auto scheduler if the function is a pipeline output.
estimate
  :: (KnownNat n, IsHalideType a)
  => Expr Int32
  -- ^ index variable
  -> Expr Int32
  -- ^ @min@ estimate
  -> Expr Int32
  -- ^ @extent@ estimate
  -> Func t n a
  -> IO ()
estimate var min extent func =
  withFunc func $ \f -> asVar var $ \i -> asExpr min $ \minExpr -> asExpr extent $ \extentExpr ->
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
  :: (KnownNat n, IsHalideType a)
  => Expr Int32
  -- ^ index variable
  -> Expr Int32
  -- ^ @min@ estimate
  -> Expr Int32
  -- ^ @extent@ estimate
  -> Func t n a
  -> IO ()
bound var min extent func =
  withFunc func $ \f -> asVar var $ \i -> asExpr min $ \minExpr -> asExpr extent $ \extentExpr ->
    [CU.exp| void {
      $(Halide::Func* f)->bound(
        *$(Halide::Var* i), *$(Halide::Expr* minExpr), *$(Halide::Expr* extentExpr)) } |]

getArgs :: (KnownNat n, IsHalideType a) => Func t n a -> IO [Expr Int32]
getArgs func =
  withFunc func $ \func' -> do
    let allocate =
          [CU.exp| std::vector<Halide::Var>* { 
            new std::vector<Halide::Var>{$(const Halide::Func* func')->args()} } |]
        destroy v = [CU.exp| void { delete $(std::vector<Halide::Var>* v) } |]
    bracket allocate destroy $ \v -> do
      n <- [CU.exp| size_t { $(const std::vector<Halide::Var>* v)->size() } |]
      forM [0 .. n - 1] $ \i ->
        wrapCxxVar
          =<< [CU.exp| Halide::Var* { 
                new Halide::Var{$(const std::vector<Halide::Var>* v)->at($(size_t i))} } |]

-- deepCopy :: (KnownNat n, IsHalideType a) => Func 'FuncTy n a -> IO (Func 'FuncTy n a)
-- deepCopy func = withFunc func $ \func' ->
--  wrapCxxFunc
--    =<< [C.throwBlock| Halide::Func* {
--          return handle_halide_exceptions([=](){
--            using namespace Halide;
--            using namespace Halide::Internal;
--            auto const& original = *$(const Halide::Func* func');
--            auto wrapper = Func{original.name() + "_wrapper"};
--            std::map<FunctionPtr, FunctionPtr> remapping;
--            original.function().deep_copy(wrapper.name(), wrapper.function().get_contents(), remapping);
--            // TODO: I don't quite understand this part... it's copy-pasted from src/Func.cpp in Halide
--            // Fix up any self-references in the clone.
--            FunctionPtr self_reference = wrapper.function().get_contents();
--            self_reference.weaken();
--            remapping.emplace(original.function().get_contents(), self_reference);
--            wrapper.function().substitute_calls(remapping);
--            return new Func{std::move(wrapper)};
--          });
--        } |]

-- | Tell Halide that the following dimensions correspond to GPU block indices.
--
-- This is useful for scheduling stages that will run serially within each GPU block.
-- If the selected target is not ptx, this just marks those dimensions as parallel.
-- gpuBlocks'
--   :: ( KnownNat n
--      , IsHalideType a
--      , IsTuple (Arguments ts) i
--      , All ((~) (Expr Int32)) ts
--      , Length ts <= 3
--      , 1 <= Length ts
--      )
--   => DeviceAPI
--   -> i
--   -> Func t n a
--   -> IO (Func t n a)
-- gpuBlocks' deviceApi vars func = do
--   withFunc func $ \f ->
--     asVectorOf @((~) (Expr Int32)) asVarOrRVar (fromTuple vars) $ \i -> do
--       [C.throwBlock| void {
--         handle_halide_exceptions([=](){
--           auto const& v = *$(std::vector<Halide::VarOrRVar>* i);
--           auto& fn = *$(Halide::Func* f);
--           auto const device = static_cast<Halide::DeviceAPI>($(int api));
--           if (v.size() == 1) {
--             fn.gpu_blocks(v.at(0), device);
--           }
--           else if (v.size() == 2) {
--             fn.gpu_blocks(v.at(0), v.at(1), device);
--           }
--           else if (v.size() == 3) {
--             fn.gpu_blocks(v.at(0), v.at(1), v.at(2), device);
--           }
--           else {
--             throw std::runtime_error{"unexpected v.size() in gpuBlocks'"};
--           }
--         });
--       } |]
--   pure func
--   where
--     api = fromIntegral . fromEnum $ deviceApi

-- | Same as 'gpuBlocks'', but uses 'DeviceDefaultGPU'.
--
-- This is useful for scheduling stages that will run serially within each GPU block.
-- If the selected target is not ptx, this just marks those dimensions as parallel.
-- gpuBlocks
--   :: ( KnownNat n
--      , IsHalideType a
--      , IsTuple (Arguments ts) i
--      , All ((~) (Expr Int32)) ts
--      , Length ts <= 3
--      , 1 <= Length ts
--      )
--   => i
--   -> Func t n a
--   -> IO (Func t n a)
-- gpuBlocks = gpuBlocks' DeviceDefaultGPU

-- | Compute all of this function once ahead of time.
--
-- See [Halide::Func::compute_root](https://halide-lang.org/docs/class_halide_1_1_func.html#a29df45a4a16a63eb81407261a9783060) for more info.
computeRoot :: (KnownNat n, IsHalideType a) => Func t n a -> IO (Func t n a)
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
  :: (KnownNat n, KnownNat m, IsHalideType a, IsHalideType b)
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
asUsed :: (KnownNat n, IsHalideType a) => Func t n a -> IO (Func 'FuncTy n a)
asUsed f =
  withFunc f $ \fPtr ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* { new Halide::Func{$(Halide::Func* fPtr)->in()} } |]

-- | Declare that this function should be implemented by a call to @halide_buffer_copy@ with the given
-- target device API.
--
-- Asserts that the Func has a pure definition which is a simple call to a single input, and no update
-- definitions. The wrapper Funcs returned by in() are suitable candidates. Consumes all pure variables,
-- and rewrites the Func to have an extern definition that calls halide_buffer_copy.
copyToDevice :: (KnownNat n, IsHalideType a) => DeviceAPI -> Func t n a -> IO (Func t n a)
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

-- | Same as 'copyToDevice DeviceHost'
copyToHost :: (KnownNat n, IsHalideType a) => Func t n a -> IO (Func t n a)
copyToHost = copyToDevice DeviceHost

-- | Split a dimension into inner and outer subdimensions with the given names, where the inner dimension
-- iterates from @0@ to @factor-1@.
--
-- The inner and outer subdimensions can then be dealt with using the other scheduling calls. It's okay
-- to reuse the old variable name as either the inner or outer variable. The first argument specifies
-- how the tail should be handled if the split factor does not provably divide the extent.
-- split
--   :: (KnownNat n, IsHalideType a)
--   => TailStrategy
--   -- ^ how to treat the remainder
--   -> Func t n a
--   -> Expr Int32
--   -- ^ loop variable to split
--   -> Expr Int32
--   -- ^ new outer loop variable
--   -> Expr Int32
--   -- ^ new inner loop variable
--   -> Expr Int32
--   -- ^ split factor
--   -> IO (Func t n a)
-- split tail func old outer inner factor = do
--   withFunc func $ \f ->
--     asVarOrRVar old $ \old' ->
--       asVarOrRVar outer $ \outer' ->
--         asVarOrRVar inner $ \inner' ->
--           asExpr factor $ \factor' ->
--             [C.throwBlock| void {
--               handle_halide_exceptions([=](){
--                 $(Halide::Func* f)->split(
--                   *$(const Halide::VarOrRVar* old'),
--                   *$(const Halide::VarOrRVar* outer'),
--                   *$(const Halide::VarOrRVar* inner'),
--                   *$(const Halide::Expr* factor'),
--                   static_cast<Halide::TailStrategy>($(int t)));
--               }); } |]
--   pure func
--   where
--     t = fromIntegral . fromEnum $ tail

-- | Join two dimensions into a single fused dimenion.
--
-- The fused dimension covers the product of the extents of the inner and outer dimensions given.
-- fuse
--   :: (KnownNat n, IsHalideType a)
--   => Func t n a
--   -> Expr Int32
--   -- ^ inner loop variable
--   -> Expr Int32
--   -- ^ outer loop variable
--   -> Expr Int32
--   -- ^ new fused loop variable
--   -> IO (Func t n a)
-- fuse func outer inner fused = do
--   withFunc func $ \f ->
--     asVarOrRVar outer $ \outer' ->
--       asVarOrRVar inner $ \inner' ->
--         asVarOrRVar fused $ \fused' ->
--           [CU.exp| void {
--                 $(Halide::Func* f)->fuse(
--                   *$(const Halide::VarOrRVar* outer'),
--                   *$(const Halide::VarOrRVar* inner'),
--                   *$(const Halide::VarOrRVar* fused')) } |]
--   pure func

-- withVarOrRVarMany :: [Expr Int32] -> (Int -> Ptr (CxxVector CxxVarOrRVar) -> IO a) -> IO a
-- withVarOrRVarMany xs f =
--   bracket allocate destroy $ \v -> do
--     let go !k [] = f k v
--         go !k (y : ys) = withVarOrRVarMany y $ \p -> do
--           [CU.exp| void { $(std::vector<Halide::Expr>* v)->push_back(*$(Halide::VarOrRVar* p)) } |]
--           go (k + 1) ys
--     go 0 xs
--   where
--     count = fromIntegral (length xs)

--   withFunc func $ \f ->
--     withVarOrRVarMany vars $ \count v -> do
--       unless natVal (Proxy @n)
--       handleHalideExceptionsM
--         [C.tryBlock| void {
--           $(Halide::Func* f)->reorder(*$(std::vector<Halide::VarOrRVar>* v));
--         } |]
--
-- class Curry (args :: [Type]) (r :: Type) (f :: Type) | args r -> f where
--   curryG :: (Arguments args -> r) -> f

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
  => Func 'ParamTy n a
  -> (Ptr CxxImageParam -> IO b)
  -> IO b
withBufferParam (Param r) action =
  getBufferParameter @n @a Nothing r >>= flip withForeignPtr action

-- instance (KnownNat n, IsHalideType a) => Named (Func 'ParamTy n a) where
--   setName :: Func 'ParamTy n a -> Text -> IO ()
--   setName (Param r) name = do
--     readIORef r >>= \case
--       Just _ -> error "the name of this Func has already been set"
--       Nothing -> do
--         fp <- mkBufferParameter @n @a (Just name)
--         writeIORef r (Just fp)

-- | Get the underlying pointer to @Halide::Func@ and invoke an 'IO' action with it.
withFunc :: (KnownNat n, IsHalideType a) => Func t n a -> (Ptr CxxFunc -> IO b) -> IO b
withFunc f = withForeignPtr (funcToForeignPtr f)

wrapCxxFunc :: Ptr CxxFunc -> IO (Func 'FuncTy n a)
wrapCxxFunc = fmap Func . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteFunc(Halide::Func *x) { delete x; } |]

forceFunc :: forall t n a. (KnownNat n, IsHalideType a) => Func t n a -> IO (Func 'FuncTy n a)
forceFunc x@(Func _) = pure x
forceFunc (Param r) = do
  fp <- getBufferParameter @n @a Nothing r
  withForeignPtr fp $ \p ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* {
            new Halide::Func{static_cast<Halide::Func>(*$(Halide::ImageParam* p))} } |]

funcToForeignPtr :: (KnownNat n, IsHalideType a) => Func t n a -> ForeignPtr CxxFunc
funcToForeignPtr x = unsafePerformIO $! forceFunc x >>= \(Func fp) -> pure fp

-- applyFunc :: IsHalideType a => ForeignPtr CxxFunc -> [ForeignPtr CxxExpr] -> IO (Expr a)
-- applyFunc func args =
--   withForeignPtr func $ \f ->
--     withExprMany args $ \v ->
--       wrapCxxExpr
--         =<< [CU.exp| Halide::Expr* {
--               new Halide::Expr{(*$(Halide::Func* f))(*$(std::vector<Halide::Expr>* v))} } |]

-- defineFunc :: Text -> [ForeignPtr CxxExpr] -> ForeignPtr CxxExpr -> IO (ForeignPtr CxxFunc)
-- defineFunc name args expr = do
--   let s = T.encodeUtf8 name
--   withExprMany args $ \x ->
--     withForeignPtr expr $ \y ->
--       newForeignPtr deleteCxxFunc
--         =<< [CU.block| Halide::Func* {
--               Halide::Func f{std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}};
--               f(*$(std::vector<Halide::Expr>* x)) = *$(Halide::Expr* y);
--               return new Halide::Func{f};
--             } |]
--
-- updateFunc
--   :: ForeignPtr CxxFunc
--   -> [ForeignPtr CxxExpr]
--   -> ForeignPtr CxxExpr
--   -> IO ()
-- updateFunc func args expr = do
--   withForeignPtr func $ \f ->
--     withExprMany args $ \x ->
--       withForeignPtr expr $ \y ->
--         [CU.block| void {
--           $(Halide::Func* f)->operator()(*$(std::vector<Halide::Expr>* x)) = *$(Halide::Expr* y);
--         } |]

-- withVarOrRVarMany :: [Expr Int32] -> (Int -> Ptr (CxxVector CxxVarOrRVar) -> IO a) -> IO a
-- withVarOrRVarMany xs f =
--   bracket allocate destroy $ \v -> do
--     let go !k [] = f k v
--         go !k (y : ys) = withVarOrRVarMany y $ \p -> do
--           [CU.exp| void { $(std::vector<Halide::Expr>* v)->push_back(*$(Halide::VarOrRVar* p)) } |]
--           go (k + 1) ys
--     go 0 xs
--   where
--     count = fromIntegral (length xs)
--     allocate =
--       [CU.block| std::vector<Halide::VarOrRVar>* {
--         auto v = new std::vector<Halide::VarOrRVar>{};
--         v->reserve($(size_t count));
--         return v;
--       } |]
--     destroy v = [CU.exp| void { delete $(std::vector<Halide::VarOrRVar>* v) } |]

-- withExprMany :: [ForeignPtr CxxExpr] -> (Ptr (CxxVector CxxExpr) -> IO a) -> IO a
-- withExprMany xs f = do
--   let count = fromIntegral (length xs)
--       allocate =
--         [CU.block| std::vector<Halide::Expr>* {
--           auto v = new std::vector<Halide::Expr>{};
--           v->reserve($(size_t count));
--           return v;
--         } |]
--       destroy v = do
--         [CU.exp| void { delete $(std::vector<Halide::Expr>* v) } |]
--         forM_ xs touchForeignPtr
--   bracket allocate destroy $ \v -> do
--     forM_ xs $ \fp ->
--       let p = unsafeForeignPtrToPtr fp
--        in [CU.exp| void { $(std::vector<Halide::Expr>* v)->push_back(*$(Halide::Expr* p)) } |]
--     f v

-- | Specifies that a type can be used as an index to a Halide function.
-- class ValidIndex (a :: Type) (n :: Nat) | a -> n, n -> a where
--   toExprList :: a -> [ForeignPtr CxxExpr]
--
-- instance ValidIndex (Expr Int32) 1 where
--   toExprList a = [exprToForeignPtr a]
--
-- instance ValidIndex (Expr Int32, Expr Int32) 2 where
--   toExprList (a, b) = [exprToForeignPtr a, exprToForeignPtr b]
--
-- instance ValidIndex (Expr Int32, Expr Int32, Expr Int32) 3 where
--   toExprList (a1, a2, a3) = exprToForeignPtr <$> [a1, a2, a3]
--
-- instance ValidIndex (Expr Int32, Expr Int32, Expr Int32, Expr Int32) 4 where
--   toExprList (a1, a2, a3, a4) = exprToForeignPtr <$> [a1, a2, a3, a4]
--
-- instance ValidIndex (Expr Int32, Expr Int32, Expr Int32, Expr Int32, Expr Int32) 5 where
--   toExprList (a1, a2, a3, a4, a5) = exprToForeignPtr <$> [a1, a2, a3, a4, a5]

-- | Define a Halide function.
--
-- @define "f" i e@ defines a Halide function called "f" such that @f[i] = e@.
-- define :: (ValidIndex i n, IsHalideType a) => Text -> i -> Expr a -> IO (Func n a)
-- define name x y = Func <$> defineFunc name (toExprList x) (exprToForeignPtr y)
define
  :: ( IsTuple (Arguments ts) i
     , All ((~) (Expr Int32)) ts
     , Length ts ~ n
     , KnownNat n
     , IsHalideType a
     )
  => Text
  -> i
  -> Expr a
  -> IO (Func 'FuncTy n a)
define name args expr =
  asVectorOf @((~) (Expr Int32)) asVar (fromTuple args) $ \x -> do
    let s = T.encodeUtf8 name
    asExpr expr $ \y ->
      wrapCxxFunc
        =<< [CU.block| Halide::Func* {
              Halide::Func f{std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)}};
              f(*$(std::vector<Halide::Var>* x)) = *$(Halide::Expr* y);
              return new Halide::Func{f};
            } |]

-- | Create an update definition for a Halide function.
--
-- @update f i e@ creates an update definition for @f@ that performs @f[i] = e@.
-- update :: (ValidIndex i n, KnownNat n, IsHalideType a) => Func n a -> i -> Expr a -> IO ()
-- update func x y = updateFunc (funcToForeignPtr func) (toExprList x) (exprToForeignPtr y)
update
  :: ( IsTuple (Arguments ts) i
     , All ((~) (Expr Int32)) ts
     , Length ts ~ n
     , KnownNat n
     , IsHalideType a
     )
  => Func 'FuncTy n a
  -> i
  -> Expr a
  -> IO ()
update func args expr =
  withFunc func $ \f ->
    asVectorOf @((~) (Expr Int32)) asExpr (fromTuple args) $ \x ->
      asExpr expr $ \y ->
        [CU.block| void {
          $(Halide::Func* f)->operator()(*$(std::vector<Halide::Expr>* x)) = *$(Halide::Expr* y);
        } |]

infix 9 !

-- | Apply a Halide function. Conceptually, @f ! i@ is equivalent to @f[i]@, i.e.
-- indexing into a lazy array.
-- (!) :: (ValidIndex i n, KnownNat n, IsHalideType r) => Func n r -> i -> Expr r
-- (!) func args = unsafePerformIO $ applyFunc (funcToForeignPtr func) (toExprList args)
(!)
  :: ( IsTuple (Arguments ts) i
     , All ((~) (Expr Int32)) ts
     , Length ts ~ n
     , KnownNat n
     , IsHalideType a
     )
  => Func t n a
  -> i
  -> Expr a
(!) func args =
  unsafePerformIO $
    withFunc func $ \f ->
      asVectorOf @((~) (Expr Int32)) asExpr (fromTuple args) $ \x ->
        wrapCxxExpr
          =<< [CU.exp| Halide::Expr* {
            new Halide::Expr{$(Halide::Func* f)->operator()(*$(std::vector<Halide::Expr>* x))}
          } |]

-- | Get a particular dimension of a pipeline parameter.
dim
  :: forall n a
   . (HasCallStack, KnownNat n, IsHalideType a)
  => Int
  -> Func 'ParamTy n a
  -> IO Dimension
dim k func
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

-- | Write out the loop nests specified by the schedule for this function.
--
-- Helpful for understanding what a schedule is doing.
--
-- For more info, see
-- [@Halide::Func::print_loop_nest@](https://halide-lang.org/docs/class_halide_1_1_func.html#a03f839d9e13cae4b87a540aa618589ae)
-- printLoopNest :: (KnownNat n, IsHalideType r) => Func n r -> IO ()
-- printLoopNest func = withFunc func $ \f ->
--   [C.exp| void { $(Halide::Func* f)->print_loop_nest() } |]

-- | Get the loop nests specified by the schedule for this function.
--
-- Helpful for understanding what a schedule is doing.
--
-- For more info, see
-- [@Halide::Func::print_loop_nest@](https://halide-lang.org/docs/class_halide_1_1_func.html#a03f839d9e13cae4b87a540aa618589ae)
prettyLoopNest :: (KnownNat n, IsHalideType r) => Func t n r -> IO Text
prettyLoopNest func = withFunc func $ \f ->
  peekAndDeleteCxxString
    =<< [C.throwBlock| std::string* {
          return handle_halide_exceptions([=]() {
            return new std::string{Halide::Internal::print_loop_nest(
              std::vector<Halide::Internal::Function>{$(Halide::Func* f)->function()})};
          });
        } |]

-- | Evaluate this function over a rectangular domain.
realize
  :: forall n a t b
   . (KnownNat n, IsHalideType a)
  => Func t n a
  -- ^ Function to evaluate
  -> [Int]
  -- ^ Domain over which to evaluate
  -> (Ptr (HalideBuffer n a) -> IO b)
  -- ^ What to do with the buffer afterwards. Note that the buffer is allocated only temporary,
  -- so do not return it directly.
  -> IO b
realize func shape action =
  withFunc func $ \f ->
    allocaCpuBuffer shape $ \buf -> do
      let raw = castPtr buf
      [C.throwBlock| void {
        handle_halide_exceptions([=](){
          $(Halide::Func* f)->realize(
            Halide::Pipeline::RealizationArg{$(halide_buffer_t* raw)});
        });
      } |]
      action buf

-- | Evaluate this function over a one-dimensional domain and return the
-- resulting buffer or buffers.
realize1D
  :: forall a t
   . IsHalideType a
  => Int
  -- ^ @size@ of the domain. The function will be evaluated on @[0, ..., size -1]@
  -> Func t 1 a
  -- ^ Function to evaluate
  -> IO (Vector a)
realize1D size func = do
  buf <- SM.new size
  withHalideBuffer @_ @1 @a buf $ \x -> do
    let b = castPtr x
    withFunc func $ \f ->
      [CU.exp| void {
        $(Halide::Func* f)->realize(
          Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)}) } |]
  S.unsafeFreeze buf

-- | A view pattern to specify the name of a buffer argument.
--
-- Example usage:
--
-- > mkKernel $ \(buffer "src" -> src) -> do
-- >   i <- mkVar "i"
-- >   define "dest" i $ src ! i
--
-- or if we want to specify the dimension and type, we can use type applications:
--
-- > mkKernel $ \(buffer @1 @Float "src" -> src) -> do
-- >   i <- mkVar "i"
-- >   define "dest" $ src ! i
buffer :: forall n a. (KnownNat n, IsHalideType a) => Text -> Func 'ParamTy n a -> Func 'ParamTy n a
buffer name p@(Param r) = unsafePerformIO $ do
  _ <- getBufferParameter @n @a (Just name) r
  pure p

-- | Similar to 'buffer', but for scalar parameters.
--
-- Example usage:
--
-- > mkKernel $ \(buffer "a" -> a) -> do
-- >   i <- mkVar "i"
-- >   define "dest" i $ a
scalar :: forall a. IsHalideType a => Text -> Expr a -> Expr a
scalar name p = unsafePerformIO $ do
  setName p name
  pure p

wrapCxxStage :: (KnownNat n, IsHalideType a) => Ptr CxxStage -> IO (Stage n a)
wrapCxxStage = fmap Stage . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteStage(Halide::Stage* p) { delete p; } |]

withCxxStage :: (KnownNat n, IsHalideType a) => Stage n a -> (Ptr CxxStage -> IO b) -> IO b
withCxxStage (Stage fp) = withForeignPtr fp

getStage :: (KnownNat n, IsHalideType a) => Func t n a -> IO (Stage n a)
getStage func =
  withFunc func $ \func' ->
    [CU.exp| Halide::Stage* { new Halide::Stage{static_cast<Halide::Stage>(*$(Halide::Func* func'))} } |]
      >>= wrapCxxStage

hasUpdateDefinitions :: (KnownNat n, IsHalideType a) => Func t n a -> IO Bool
hasUpdateDefinitions func =
  withFunc func $ \func' ->
    toBool <$> [CU.exp| bool { $(const Halide::Func* func')->has_update_definition() } |]

getUpdateStage :: (KnownNat n, IsHalideType a) => Int -> Func 'FuncTy n a -> IO (Stage n a)
getUpdateStage k func =
  withFunc func $ \func' ->
    let k' = fromIntegral k
     in [CU.exp| Halide::Stage* { new Halide::Stage{$(Halide::Func* func')->update($(int k'))} } |]
          >>= wrapCxxStage

-- | Identify the loop nest corresponding to some dimension of some function.
getLoopLevelAtStage
  :: (KnownNat n, IsHalideType a)
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
getLoopLevel :: (KnownNat n, IsHalideType a) => Func t n a -> Expr Int32 -> IO (LoopLevel 'LockedTy)
getLoopLevel f i = getLoopLevelAtStage f i (-1)

-- | Allocate storage for this function within a particular loop level.
--
-- Scheduling storage is optional, and can be used to separate the loop level at which storage is allocated
-- from the loop level at which computation occurs to trade off between locality and redundant work.
--
-- For more info, see [Halide::Func::store_at](https://halide-lang.org/docs/class_halide_1_1_func.html#a417c08f8aa3a5cdf9146fba948b65193).
storeAt :: (KnownNat n, IsHalideType a) => Func 'FuncTy n a -> LoopLevel t -> IO (Func 'FuncTy n a)
storeAt func level = do
  withFunc func $ \f ->
    withCxxLoopLevel level $ \l ->
      [CU.exp| void { $(Halide::Func* f)->store_at(*$(const Halide::LoopLevel* l)) } |]
  pure func

-- | Schedule a function to be computed within the iteration over a given loop level.
--
-- For more info, see [Halide::Func::compute_at](https://halide-lang.org/docs/class_halide_1_1_func.html#a800cbcc3ca5e3d3fa1707f6e1990ec83).
computeAt :: (KnownNat n, IsHalideType a) => Func 'FuncTy n a -> LoopLevel t -> IO (Func 'FuncTy n a)
computeAt func level = do
  withFunc func $ \f ->
    withCxxLoopLevel level $ \l ->
      [CU.exp| void { $(Halide::Func* f)->compute_at(*$(const Halide::LoopLevel* l)) } |]
  pure func
