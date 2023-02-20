{-# LANGUAGE AllowAmbiguousTypes #-}
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
  ( Func (..)
  , FuncTy (..)
  , buffer
  , scalar
  -- , setName
  , define
  , update
  , (!)
  -- , printLoopNest
  , prettyLoopNest
  , realize1D

    -- * Scheduling
  , TailStrategy (..)
  , vectorize
  , unroll
  , reorder
  , split
  , fuse
  , gpuBlocks
  , gpuBlocks'
  , computeRoot
  , asUsed
  , asUsedBy
  , copyToDevice
  , copyToHost
  , dim
  , estimate
  , bound

    -- * Debugging

    -- * Internal
  , withFunc
  , withBufferParam
  , wrapCxxFunc
  )
where

import Data.IORef
import Data.Kind (Type)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Foreign.ForeignPtr
import Foreign.Marshal (with)
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
import Language.Halide.Target
import Language.Halide.Type
import Language.Halide.Utils
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (min, tail)

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
  -> Func t n a
  -> Expr Int32
  -- ^ Variable to vectorize
  -> Expr Int32
  -- ^ Split factor
  -> IO ()
vectorize strategy func var factor =
  withFunc func $ \f ->
    asVarOrRVar var $ \x ->
      asExpr factor $ \n ->
        [C.throwBlock| void {
          $(Halide::Func* f)->vectorize(*$(Halide::VarOrRVar* x), *$(Halide::Expr* n),
                                        static_cast<Halide::TailStrategy>($(int tail)));
        } |]
  where
    tail = fromIntegral (fromEnum strategy)

-- | Split a dimension by the given factor, then unroll the inner dimension.
--
-- This is how you unroll a loop of unknown size by some constant factor. After
-- this call, @var@ refers to the outer dimension of the split.
unroll
  :: (KnownNat n, IsHalideType a)
  => TailStrategy
  -> Func t n a
  -> Expr Int32
  -- ^ Variable @var@ to vectorize
  -> Expr Int32
  -- ^ Split factor
  -> IO ()
unroll strategy func var factor =
  withFunc func $ \f ->
    asVarOrRVar var $ \x ->
      asExpr factor $ \n ->
        [C.throwBlock| void {
          $(Halide::Func* f)->unroll(*$(Halide::VarOrRVar* x), *$(Halide::Expr* n),
                                     static_cast<Halide::TailStrategy>($(int tail)));
        } |]
  where
    tail = fromIntegral (fromEnum strategy)

-- | Reorder variables to have the given nesting order, from innermost out.
reorder
  :: forall t n a i ts
   . ( IsTuple (Arguments ts) i
     , All ((~) (Expr Int32)) ts
     , Length ts ~ n
     , KnownNat n
     , IsHalideType a
     )
  => Func t n a
  -> i
  -> IO ()
reorder func args =
  asVectorOf @((~) (Expr Int32)) asVarOrRVar (fromTuple args) $ \v -> do
    withFunc func $ \f ->
      [C.throwBlock| void { $(Halide::Func* f)->reorder(*$(std::vector<Halide::VarOrRVar>* v)); } |]

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

-- | Tell Halide that the following dimensions correspond to GPU block indices.
--
-- This is useful for scheduling stages that will run serially within each GPU block.
-- If the selected target is not ptx, this just marks those dimensions as parallel.
gpuBlocks'
  :: ( KnownNat n
     , IsHalideType a
     , IsTuple (Arguments ts) i
     , All ((~) (Expr Int32)) ts
     , Length ts <= 3
     , 1 <= Length ts
     )
  => DeviceAPI
  -> i
  -> Func t n a
  -> IO (Func t n a)
gpuBlocks' deviceApi vars func = do
  withFunc func $ \f ->
    asVectorOf @((~) (Expr Int32)) asVarOrRVar (fromTuple vars) $ \i -> do
      [C.throwBlock| void {
        handle_halide_exceptions([=](){
          auto const& v = *$(std::vector<Halide::VarOrRVar>* i);
          auto& fn = *$(Halide::Func* f);
          auto const device = static_cast<Halide::DeviceAPI>($(int api));
          if (v.size() == 1) {
            fn.gpu_blocks(v.at(0), device);
          }
          else if (v.size() == 2) {
            fn.gpu_blocks(v.at(0), v.at(1), device);
          }
          else if (v.size() == 3) {
            fn.gpu_blocks(v.at(0), v.at(1), v.at(2), device);
          }
          else {
            throw std::runtime_error{"unexpected v.size() in gpuBlocks'"};
          }
        });
      } |]
  pure func
  where
    api = fromIntegral . fromEnum $ deviceApi

-- | Same as 'gpuBlocks'', but uses 'DeviceDefaultGPU'.
--
-- This is useful for scheduling stages that will run serially within each GPU block.
-- If the selected target is not ptx, this just marks those dimensions as parallel.
gpuBlocks
  :: ( KnownNat n
     , IsHalideType a
     , IsTuple (Arguments ts) i
     , All ((~) (Expr Int32)) ts
     , Length ts <= 3
     , 1 <= Length ts
     )
  => i
  -> Func t n a
  -> IO (Func t n a)
gpuBlocks = gpuBlocks' DeviceDefaultGPU

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
split
  :: (KnownNat n, IsHalideType a)
  => TailStrategy
  -- ^ how to treat the remainder
  -> Func t n a
  -> Expr Int32
  -- ^ loop variable to split
  -> Expr Int32
  -- ^ new outer loop variable
  -> Expr Int32
  -- ^ new inner loop variable
  -> Expr Int32
  -- ^ split factor
  -> IO (Func t n a)
split tail func old outer inner factor = do
  withFunc func $ \f ->
    asVarOrRVar old $ \old' ->
      asVarOrRVar outer $ \outer' ->
        asVarOrRVar inner $ \inner' ->
          asExpr factor $ \factor' ->
            [C.throwBlock| void {
              handle_halide_exceptions([=](){
                $(Halide::Func* f)->split(
                  *$(const Halide::VarOrRVar* old'),
                  *$(const Halide::VarOrRVar* outer'),
                  *$(const Halide::VarOrRVar* inner'),
                  *$(const Halide::Expr* factor'),
                  static_cast<Halide::TailStrategy>($(int t)));
              }); } |]
  pure func
  where
    t = fromIntegral . fromEnum $ tail

-- | Join two dimensions into a single fused dimenion.
--
-- The fused dimension covers the product of the extents of the inner and outer dimensions given.
fuse
  :: (KnownNat n, IsHalideType a)
  => Func t n a
  -> Expr Int32
  -- ^ inner loop variable
  -> Expr Int32
  -- ^ outer loop variable
  -> Expr Int32
  -- ^ new fused loop variable
  -> IO (Func t n a)
fuse func outer inner fused = do
  withFunc func $ \f ->
    asVarOrRVar outer $ \outer' ->
      asVarOrRVar inner $ \inner' ->
        asVarOrRVar fused $ \fused' ->
          [CU.exp| void {
                $(Halide::Func* f)->fuse(
                  *$(const Halide::VarOrRVar* outer'),
                  *$(const Halide::VarOrRVar* inner'),
                  *$(const Halide::VarOrRVar* fused')) } |]
  pure func

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

instance (KnownNat n, IsHalideType a) => Named (Func 'ParamTy n a) where
  setName :: Func 'ParamTy n a -> Text -> IO ()
  setName (Param r) name = do
    readIORef r >>= \case
      Just _ -> error "the name of this Func has already been set"
      Nothing -> do
        fp <- mkBufferParameter @n @a (Just name)
        writeIORef r (Just fp)

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

-- | Evaluate this function over a one-dimensional domain and return the
-- resulting buffer or buffers.
realize1D
  :: IsHalideType a
  => Func t 1 a
  -- ^ Function to evaluate
  -> Int
  -- ^ @size@ of the domain. The function will be evaluated on @[0, ..., size -1]@
  -> IO (Vector a)
realize1D func size = do
  buf <- SM.new size
  withHalideBuffer buf $ \x -> do
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
