{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Language.Halide.Kernel
-- Description : Compiling functions to kernels
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Kernel
  ( compile
  , compileForTarget
  , compileToCallable
  , compileToLoweredStmt
  , StmtOutputFormat (..)
  , LoweredSignature
  )
where

import Control.Exception (bracket)
import Control.Monad.Primitive (touch)
import Control.Monad.ST (RealWorld)
import Data.IORef
import Data.Kind (Type)
import Data.Primitive.PrimArray (MutablePrimArray)
import Data.Primitive.PrimArray qualified as P
import Data.Primitive.Ptr qualified as P
import Data.Proxy
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.IO qualified as T
import Foreign.C.Types (CUIntPtr (..))
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable
import GHC.TypeNats
import Language.C.Inline qualified as C
import Language.C.Inline.Cpp.Exception qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Func
import Language.Halide.RedundantConstraints
import Language.Halide.Target
import Language.Halide.Type
import System.IO.Temp (withSystemTempDirectory)

-- | Haskell counterpart of @Halide::Argument@.
data CxxArgument

importHalide

instanceHasCxxVector "Halide::Argument"

data ArgvStorage s
  = ArgvStorage
      {-# UNPACK #-} !(MutablePrimArray s (Ptr ()))
      {-# UNPACK #-} !(MutablePrimArray s CUIntPtr)

newArgvStorage :: Int -> IO (ArgvStorage RealWorld)
newArgvStorage n = ArgvStorage <$> P.newPinnedPrimArray n <*> P.newPinnedPrimArray n

setArgvStorage
  :: All ValidArgument args
  => ArgvStorage RealWorld
  -> Ptr CxxUserContext
  -> Arguments args
  -> IO ()
setArgvStorage (ArgvStorage argv scalarStorage) context inputs = do
  let argvPtr = P.mutablePrimArrayContents argv
      scalarStoragePtr = P.mutablePrimArrayContents scalarStorage
      go :: All ValidArgument args' => Int -> Arguments args' -> IO ()
      go !_ Nil = pure ()
      go !i (x ::: xs) = do
        fillSlot
          (castPtr $ argvPtr `P.advancePtr` i)
          (castPtr $ scalarStoragePtr `P.advancePtr` i)
          x
        go (i + 1) xs
  go 0 (context ::: inputs)
  touch argv
  touch scalarStorage

-- | Specifies that the type can be used as an argument to a kernel.
class ValidArgument (t :: Type) where
  fillSlot :: Ptr () -> Ptr () -> t -> IO ()

instance IsHalideType t => ValidArgument t where
  fillSlot :: Ptr () -> Ptr () -> t -> IO ()
  fillSlot argv scalarStorage x = do
    poke (castPtr scalarStorage :: Ptr t) x
    poke (castPtr argv :: Ptr (Ptr ())) scalarStorage
  {-# INLINE fillSlot #-}

instance {-# OVERLAPPING #-} ValidArgument (Ptr CxxUserContext) where
  fillSlot :: Ptr () -> Ptr () -> Ptr CxxUserContext -> IO ()
  fillSlot argv scalarStorage x = do
    poke (castPtr scalarStorage :: Ptr (Ptr CxxUserContext)) x
    poke (castPtr argv :: Ptr (Ptr ())) scalarStorage
  {-# INLINE fillSlot #-}

instance {-# OVERLAPPING #-} ValidArgument (Ptr (HalideBuffer n a)) where
  fillSlot :: Ptr () -> Ptr () -> Ptr (HalideBuffer n a) -> IO ()
  fillSlot argv _ x = do
    poke (castPtr argv :: Ptr (Ptr (HalideBuffer n a))) x
  {-# INLINE fillSlot #-}

-- class ValidArgument (Lowered t) => ValidParameter (t :: Type) where
--   appendToArgList :: Ptr (CxxVector CxxArgument) -> t -> IO ()
--   prepareParameter :: IO t

-- instance IsHalideType a => ValidParameter (Expr a) where
--   appendToArgList :: Ptr (CxxVector CxxArgument) -> Expr a -> IO ()
--   appendToArgList v expr =
--     asScalarParam expr $ \p ->
--       [CU.exp| void { $(std::vector<Halide::Argument>* v)->emplace_back(
--         $(Halide::Internal::Parameter const* p)->name(),
--         Halide::Argument::InputScalar,
--         $(Halide::Internal::Parameter const* p)->type(),
--         $(Halide::Internal::Parameter const* p)->dimensions(),
--         $(Halide::Internal::Parameter const* p)->get_argument_estimates()) } |]
--   prepareParameter :: IO (Expr a)
--   prepareParameter = ScalarParam <$> newIORef Nothing

-- instance (KnownNat n, IsHalideType a, t ~ 'ParamTy) => ValidParameter (Func t n (Expr a)) where
--   appendToArgList :: Ptr (CxxVector CxxArgument) -> Func 'ParamTy n (Expr a) -> IO ()
--   appendToArgList v func@(Param _) =
--     withBufferParam func $ \p ->
--       [CU.exp| void { $(std::vector<Halide::Argument>* v)->push_back(
--         *$(Halide::ImageParam const* p)) } |]
--   prepareParameter = Param <$> newIORef Nothing

class KnownNat n => FuncBuilder f (n :: Nat) (a :: Type) | f -> n a where
  buildFunc :: Ptr (CxxVector CxxArgument) -> f -> IO (Func 'FuncTy n a)

instance (k ~ 'ParamTy, KnownNat m, IsHalideType t, FuncBuilder r n a) => FuncBuilder (Func k m (Expr t) -> r) n a where
  buildFunc v f = do
    param <- Param <$> newIORef Nothing
    func <- buildFunc v (f param)
    withBufferParam param $ \p ->
      [CU.exp| void { $(std::vector<Halide::Argument>* v)->push_back(*$(Halide::ImageParam const* p)) } |]
    pure func

instance (IsHalideType t, FuncBuilder r n a) => FuncBuilder (Expr t -> r) n a where
  buildFunc v f = do
    param <- ScalarParam <$> newIORef Nothing
    func <- buildFunc v (f param)
    asScalarParam param $ \p ->
      [CU.block| void {
        auto const& p = *$(Halide::Internal::Parameter const* p);
        $(std::vector<Halide::Argument>* v)->emplace_back(
          p.name(),
          Halide::Argument::InputScalar,
          p.type(),
          p.dimensions(),
          p.get_argument_estimates());
      } |]
    pure func

instance (KnownNat n, t ~ 'FuncTy, n' ~ n, a' ~ a) => FuncBuilder (IO (Func t n' a')) n a where
  buildFunc _ action = action

type family LoweredSignature f where
  LoweredSignature (Expr a -> r) = a -> LoweredSignature r
  LoweredSignature (Func t n (Expr a) -> r) = Ptr (HalideBuffer n a) -> LoweredSignature r
  LoweredSignature (IO (Func t n (Expr a))) = Ptr (HalideBuffer n a) -> IO ()
  LoweredSignature (IO (Func t n (Expr a1, Expr a2))) = Ptr (HalideBuffer n a1) -> Ptr (HalideBuffer n a2) -> IO ()

type IsHalideKernel f = (KnownNat (Length (FunctionArguments f)), All ValidArgument (FunctionArguments f), Curry (FunctionArguments f) (IO ()) f)

newtype Callable (signature :: Type) = Callable (ForeignPtr CxxCallable)

compileToCallable
  :: forall n a f
   . (FuncBuilder f n a, IsHalideKernel (LoweredSignature f))
  => Target
  -> f
  -> IO (Callable (LoweredSignature f))
compileToCallable target builder =
  bracket (newCxxVector Nothing) deleteCxxVector $ \v -> do
    func <- buildFunc @f @n @a v builder
    withCxxFunc func $ \func' ->
      withCxxTarget target $ \target' ->
        wrapCxxCallable
          =<< [C.throwBlock| Halide::Callable* {
                return handle_halide_exceptions([=]() {
                  auto& func = *$(Halide::Func* func');
                  auto& args = *$(std::vector<Halide::Argument>* v);
                  auto const& target = *$(const Halide::Target* target');
                  std::reverse(std::begin(args), std::end(args));
                  return new Halide::Callable{func.compile_to_callable(args, target)};
                });
              } |]
  where
    _ = keepRedundantConstraint @(IsHalideKernel (LoweredSignature f))

callableToFunction :: forall f. IsHalideKernel f => Callable f -> IO f
callableToFunction (Callable callable) = do
  context <- newEmptyCxxUserContext
  -- +1 comes from CxxUserContext
  let argc = 1 + fromIntegral (natVal (Proxy @(Length (FunctionArguments f))))
  storage@(ArgvStorage argv scalarStorage) <- newArgvStorage (fromIntegral argc)
  let argvPtr = P.mutablePrimArrayContents argv
      contextPtr = unsafeForeignPtrToPtr context
      callablePtr = unsafeForeignPtrToPtr callable
      kernel args = do
        setArgvStorage storage contextPtr args
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
  pure $ curryG @(FunctionArguments f) @(IO ()) kernel

-- class PrepareParameters ts where
--   prepareParameters :: IO (Arguments ts)
--
-- instance PrepareParameters '[] where
--   prepareParameters :: IO (Arguments '[])
--   prepareParameters = pure Nil
--
-- instance (ValidParameter t, PrepareParameters ts) => PrepareParameters (t ': ts) where
--   prepareParameters :: IO (Arguments (t : ts))
--   prepareParameters = do
--     t <- prepareParameter @t
--     ts <- prepareParameters @ts
--     pure $ t ::: ts

-- prepareCxxArguments
--   :: forall ts b
--    . (ValidParameters' ts, All ValidParameter ts, KnownNat (Length ts))
--   => Arguments ts
--   -> (Ptr (CxxVector CxxArgument) -> IO b)
--   -> IO b
-- prepareCxxArguments args action = do
--   let count = fromIntegral (natVal (Proxy @(Length ts)))
--       allocate =
--         [CU.block| std::vector<Halide::Argument>* {
--           auto p = new std::vector<Halide::Argument>{};
--           p->reserve($(size_t count));
--           return p;
--         } |]
--       destroy p = [CU.exp| void { delete $(std::vector<Halide::Argument>* p) } |]
--   bracket allocate destroy $ \v -> do
--     let go :: (All ValidParameter ts') => Arguments ts' -> IO ()
--         go Nil = pure ()
--         go (x ::: xs) = appendToArgList v x >> go xs
--     go args
--     action v

wrapCxxUserContext :: Ptr CxxUserContext -> IO (ForeignPtr CxxUserContext)
wrapCxxUserContext = newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteUserContext(Halide::JITUserContext* p) { delete p; } |]

newEmptyCxxUserContext :: IO (ForeignPtr CxxUserContext)
newEmptyCxxUserContext =
  wrapCxxUserContext =<< [CU.exp| Halide::JITUserContext* { new Halide::JITUserContext{} } |]

-- wrapCxxCallable :: Ptr CxxCallable -> IO (Callable inputs outputs)
-- wrapCxxCallable = fmap Callable . newForeignPtr deleter
--   where
--     deleter = [C.funPtr| void deleteCallable(Halide::Callable* p) { delete p; } |]

wrapCxxCallable :: Ptr CxxCallable -> IO (Callable signature)
wrapCxxCallable = fmap Callable . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteCallable(Halide::Callable* p) { delete p; } |]

-- class All ValidArgument (LoweredOutputs t) => IsOutput t

-- type Lowered :: forall k. k -> k

-- | Specifies how t'Expr' and t'Func' parameters become scalar and buffer arguments in compiled kernels.
-- type family Lowered (t :: k) :: k where
--   Lowered (Expr a) = a
--   Lowered (Func t n (Expr a)) = Ptr (HalideBuffer n a)
--   Lowered '[] = '[]
--   Lowered (t ': ts) = (Lowered t ': Lowered ts)

-- | A constraint that specifies that the function @f@ returns @'IO' ('Func' t n a)@.
-- class (FunctionReturn f ~ IO (Func 'FuncTy n a), KnownNat n) => ReturnsFunc f n a | f -> n a

-- instance (FunctionReturn f ~ IO (Func 'FuncTy n a), KnownNat n) => ReturnsFunc f n a

-- type family ValidParameters' (p :: [Type]) :: Constraint where
--   ValidParameters' (Expr a ': rest) = (IsHalideType a, ValidParameter (Expr a), ValidParameters' rest)
--   ValidParameters' (Func t n (Expr a) ': rest) = (t ~ 'ParamTy, IsHalideType a, ValidParameter (Func 'ParamTy n (Expr a)), ValidParameters' rest)
--   ValidParameters' (a ': rest) = (Bottom, ValidParameters' rest)
--   ValidParameters' '[] = ()

-- type IsFuncBuilder f n a =
--   ( ValidParameters' (FunctionArguments f)
--   , All ValidParameter (FunctionArguments f)
--   , All ValidArgument (Concat (Lowered (FunctionArguments f)) (LoweredOutputs (Func 'FuncTy n a)))
--   , UnCurry f (FunctionArguments f) (FunctionReturn f)
--   , PrepareParameters (FunctionArguments f)
--   , ReturnsFunc f n a
--   , KnownNat (Length (FunctionArguments f))
--   , KnownNat (Length (Lowered (FunctionArguments f)))
--   , KnownNat (Length (LoweredOutputs (Func 'FuncTy n a)))
--   )

-- buildFunc :: (IsFuncBuilder f n a) => f -> IO (Arguments (FunctionArguments f), Func 'FuncTy n a)
-- buildFunc builder = do
--   parameters <- prepareParameters
--   func <- uncurryG builder parameters
--   pure (parameters, func)

-- newtype Callable (inputs :: [Type]) (outputs :: [Type]) = Callable (ForeignPtr CxxCallable)

-- compileToCallable
--   :: forall n a f inputs outputs
--    . ( IsFuncBuilder f n a
--      , Lowered (FunctionArguments f) ~ inputs
--      , LoweredOutputs (Func 'FuncTy n a) ~ outputs
--      )
--   => Target
--   -> f
--   -> IO (Callable inputs outputs)
-- compileToCallable target builder = do
--   (args, func) <- buildFunc builder
--   prepareCxxArguments args $ \args' ->
--     case func of
--       Func fp ->
--         withForeignPtr fp $ \func' ->
--           withCxxTarget target $ \target' ->
--             wrapCxxCallable
--               =<< [C.throwBlock| Halide::Callable* {
--                     return handle_halide_exceptions([=]() {
--                       return new Halide::Callable{
--                         $(Halide::Func* func')->compile_to_callable(
--                           *$(const std::vector<Halide::Argument>* args'),
--                           *$(const Halide::Target* target'))};
--                     });
--                   } |]
--   where
--     _ = keepRedundantConstraint (Proxy @(LoweredOutputs (Func 'FuncTy n a) ~ outputs))

-- callableToFunction
--   :: forall inputs outputs kernel
--    . ( Curry (Concat inputs outputs) (IO ()) kernel
--      , KnownNat (Length inputs)
--      , KnownNat (Length outputs)
--      , All ValidArgument (Concat inputs outputs)
--      )
--   => Callable inputs outputs
--   -> IO kernel
-- callableToFunction (Callable callable) = do
--   context <- newEmptyCxxUserContext
--   -- +1 comes from CxxUserContext
--   let argc =
--         1
--           + fromIntegral (natVal (Proxy @(Length inputs)))
--           + fromIntegral (natVal (Proxy @(Length outputs)))
--   storage@(ArgvStorage argv scalarStorage) <- newArgvStorage (fromIntegral argc)
--   let argvPtr = P.mutablePrimArrayContents argv
--       contextPtr = unsafeForeignPtrToPtr context
--       callablePtr = unsafeForeignPtrToPtr callable
--       kernel args = do
--         setArgvStorage storage (contextPtr ::: args)
--         [CU.exp| void {
--           handle_halide_exceptions([=]() {
--             return $(Halide::Callable* callablePtr)->call_argv_fast(
--               $(int argc), $(const void* const* argvPtr));
--           })
--         } |]
--         touch argv
--         touch scalarStorage
--         touch context
--         touch callable
--   pure $ curryG @(Concat inputs outputs) @(IO ()) kernel

-- | Convert a function that builds a Halide 'Func' into a normal Haskell function acccepting scalars and
-- 'HalideBuffer's.
--
-- For example:
--
-- @
-- builder :: Expr Float -> Func 'ParamTy 1 Float -> IO (Func 'FuncTy 1 Float)
-- builder scale inputVector = do
--   i <- 'mkVar' "i"
--   scaledVector <- 'define' "scaledVector" i $ scale * inputVector '!' i
--   pure scaledVector
-- @
--
-- The @builder@ function accepts a scalar parameter and a vector and scales the vector by the given factor.
-- We can now pass @builder@ to 'compile':
--
-- @
-- scaler <- 'compile' builder
-- 'withHalideBuffer' @1 @Float [1, 1, 1] $ \inputVector ->
--   'allocaCpuBuffer' [3] $ \outputVector -> do
--     -- invoke the kernel
--     scaler 2.0 inputVector outputVector
--     -- print the result
--     print =<< 'peekToList' outputVector
-- @
compile
  :: forall f n a
   . (FuncBuilder f n a, IsHalideKernel (LoweredSignature f))
  => f
  -- ^ Function to compile
  -> IO (LoweredSignature f)
  -- ^ Compiled kernel
compile = compileForTarget hostTarget

-- | Similar to 'compile', but the first argument lets you explicitly specify the compilation target.
compileForTarget
  :: forall f n a
   . (FuncBuilder f n a, IsHalideKernel (LoweredSignature f))
  => Target
  -> f
  -> IO (LoweredSignature f)
compileForTarget target builder = compileToCallable target builder >>= callableToFunction

-- | Format in which to return the lowered code.
data StmtOutputFormat
  = -- | plain text
    StmtText
  | -- | HTML
    StmtHTML
  deriving stock (Show, Eq)

instance Enum StmtOutputFormat where
  fromEnum =
    fromIntegral . \case
      StmtText -> [CU.pure| int { static_cast<int>(Halide::StmtOutputFormat::Text) } |]
      StmtHTML -> [CU.pure| int { static_cast<int>(Halide::StmtOutputFormat::HTML) } |]
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::StmtOutputFormat::Text) } |] = StmtText
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::StmtOutputFormat::HTML) } |] = StmtHTML
    | otherwise = error $ "invalid StmtOutputFormat " <> show k

-- | Get the internal representation of lowered code.
--
-- Useful for analyzing and debugging scheduling. Can emit HTML or plain text.
compileToLoweredStmt
  :: forall n a f. (FuncBuilder f n a) => StmtOutputFormat -> Target -> f -> IO Text
compileToLoweredStmt format target builder = do
  withSystemTempDirectory "halide-haskell" $ \dir -> do
    let s = encodeUtf8 (pack (dir <> "/code.stmt"))
        o = fromIntegral (fromEnum format)
    bracket (newCxxVector Nothing) deleteCxxVector $ \v -> do
      func <- buildFunc @f @n @a v builder
      withCxxFunc func $ \func' ->
        withCxxTarget target $ \target' ->
          [C.throwBlock| void {
            handle_halide_exceptions([=]() {
              auto& func = *$(Halide::Func* func');
              auto& args = *$(std::vector<Halide::Argument>* v);
              auto const& target = *$(const Halide::Target* target');
              std::reverse(std::begin(args), std::end(args));

              func.compile_to_lowered_stmt(
                std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)},
                args,
                static_cast<Halide::StmtOutputFormat>($(int o)),
                target);
            });
          } |]
    T.readFile (dir <> "/code.stmt")
