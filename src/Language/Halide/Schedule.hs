{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Halide.Schedule
  ( Dim (..)
  , DimType (..)
  , ForType (..)
  , SplitContents (..)
  , FuseContents (..)
  , Split (..)
  , Bound (..)
  , StorageDim (..)
  , FusedPair (..)
  , FuseLoopLevel (..)
  , StageSchedule (..)
  , ReductionVariable (..)
  , PrefetchDirective (..)
  , getStageSchedule
  -- , getStageSchedule
  -- , getFusedPairs
  -- , getReductionVariables
  -- , getFuseLoopLevel
  -- , getDims
  -- , getSplits
  , AutoScheduler (..)
  , loadAutoScheduler
  , applyAutoScheduler
  , getHalideLibraryPath
  , applySplits
  , applyDims
  , applySchedule
  )
where

import Control.Monad (void)
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
import Foreign.Marshal (allocaArray, peekArray, toBool)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Func
import Language.Halide.LoopLevel
import Language.Halide.Target
import Language.Halide.Type
import Language.Halide.Utils
import System.FilePath (takeDirectory)
import Prelude hiding (tail)

#if USE_DLOPEN
import qualified System.Posix.DynamicLinker as DL

loadLibrary :: Text -> IO ()
loadLibrary path = do
  _ <- DL.dlopen (unpack path) [DL.RTLD_LAZY]
  pure ()

#else
import qualified System.Win32.DLL as Win32

loadLibrary :: Text -> IO ()
loadLibrary path = do
  _ <- Win32.loadLibrary (unpack path)
  pure ()
#endif

-- | Type of dimension that tells which transformations are legal on it.
data DimType = DimPureVar | DimPureRVar | DimImpureRVar
  deriving stock (Show, Eq)

-- | Specifies how loop values are traversed.
data ForType
  = ForSerial
  | ForParallel
  | ForVectorized
  | ForUnrolled
  | ForExtern
  | ForGPUBlock
  | ForGPUThread
  | ForGPULane
  deriving stock (Show, Eq)

data Dim = Dim {var :: !Text, forType :: !ForType, deviceApi :: !DeviceAPI, dimType :: !DimType}
  deriving stock (Show, Eq)

data FuseContents = FuseContents
  { fuseOuter :: !Text
  , fuseInner :: !Text
  , fuseNew :: !Text
  }
  deriving stock (Show, Eq)

data SplitContents = SplitContents
  { splitOld :: !Text
  , splitOuter :: !Text
  , splitInner :: !Text
  , splitFactor :: !(Expr Int32)
  , splitExact :: !Bool
  , splitTail :: !TailStrategy
  }
  deriving stock (Show)

data Split
  = SplitVar !SplitContents
  | FuseVars !FuseContents
  deriving stock (Show)

data Bound = Bound
  { boundVar :: !Text
  , boundMin :: !(Maybe (Expr Int32))
  , boundExtent :: !(Expr Int32)
  , boundModulus :: !(Maybe (Expr Int32))
  , boundRemainder :: !(Maybe (Expr Int32))
  }
  deriving stock (Show)

data StorageDim = StorageDim
  { storageVar :: !Text
  , storageAlignment :: !(Maybe (Expr Int32))
  , storageBound :: !(Maybe (Expr Int32))
  , storageFold :: !(Maybe (Expr Int32, Bool))
  }
  deriving stock (Show)

data FusedPair = FusedPair !Text !(Text, Int) !(Text, Int)
  deriving stock (Show, Eq)

data FuseLoopLevel = FuseLoopLevel !SomeLoopLevel
  deriving stock (Show, Eq)

data ReductionVariable = ReductionVariable {varName :: !Text, minExpr :: !(Expr Int32), extentExpr :: !(Expr Int32)}
  deriving stock (Show)

data PrefetchBoundStrategy
  = PrefetchClamp
  | PrefetchGuardWithIf
  | PrefetchNonFaulting
  deriving stock (Show, Eq)

data PrefetchDirective = PrefetchDirective
  { prefetchFunc :: !Text
  , prefetchAt :: !Text
  , prefetchFrom :: !Text
  , prefetchOffset :: !(Expr Int32)
  , prefetchStrategy :: !PrefetchBoundStrategy
  , prefetchParameter :: !(Maybe (ForeignPtr CxxParameter))
  }
  deriving stock (Show)

data StageSchedule = StageSchedule
  { rvars :: ![ReductionVariable]
  , splits :: ![Split]
  , dims :: ![Dim]
  , prefetches :: ![PrefetchDirective]
  , fuseLevel :: !FuseLoopLevel
  , fusedPairs :: ![FusedPair]
  , allowRaceConditions :: !Bool
  , atomic :: !Bool
  , overrideAtomicAssociativityTest :: !Bool
  }
  deriving stock (Show)

importHalide

instanceHasCxxVector "Halide::Internal::Dim"
instanceHasCxxVector "Halide::Internal::Split"
instanceHasCxxVector "Halide::Internal::FusedPair"
instanceHasCxxVector "Halide::Internal::ReductionVariable"

instance Enum ForType where
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Serial) } |] =
        ForSerial
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Parallel) } |] =
        ForParallel
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Vectorized) } |] =
        ForVectorized
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Unrolled) } |] =
        ForUnrolled
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Extern) } |] =
        ForExtern
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::GPUBlock) } |] =
        ForGPUBlock
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::GPUThread) } |] =
        ForGPUThread
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::GPULane) } |] =
        ForGPULane
    | otherwise = error $ "invalid ForType: " <> show k
  fromEnum = error "Enum instance for ForType does not implement fromEnum"

instance Enum DimType where
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::DimType::PureVar) } |] =
        DimPureVar
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::DimType::PureRVar) } |] =
        DimPureRVar
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::DimType::ImpureRVar) } |] =
        DimImpureRVar
    | otherwise = error $ "invalid DimType: " <> show k
  fromEnum = error "Enum instance for DimType does not implement fromEnum"

instance Enum PrefetchBoundStrategy where
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::PrefetchBoundStrategy::Clamp) } |] =
        PrefetchClamp
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::PrefetchBoundStrategy::GuardWithIf) } |] =
        PrefetchGuardWithIf
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::PrefetchBoundStrategy::NonFaulting) } |] =
        PrefetchNonFaulting
    | otherwise = error $ "invalid PrefetchBoundStrategy: " <> show k
  fromEnum = error "Enum instance for ForType does not implement fromEnum"

instance Storable FusedPair where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::FusedPair) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::FusedPair) } |]
  peek p = do
    func1 <-
      peekCxxString
        =<< [CU.exp| const std::string* { &$(const Halide::Internal::FusedPair* p)->func_1 } |]
    func2 <-
      peekCxxString
        =<< [CU.exp| const std::string* { &$(const Halide::Internal::FusedPair* p)->func_2 } |]
    stage1 <-
      fromIntegral
        <$> [CU.exp| size_t { $(const Halide::Internal::FusedPair* p)->stage_1 } |]
    stage2 <-
      fromIntegral
        <$> [CU.exp| size_t { $(const Halide::Internal::FusedPair* p)->stage_2 } |]
    varName <-
      peekCxxString
        =<< [CU.exp| const std::string* { &$(const Halide::Internal::FusedPair* p)->var_name } |]
    pure $ FusedPair varName (func1, stage1) (func2, stage2)
  poke _ _ = error "Storable instance of FusedPair does not implement poke"

instance Storable ReductionVariable where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::ReductionVariable) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::ReductionVariable) } |]
  peek p = do
    varName <-
      peekCxxString
        =<< [CU.exp| const std::string* { &$(const Halide::Internal::ReductionVariable* p)->var } |]
    minExpr <-
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          $(const Halide::Internal::ReductionVariable* p)->min} } |]
    extentExpr <-
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          $(const Halide::Internal::ReductionVariable* p)->extent} } |]
    pure $ ReductionVariable varName minExpr extentExpr
  poke _ _ = error "Storable instance of ReductionVariable does not implement poke"

instance Storable PrefetchDirective where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::PrefetchDirective) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::PrefetchDirective) } |]
  peek p = do
    funcName' <-
      peekCxxString
        =<< [CU.exp| const std::string* { &$(const Halide::Internal::PrefetchDirective* p)->name } |]
    atVar' <-
      peekCxxString
        =<< [CU.exp| const std::string* { &$(const Halide::Internal::PrefetchDirective* p)->at } |]
    fromVar' <-
      peekCxxString
        =<< [CU.exp| const std::string* { &$(const Halide::Internal::PrefetchDirective* p)->from } |]
    offset' <-
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          $(const Halide::Internal::PrefetchDirective* p)->offset} } |]
    strategy' <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(const Halide::Internal::PrefetchDirective* p)->strategy) } |]
    -- isDefined <-
    --   toBool
    --     <$> [CU.exp| bool { $(const Halide::Internal::PrefetchDirective* p)->param.defined() } |]
    -- param' <-
    --   if isDefined
    --     then
    --       fmap Just $
    --         wrapCxxParameter
    --           =<< [CU.exp| Halide::Internal::Parameter* {
    --                 new Halide::Internal::Parameter{$(const Halide::Internal::PrefetchDirective* p)->param} } |]
    --     else pure Nothing
    pure $ PrefetchDirective funcName' atVar' fromVar' offset' strategy' Nothing
  poke _ _ = error "Storable instance for PrefetchDirective does not implement poke"

getReductionVariables :: Ptr CxxStageSchedule -> IO [ReductionVariable]
getReductionVariables schedule =
  peekCxxVector
    =<< [CU.exp| const std::vector<Halide::Internal::ReductionVariable>* {
          &$(const Halide::Internal::StageSchedule* schedule)->rvars() } |]

getSplits :: Ptr CxxStageSchedule -> IO [Split]
getSplits schedule =
  peekCxxVector
    =<< [CU.exp| const std::vector<Halide::Internal::Split>* {
            &$(const Halide::Internal::StageSchedule* schedule)->splits() } |]

getDims :: Ptr CxxStageSchedule -> IO [Dim]
getDims schedule =
  peekCxxVector
    =<< [CU.exp| const std::vector<Halide::Internal::Dim>* {
          &$(const Halide::Internal::StageSchedule* schedule)->dims() } |]

getFuseLoopLevel :: Ptr CxxStageSchedule -> IO FuseLoopLevel
getFuseLoopLevel schedule =
  fmap FuseLoopLevel $
    wrapCxxLoopLevel
      =<< [CU.exp| Halide::LoopLevel* {
            new Halide::LoopLevel{$(const Halide::Internal::StageSchedule* schedule)->fuse_level().level}
          } |]

getFusedPairs :: Ptr CxxStageSchedule -> IO [FusedPair]
getFusedPairs schedule = do
  peekCxxVector
    =<< [CU.exp| const std::vector<Halide::Internal::FusedPair>* {
          &$(const Halide::Internal::StageSchedule* schedule)->fused_pairs() } |]

peekStageSchedule :: Ptr CxxStageSchedule -> IO StageSchedule
peekStageSchedule schedule = do
  rvars' <- getReductionVariables schedule
  splits' <- getSplits schedule
  dims' <- getDims schedule
  let prefetches' = []
  fuseLevel' <- getFuseLoopLevel schedule
  fusedPairs' <- getFusedPairs schedule
  allowRaceConditions' <-
    toBool
      <$> [CU.exp| bool { $(const Halide::Internal::StageSchedule* schedule)->allow_race_conditions() } |]
  atomic' <-
    toBool
      <$> [CU.exp| bool { $(const Halide::Internal::StageSchedule* schedule)->atomic() } |]
  overrideAtomicAssociativityTest' <-
    toBool
      <$> [CU.exp| bool { $(const Halide::Internal::StageSchedule* schedule)->override_atomic_associativity_test() } |]
  pure $
    StageSchedule
      { rvars = rvars'
      , splits = splits'
      , dims = dims'
      , prefetches = prefetches'
      , fuseLevel = fuseLevel'
      , fusedPairs = fusedPairs'
      , allowRaceConditions = allowRaceConditions'
      , atomic = atomic'
      , overrideAtomicAssociativityTest = overrideAtomicAssociativityTest'
      }

instance Storable Dim where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::Dim) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::Dim) } |]
  peek p = do
    name <- peekCxxString =<< [CU.exp| const std::string* { &$(Halide::Internal::Dim* p)->var } |]
    forType' <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->for_type) } |]
    device <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->device_api) } |]
    dimType' <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->dim_type) } |]
    pure $ Dim name forType' device dimType'
  poke _ = error "Storable instance for Dim does not implement poke"

peekOld :: Ptr Split -> IO Text
peekOld p = peekCxxString =<< [CU.exp| const std::string* { &$(const Halide::Internal::Split* p)->old_var } |]

peekOuter :: Ptr Split -> IO Text
peekOuter p = peekCxxString =<< [CU.exp| const std::string* { &$(const Halide::Internal::Split* p)->outer } |]

peekInner :: Ptr Split -> IO Text
peekInner p = peekCxxString =<< [CU.exp| const std::string* { &$(const Halide::Internal::Split* p)->inner } |]

peekFactor :: Ptr Split -> IO (Expr Int32)
peekFactor p =
  cxxConstructExpr $ \ptr ->
    [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
      $(const Halide::Internal::Split* p)->factor} } |]

instance Storable Split where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::Split) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::Split) } |]
  peek p = do
    isRename <- toBool <$> [CU.exp| bool { $(const Halide::Internal::Split* p)->is_rename() } |]
    isSplit <- toBool <$> [CU.exp| bool { $(const Halide::Internal::Split* p)->is_split() } |]
    isFuse <- toBool <$> [CU.exp| bool { $(const Halide::Internal::Split* p)->is_fuse() } |]
    isPurify <- toBool <$> [CU.exp| bool { $(const Halide::Internal::Split* p)->is_purify() } |]
    let r
          | isSplit =
              fmap SplitVar $
                SplitContents
                  <$> peekOld p
                  <*> peekOuter p
                  <*> peekInner p
                  <*> peekFactor p
                  <*> (toBool <$> [CU.exp| bool { $(const Halide::Internal::Split* p)->exact } |])
                  <*> fmap
                    (toEnum . fromIntegral)
                    [CU.exp| int { static_cast<int>($(const Halide::Internal::Split* p)->tail) } |]
          | isFuse =
              fmap FuseVars $
                FuseContents
                  <$> peekOuter p
                  <*> peekInner p
                  <*> peekOld p
          | isRename = error "renames are not yet implemented"
          | isPurify = error "purify is not yet implemented"
          | otherwise = error "invalid split type"
    r
  poke _ = error "Storable instance for Split does not implement poke"

-- wrapCxxStageSchedule :: Ptr CxxStageSchedule -> IO StageSchedule
-- wrapCxxStageSchedule = fmap StageSchedule . newForeignPtr deleter
--   where
--     deleter =
--       [C.funPtr| void deleteSchedule(Halide::Internal::StageSchedule* p) {
--         std::cout << "deleting ..." << std::endl;
--         delete p; } |]

getStageSchedule :: (KnownNat n, IsHalideType a) => Stage n a -> IO StageSchedule
getStageSchedule stage =
  withCxxStage stage $ \stage' ->
    peekStageSchedule
      =<< [CU.exp| const Halide::Internal::StageSchedule* {
            &$(const Halide::Stage* stage')->get_schedule() } |]

#if USE_DLOPEN

getHalideLibraryPath :: IO (Maybe Text)
getHalideLibraryPath = do
  ptr <-
    [CU.block| std::string* {
      Dl_info info;
      if (dladdr((void const*)&Halide::load_plugin, &info) != 0 && info.dli_sname != nullptr) {
        auto symbol = dlsym(RTLD_NEXT, info.dli_sname);
        if (dladdr(symbol, &info) != 0 && info.dli_fname != nullptr) {
          return new std::string{info.dli_fname};
        }
      }
      return nullptr;
    } |]
  if ptr == nullPtr
    then pure Nothing
    else Just . pack . takeDirectory . unpack <$> peekAndDeleteCxxString ptr

#else

getHalideLibraryPath :: IO (Maybe Text)
getHalideLibraryPath = pure Nothing

#endif

data AutoScheduler
  = Adams2019
  | Li2018
  | Mullapudi2016
  deriving stock (Eq, Show)

loadAutoScheduler :: AutoScheduler -> IO ()
loadAutoScheduler scheduler = do
  lib <- getHalideLibraryPath
  let prepare s
        | Just dir <- lib = dir <> "/lib" <> s <> ".so"
        | Nothing <- lib = "lib" <> s <> ".so"
      path = prepare $
        case scheduler of
          Adams2019 -> "autoschedule_adams2019"
          Li2018 -> "autoschedule_li2018"
          Mullapudi2016 -> "autoschedule_mullapudi2016"
  loadLibrary path

applyAutoScheduler :: (KnownNat n, IsHalideType a) => AutoScheduler -> Target -> Func t n a -> IO Text
applyAutoScheduler scheduler target func = do
  let s = encodeUtf8 . pack . show $ scheduler
  withFunc func $ \f ->
    withCxxTarget target $ \t -> do
      peekAndDeleteCxxString
        =<< [C.throwBlock| std::string* {
              return handle_halide_exceptions([=](){
                auto name = std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)};
                auto pipeline = Halide::Pipeline{*$(Halide::Func* f)};
                auto params = Halide::AutoschedulerParams{name};
                auto results = pipeline.apply_autoscheduler(*$(Halide::Target* t), params);
                return new std::string{std::move(results.schedule_source)};
              });
            } |]

makeUnqualified :: Text -> Text
makeUnqualified = snd . T.breakOnEnd "."

applySplit :: (KnownNat n, IsHalideType a) => Split -> Stage n a -> IO ()
applySplit (SplitVar x) stage = do
  oldVar <- mkVar (makeUnqualified x.splitOld)
  outerVar <- mkVar (makeUnqualified x.splitOuter)
  innerVar <- mkVar (makeUnqualified x.splitInner)
  void $ Language.Halide.Func.split x.splitTail oldVar (outerVar, innerVar) x.splitFactor stage
applySplit (FuseVars x) stage = do
  newVar <- mkVar (makeUnqualified x.fuseNew)
  innerVar <- mkVar (makeUnqualified x.fuseInner)
  outerVar <- mkVar (makeUnqualified x.fuseOuter)
  void $ Language.Halide.Func.fuse (innerVar, outerVar) newVar stage

applySplits :: (KnownNat n, IsHalideType a) => [Split] -> Stage n a -> IO ()
applySplits xs stage = mapM_ (`applySplit` stage) xs

applyDim :: (KnownNat n, IsHalideType a) => Dim -> Stage n a -> IO ()
applyDim x stage = do
  var' <- mkVar (makeUnqualified x.var)
  void $
    case x.forType of
      ForSerial -> pure stage
      ForParallel -> parallel var' stage
      ForVectorized -> vectorize var' stage
      ForUnrolled -> unroll var' stage
      ForExtern -> error "extern ForType is not yet supported by applyDim"
      ForGPUBlock -> gpuBlocks x.deviceApi var' stage
      ForGPUThread -> gpuThreads x.deviceApi var' stage
      ForGPULane -> gpuLanes x.deviceApi var' stage

applyDims :: (KnownNat n, IsHalideType a) => [Dim] -> Stage n a -> IO ()
applyDims xs stage = do
  mapM_ (`applyDim` stage) xs
  vars <- mapM (mkVar . makeUnqualified . (.var)) xs
  void $ reorder vars stage

applySchedule :: (KnownNat n, IsHalideType a) => StageSchedule -> Stage n a -> IO ()
applySchedule schedule stage = do
  applySplits schedule.splits stage
  applyDims schedule.dims stage

-- data SplitContents = SplitContents
--   { old :: !Text
--   , outer :: !Text
--   , inner :: !Text
--   , factor :: !(Maybe Int)
--   , exact :: !Bool
--   , tail :: !TailStrategy
--   }
--   deriving stock (Show, Eq)
--
--
-- applySplits :: [Split] -> Stage n a -> IO ()
-- applySplits splits stage =

-- v <-
--   [CU.exp| Halide::Internal::Dim* {
--        $(Halide::Internal::StageSchedule* schedule)->dims().data() } |]
-- putStrLn $ "n = " <> show n
-- mapM (\i -> print i >> peekElemOff v i) [0 .. n - 1]
