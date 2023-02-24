{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Halide.Schedule
  ( Dim (..)
  , DimType (..)
  , ForType (..)
  , Split (..)
  , StageSchedule (..)
  , getStageSchedule
  -- , getDims
  , getSplits
  , AutoScheduler (..)
  , loadAutoScheduler
  , applyAutoScheduler
  , getHalideLibraryPath
  )
where

import Data.ByteString (packCString)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable
import GHC.Records (HasField (..))
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Func
import Language.Halide.Target
import Language.Halide.Type
import Language.Halide.Utils
import System.FilePath (takeDirectory)
import System.IO.Unsafe
import System.Posix.DynamicLinker
import Prelude hiding (tail)

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

data FuseContents = FuseContents {outer :: !Text, inner :: !Text, new :: !Text}
  deriving stock (Show, Eq)

data SplitContents = SplitContents
  { old :: !Text
  , outer :: !Text
  , inner :: !Text
  , factor :: !(Maybe Int)
  , exact :: !Bool
  , tail :: !TailStrategy
  }
  deriving stock (Show, Eq)

data Split
  = SplitVar !SplitContents
  | FuseVars !FuseContents
  deriving stock (Show, Eq)

data Bound = Bound
  { var :: !Text
  , min :: !(Maybe (Expr Int32))
  , extent :: !(Expr Int32)
  , modulus :: !(Maybe (Expr Int32))
  , remainder :: !(Maybe (Expr Int32))
  }
  deriving stock (Show)

data StorageDim = StorageDim
  { var :: !Text
  , alignment :: !(Maybe (Expr Int32))
  , bound :: !(Maybe (Expr Int32))
  , fold :: !(Maybe (Expr Int32, Bool))
  }
  deriving stock (Show)

data FusedPair = FusedPair !Text !(Text, Int) !(Text, Int)
  deriving stock (Show, Eq)

newtype StageSchedule = StageSchedule (ForeignPtr CxxStageSchedule)

importHalide

instanceHasCxxVector "Halide::Internal::Dim"
instanceHasCxxVector "Halide::Internal::Split"

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

instance Storable Dim where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::Dim) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::Dim) } |]
  peek p = do
    name <- peekCxxString =<< [CU.exp| const std::string* { &$(Halide::Internal::Dim* p)->var } |]
    forType <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->for_type) } |]
    device <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->device_api) } |]
    dimType <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->dim_type) } |]
    pure $ Dim name forType device dimType
  poke _ = error "Storable instance for Dim does not implement poke"

peekOld :: Ptr Split -> IO Text
peekOld p =
  peekCxxString =<< [CU.exp| const std::string* { &$(Halide::Internal::Split* p)->old_var } |]

peekOuter :: Ptr Split -> IO Text
peekOuter p =
  peekCxxString =<< [CU.exp| const std::string* { &$(Halide::Internal::Split* p)->outer } |]

peekInner :: Ptr Split -> IO Text
peekInner p =
  peekCxxString =<< [CU.exp| const std::string* { &$(Halide::Internal::Split* p)->inner } |]

peekFactor :: Ptr Split -> IO (Maybe Int)
peekFactor p =
  toFactor
    <$> [CU.block| int {
          auto expr = $(Halide::Internal::Split* p)->factor;
          Halide::Internal::IntImm const* node = expr.as<Halide::Internal::IntImm>();
          if (node == nullptr) return -1;
          return node->value;
        } |]
  where
    toFactor n
      | n < 0 = Nothing
      | otherwise = Just (fromIntegral n)

instance Storable Split where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::Split) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::Split) } |]
  peek p = do
    isRename <- toBool <$> [CU.exp| bool { $(Halide::Internal::Split* p)->is_rename() } |]
    isSplit <- toBool <$> [CU.exp| bool { $(Halide::Internal::Split* p)->is_split() } |]
    isFuse <- toBool <$> [CU.exp| bool { $(Halide::Internal::Split* p)->is_fuse() } |]
    isPurify <- toBool <$> [CU.exp| bool { $(Halide::Internal::Split* p)->is_purify() } |]
    let r
          | isSplit =
              fmap SplitVar $
                SplitContents
                  <$> peekOld p
                  <*> peekOuter p
                  <*> peekInner p
                  <*> peekFactor p
                  <*> (toBool <$> [CU.exp| bool { $(Halide::Internal::Split* p)->exact } |])
                  <*> fmap
                    (toEnum . fromIntegral)
                    [CU.exp| int { static_cast<int>($(Halide::Internal::Split* p)->tail) } |]
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

wrapCxxStageSchedule :: Ptr CxxStageSchedule -> IO StageSchedule
wrapCxxStageSchedule = fmap StageSchedule . newForeignPtr deleter
  where
    deleter =
      [C.funPtr| void deleteSchedule(Halide::Internal::StageSchedule* p) {
        std::cout << "deleting ..." << std::endl;
        delete p; } |]

getStageSchedule :: (KnownNat n, IsHalideType a) => Func t n a -> IO StageSchedule
getStageSchedule func =
  withFunc func $ \f ->
    wrapCxxStageSchedule
      =<< [CU.exp| Halide::Internal::StageSchedule* {
            new Halide::Internal::StageSchedule{$(Halide::Func const* f)->get_schedule()} } |]

instance HasField "dims" StageSchedule [Dim] where
  getField (StageSchedule fp) = unsafePerformIO $
    withForeignPtr fp $ \schedule -> do
      n <-
        fromIntegral
          <$> [CU.exp| size_t { $(Halide::Internal::StageSchedule* schedule)->dims().size() } |]
      allocaArray n $ \p -> do
        [CU.block| void {
          auto const& dims = $(Halide::Internal::StageSchedule* schedule)->dims();
          auto* out = $(Halide::Internal::Dim* p);
          std::copy(std::begin(dims), std::end(dims), out);
          // auto const n = $(Halide::Internal::StageSchedule* schedule)->dims().size();
          // for (auto i = 0; i < n; ++i) {
          //   $(Halide::Internal::Dim* p)[i] =
          //     $(Halide::Internal::StageSchedule* schedule)->dims()[i];
          // }
        } |]
        peekArray n p

-- getDims :: StageSchedule -> [Dim]
-- getDims (StageSchedule fp) = unsafePerformIO $
--   withForeignPtr fp $ \schedule -> do
--     n <-
--       fromIntegral
--         <$> [CU.exp| size_t { $(Halide::Internal::StageSchedule* schedule)->dims().size() } |]
--     allocaArray n $ \p -> do
--       [CU.block| void {
--         auto const n = $(Halide::Internal::StageSchedule* schedule)->dims().size();
--         for (auto i = 0; i < n; ++i) {
--           $(Halide::Internal::Dim* p)[i] =
--             $(Halide::Internal::StageSchedule* schedule)->dims()[i];
--         }
--       } |]
--       peekArray n p

getSplits :: StageSchedule -> [Split]
getSplits (StageSchedule fp) = unsafePerformIO $
  withForeignPtr fp $ \schedule -> do
    n <-
      fromIntegral
        <$> [CU.exp| size_t { $(Halide::Internal::StageSchedule* schedule)->splits().size() } |]
    allocaArray n $ \p -> do
      [CU.block| void {
        auto const n = $(Halide::Internal::StageSchedule* schedule)->splits().size();
        for (auto i = 0; i < n; ++i) {
          $(Halide::Internal::Split* p)[i] =
            $(Halide::Internal::StageSchedule* schedule)->splits()[i];
        }
      } |]
      peekArray n p

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

data AutoScheduler
  = Adams2019
  | Li2018
  | Mullapudi2016
  deriving stock (Eq, Show)

loadAutoScheduler :: AutoScheduler -> IO ()
loadAutoScheduler scheduler = do
  lib <- getHalideLibraryPath
  print lib
  let prepare s
        | Just dir <- lib = dir <> "/lib" <> s <> ".so"
        | Nothing <- lib = "lib" <> s <> ".so"
      path = prepare $
        case scheduler of
          Adams2019 -> "autoschedule_adams2019"
          Li2018 -> "autoschedule_li2018"
          Mullapudi2016 -> "autoschedule_mullapudi2016"
  _ <- dlopen (unpack path) [RTLD_LAZY]
  pure ()

applyAutoScheduler :: (KnownNat n, IsHalideType a) => AutoScheduler -> Target -> Func t n a -> IO ()
applyAutoScheduler scheduler target func = do
  let s = encodeUtf8 . pack . show $ scheduler
  withFunc func $ \f ->
    withCxxTarget target $ \t -> do
      [C.throwBlock| void {
        handle_halide_exceptions([=](){
          auto name = std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)};
          auto pipeline = Halide::Pipeline{*$(Halide::Func* f)};
          auto params = Halide::AutoschedulerParams{name};
          auto results = pipeline.apply_autoscheduler(*$(Halide::Target* t), params);
          std::cout << '\n' << results.schedule_source << std::endl;
        });
      } |]

-- v <-
--   [CU.exp| Halide::Internal::Dim* {
--        $(Halide::Internal::StageSchedule* schedule)->dims().data() } |]
-- putStrLn $ "n = " <> show n
-- mapM (\i -> print i >> peekElemOff v i) [0 .. n - 1]
