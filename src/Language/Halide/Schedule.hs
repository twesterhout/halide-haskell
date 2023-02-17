{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Halide.Schedule
  ( Dim (..)
  , DimType (..)
  , LoopLevel (..)
  , ForType (..)
  , Split (..)
  , StageSchedule (..)
  , getStageSchedule
  , getDims
  , getSplits
  , applyAutoscheduler
  )
where

import Data.ByteString (packCString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Foreign.C.Types (CInt (..))
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr (Ptr)
import Foreign.Storable
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
import Language.Halide.Func
import Language.Halide.Target
import Language.Halide.Type
import System.IO.Unsafe
import Prelude hiding (tail)

data DimType = DimPureVar | DimPureRVar | DimImpureRVar
  deriving stock (Show, Eq)

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

data LoopAlignStrategy
  = LoopAlignStart
  | LoopAlignEnd
  | LoopNoAlign
  | LoopAuto
  deriving stock (Show, Eq)

newtype LoopLevel = ForeignPtr CxxLoopLevel

data Dim = Dim !Text !ForType !DeviceAPI !DimType
  deriving stock (Show, Eq)

data Split = SplitVar !Text !Text !Text !(Maybe Int) !Bool !TailStrategy !CInt
  deriving stock (Show, Eq)

importHalide

instanceHasCxxVector "Halide::Internal::Dim"
instanceHasCxxVector "Halide::Internal::Split"

instance Enum ForType where
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Serial) } |] = ForSerial
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Parallel) } |] = ForParallel
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Vectorized) } |] = ForVectorized
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Unrolled) } |] = ForUnrolled
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::Extern) } |] = ForExtern
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::GPUBlock) } |] = ForGPUBlock
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::GPUThread) } |] = ForGPUThread
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::ForType::GPULane) } |] = ForGPULane
    | otherwise = error $ "invalid ForType: " <> show k
  fromEnum = error "Enum instance for ForType does not implement fromEnum"

instance Enum DimType where
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::DimType::PureVar) } |] = DimPureVar
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::DimType::PureRVar) } |] = DimPureRVar
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::Internal::DimType::ImpureRVar) } |] = DimImpureRVar
    | otherwise = error $ "invalid DimType: " <> show k
  fromEnum = error "Enum instance for DimType does not implement fromEnum"

instance Storable Dim where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::Dim) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::Dim) } |]
  peek p = do
    name <-
      fmap decodeUtf8 $
        packCString
          =<< [CU.block| char const* {
            std::cout << $(Halide::Internal::Dim* p)->var << std::endl;
            return $(Halide::Internal::Dim* p)->var.c_str();
          } |]
    (forType :: ForType) <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->for_type) } |]
    (device :: DeviceAPI) <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->device_api) } |]
    (dimType :: DimType) <-
      toEnum . fromIntegral
        <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Dim* p)->dim_type) } |]
    pure $ Dim name forType device dimType
  poke p = error "Storable instance for Dim does not implement poke"

instance Storable Split where
  sizeOf _ = fromIntegral [CU.pure| size_t { sizeof(Halide::Internal::Split) } |]
  alignment _ = fromIntegral [CU.pure| size_t { alignof(Halide::Internal::Split) } |]
  peek p = do
    oldVar <-
      fmap decodeUtf8
        <$> packCString
        =<< [CU.exp| char const* { $(Halide::Internal::Split* p)->old_var.c_str() } |]
    outer <-
      fmap decodeUtf8
        <$> packCString
        =<< [CU.exp| char const* { $(Halide::Internal::Split* p)->outer.c_str() } |]
    inner <-
      fmap decodeUtf8
        <$> packCString
        =<< [CU.exp| char const* { $(Halide::Internal::Split* p)->inner.c_str() } |]
    factor <-
      fromIntegral
        <$> [CU.block| int {
              auto expr = $(Halide::Internal::Split* p)->factor;
              Halide::Internal::IntImm const* node = expr.as<Halide::Internal::IntImm>();
              if (node == nullptr) return -1;
              return node->value;
            } |]
    exact <- toEnum . fromIntegral <$> [CU.exp| bool { $(Halide::Internal::Split* p)->exact } |]
    (tail :: TailStrategy) <-
      toEnum . fromIntegral <$> [CU.exp| int { static_cast<int>($(Halide::Internal::Split* p)->tail) } |]
    splitType <- [CU.exp| int { $(Halide::Internal::Split* p)->split_type } |]
    pure $ SplitVar oldVar outer inner (Just factor) exact tail splitType
  poke p = error "Storable instance for Split does not implement poke"

data StageSchedule = StageSchedule (ForeignPtr CxxStageSchedule)

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

getDims :: StageSchedule -> [Dim]
getDims (StageSchedule fp) = unsafePerformIO $
  withForeignPtr fp $ \schedule -> do
    n <-
      fromIntegral
        <$> [CU.exp| size_t { $(Halide::Internal::StageSchedule* schedule)->dims().size() } |]
    allocaArray n $ \p -> do
      [CU.block| void {
        auto const n = $(Halide::Internal::StageSchedule* schedule)->dims().size();
        for (auto i = 0; i < n; ++i) {
          $(Halide::Internal::Dim* p)[i] =
            $(Halide::Internal::StageSchedule* schedule)->dims()[i];
        }
      } |]
      peekArray n p

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

applyAutoscheduler :: (KnownNat n, IsHalideType a) => Func t n a -> Text -> Target -> IO ()
applyAutoscheduler func name target = do
  let s = encodeUtf8 name
  withFunc func $ \f ->
    withCxxTarget target $ \t -> do
      [C.throwBlock| void {
        handle_halide_exceptions([=](){
          auto name = std::string{$bs-ptr:s, static_cast<size_t>($bs-len:s)};
          if (name == "Adams2019") {
            Halide::load_plugin("autoschedule_adams2019");
          }
          else if (name == "Li2018") {
            Halide::load_plugin("autoschedule_li2018");
          }
          else if (name == "Mullapudi2016") {
            Halide::load_plugin("autoschedule_mullapudi2016");
          }
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
