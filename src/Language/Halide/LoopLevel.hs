{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Language.Halide.LoopLevel
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.LoopLevel
  ( LoopLevel (..)
  , LoopAlignStrategy (..)
  , getLoopLevelAtStage
  , getLoopLevel
  , storeAt
  , computeAt
  , computeWith

    -- * Internal
  , CxxLoopLevel
  , withCxxLoopLevel
  )
where

import Data.Text (unpack)
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Func
import Language.Halide.Type
import Language.Halide.Utils
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (min, tail)

-- | Haskell counterpart of @Halide::LoopLevel@
data CxxLoopLevel

importHalide

-- | A reference to a site in a Halide statement at the top of the body of a particular for loop.
newtype LoopLevel = LoopLevel (ForeignPtr CxxLoopLevel)

instance Eq LoopLevel where
  (==) level1 level2 =
    toEnum . fromIntegral . unsafePerformIO $
      withCxxLoopLevel level1 $ \l1 ->
        withCxxLoopLevel level2 $ \l2 ->
          [CU.exp| bool { *$(const Halide::LoopLevel* l1) == *$(const Halide::LoopLevel* l2) } |]

instance Show LoopLevel where
  show level = unpack . unsafePerformIO $
    withCxxLoopLevel level $ \l ->
      peekAndDeleteCxxString
        =<< [C.throwBlock| std::string* {
              return handle_halide_exceptions([=](){
                return new std::string{$(const Halide::LoopLevel* l)->to_string()};
              });
            } |]

-- | Different ways to handle the case when the start/end of the loops of stages computed with (fused)
-- are not aligned.
data LoopAlignStrategy
  = -- | Shift the start of the fused loops to align.
    LoopAlignStart
  | -- | Shift the end of the fused loops to align.
    LoopAlignEnd
  | -- | 'computeWith' will make no attempt to align the start/end of the fused loops.
    LoopNoAlign
  | -- | By default, LoopAlignStrategy is set to 'LoopNoAlign'.
    LoopAlignAuto
  deriving stock (Show, Eq, Ord)

instance Enum LoopAlignStrategy where
  fromEnum =
    fromIntegral . \case
      LoopAlignStart -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignStart) } |]
      LoopAlignEnd -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignEnd) } |]
      LoopNoAlign -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::NoAlign) } |]
      LoopAlignAuto -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::Auto) } |]
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignStart) } |] = LoopAlignStart
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignEnd) } |] = LoopAlignEnd
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::NoAlign) } |] = LoopNoAlign
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::Auto) } |] = LoopAlignAuto
    | otherwise = error $ "invalid LoopAlignStrategy: " <> show k

wrapCxxLoopLevel :: Ptr CxxLoopLevel -> IO LoopLevel
wrapCxxLoopLevel = fmap LoopLevel . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteLoopLevel(Halide::LoopLevel* p) { delete p; } |]

withCxxLoopLevel :: LoopLevel -> (Ptr CxxLoopLevel -> IO a) -> IO a
withCxxLoopLevel (LoopLevel fp) action = withForeignPtr fp $ \p -> do
  [CU.exp| void { $(Halide::LoopLevel* p)->lock() } |]
  action p

-- | Identify the loop nest corresponding to some dimension of some function.
getLoopLevelAtStage
  :: (KnownNat n, IsHalideType a)
  => Func t n a
  -> Expr Int32
  -> Int
  -- ^ update index
  -> IO LoopLevel
getLoopLevelAtStage func var stageIndex =
  withFunc func $ \f -> asVarOrRVar var $ \i ->
    wrapCxxLoopLevel
      =<< [C.throwBlock| Halide::LoopLevel* {
            return handle_halide_exceptions([=](){
              return new Halide::LoopLevel{*$(const Halide::Func* f),
                                           *$(const Halide::VarOrRVar* i),
                                           $(int k)};
            });
          } |]
  where
    k = fromIntegral stageIndex

-- | Same as 'getLoopLevelAtStage' except that the stage is @-1@.
getLoopLevel :: (KnownNat n, IsHalideType a) => Func t n a -> Expr Int32 -> IO LoopLevel
getLoopLevel f i = getLoopLevelAtStage f i (-1)

-- | Allocate storage for this function within a particular loop level.
--
-- Scheduling storage is optional, and can be used to separate the loop level at which storage is allocated
-- from the loop level at which computation occurs to trade off between locality and redundant work.
--
-- For more info, see [Halide::Func::store_at](https://halide-lang.org/docs/class_halide_1_1_func.html#a417c08f8aa3a5cdf9146fba948b65193).
storeAt :: (KnownNat n, IsHalideType a) => Func 'FuncTy n a -> LoopLevel -> IO (Func 'FuncTy n a)
storeAt func level = do
  withFunc func $ \f ->
    withCxxLoopLevel level $ \l ->
      [CU.exp| void { $(Halide::Func* f)->store_at(*$(const Halide::LoopLevel* l)) } |]
  pure func

-- | Schedule a function to be computed within the iteration over a given loop level.
--
-- For more info, see [Halide::Func::compute_at](https://halide-lang.org/docs/class_halide_1_1_func.html#a800cbcc3ca5e3d3fa1707f6e1990ec83).
computeAt :: (KnownNat n, IsHalideType a) => Func 'FuncTy n a -> LoopLevel -> IO (Func 'FuncTy n a)
computeAt func level = do
  withFunc func $ \f ->
    withCxxLoopLevel level $ \l ->
      [CU.exp| void { $(Halide::Func* f)->compute_at(*$(const Halide::LoopLevel* l)) } |]
  pure func

-- | Schedule the iteration over this stage to be fused with another stage from outermost loop to a given LoopLevel.
--
-- For more info, see [Halide::Stage::compute_with](https://halide-lang.org/docs/class_halide_1_1_stage.html#a82a2ae25a009d6a2d52cb407a25f0a5b).
computeWith
  :: (KnownNat n, IsHalideType a) => LoopAlignStrategy -> Func 'FuncTy n a -> LoopLevel -> IO (Func 'FuncTy n a)
computeWith align func level = do
  withFunc func $ \f ->
    withCxxLoopLevel level $ \l ->
      [C.throwBlock| void {
        handle_halide_exceptions([=]() {
          $(Halide::Func* f)->compute_with(*$(const Halide::LoopLevel* l),
                                           static_cast<Halide::LoopAlignStrategy>($(int a)));
        }); } |]
  pure func
  where
    a = fromIntegral . fromEnum $ align
