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

    -- * Internal
  , CxxLoopLevel
  , withCxxLoopLevel
  , wrapCxxLoopLevel
  )
where

import Data.Text (unpack)
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
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
  level1 == level2 =
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
