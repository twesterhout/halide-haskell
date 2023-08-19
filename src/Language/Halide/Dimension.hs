{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |
-- Module      : Language.Halide.Dimension
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Dimension
  ( Dimension (..)
  , setMin
  , setExtent
  , setStride
  , setEstimate

    -- * Internal
  , CxxDimension
  , wrapCxxDimension
  , withCxxDimension
  )
where

import Foreign.ForeignPtr
import Foreign.Ptr (Ptr)
import GHC.Records (HasField (..))
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Type
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (tail, (>))

-- | Haskell counterpart of [@Halide::Internal::Dimension@](https://halide-lang.org/docs/class_halide_1_1_internal_1_1_dimension.html).
data CxxDimension

importHalide

-- | Information about a buffer's dimension, such as the min, extent, and stride.
newtype Dimension = Dimension (ForeignPtr CxxDimension)

instance Show Dimension where
  showsPrec d dim =
    showParen (d > 10) $
      showString "Dimension { min="
        . shows dim.min
        . showString (", extent=" :: String)
        . shows dim.extent
        . showString (", stride=" :: String)
        . shows dim.stride
        . showString " }"

instance HasField "min" Dimension (Expr Int32) where
  getField :: Dimension -> Expr Int32
  getField dim = unsafePerformIO $
    withCxxDimension dim $ \d ->
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          $(const Halide::Internal::Dimension* d)->min()} } |]

-- | Set the min in a given dimension to equal the given expression. Setting the mins to
-- zero may simplify some addressing math.
--
-- For more info, see [Halide::Internal::Dimension::set_min](https://halide-lang.org/docs/class_halide_1_1_internal_1_1_dimension.html#a84acaf7733391fdaea4f4cec24a60de2).
setMin :: Expr Int32 -> Dimension -> IO Dimension
setMin expr dim = do
  asExpr expr $ \n ->
    withCxxDimension dim $ \d ->
      [CU.exp| void {
        $(Halide::Internal::Dimension* d)->set_min(*$(const Halide::Expr* n)) } |]
  pure dim

instance HasField "extent" Dimension (Expr Int32) where
  getField :: Dimension -> Expr Int32
  getField dim = unsafePerformIO $
    withCxxDimension dim $ \d ->
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          $(const Halide::Internal::Dimension* d)->extent()} } |]

-- | Set the extent in a given dimension to equal the given expression.
--
-- Halide will generate runtime errors for Buffers that fail this check.
--
-- For more info, see [Halide::Internal::Dimension::set_extent](https://halide-lang.org/docs/class_halide_1_1_internal_1_1_dimension.html#a54111d8439a065bdaca5b9ff9bcbd630).
setExtent :: Expr Int32 -> Dimension -> IO Dimension
setExtent expr dim = do
  asExpr expr $ \n ->
    withCxxDimension dim $ \d ->
      [CU.exp| void {
        $(Halide::Internal::Dimension* d)->set_extent(*$(const Halide::Expr* n)) } |]
  pure dim

instance HasField "max" Dimension (Expr Int32) where
  getField :: Dimension -> Expr Int32
  getField dim = unsafePerformIO $
    withCxxDimension dim $ \d ->
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          $(Halide::Internal::Dimension* d)->max()} } |]

instance HasField "stride" Dimension (Expr Int32) where
  getField :: Dimension -> Expr Int32
  getField dim = unsafePerformIO $
    withCxxDimension dim $ \d ->
      cxxConstructExpr $ \ptr ->
        [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{
          $(Halide::Internal::Dimension* d)->stride()} } |]

-- | Set the stride in a given dimension to equal the given expression.
--
-- This is particularly useful to set when vectorizing. Known strides for the vectorized
-- dimensions generate better code.
--
-- For more info, see [Halide::Internal::Dimension::set_stride](https://halide-lang.org/docs/class_halide_1_1_internal_1_1_dimension.html#a94f4c432a89907e2cc2aa908b5012cf8).
setStride :: Expr Int32 -> Dimension -> IO Dimension
setStride expr dim = do
  asExpr expr $ \n ->
    withCxxDimension dim $ \d ->
      [CU.exp| void {
        $(Halide::Internal::Dimension* d)->set_stride(*$(const Halide::Expr* n)) } |]
  pure dim

-- | Set estimates for autoschedulers.
setEstimate
  :: Expr Int32
  -- ^ @min@ estimate
  -> Expr Int32
  -- ^ @extent@ estimate
  -> Dimension
  -> IO Dimension
setEstimate minExpr extentExpr dim = do
  asExpr minExpr $ \m ->
    asExpr extentExpr $ \e ->
      withCxxDimension dim $ \d ->
        [CU.exp| void {
          $(Halide::Internal::Dimension* d)->set_estimate(*$(const Halide::Expr* m),
                                                          *$(const Halide::Expr* e)) } |]
  pure dim

wrapCxxDimension :: Ptr CxxDimension -> IO Dimension
wrapCxxDimension = fmap Dimension . newForeignPtr deleter
  where
    deleter = [C.funPtr| void deleteDimension(Halide::Internal::Dimension* p) { delete p; } |]

withCxxDimension :: Dimension -> (Ptr CxxDimension -> IO a) -> IO a
withCxxDimension (Dimension fp) = withForeignPtr fp
