{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Language.Halide.BoundaryConditions
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.BoundaryConditions
  ( repeatEdge
  , constantExterior
  )
where

import GHC.TypeLits
import Language.C.Inline.Unsafe qualified as CU
import Language.Halide.Buffer
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Func
import Language.Halide.Type
import Prelude hiding (min, tail)

importHalide

-- | A sinle-dimensional span including all numbers between @min@ and @(min + extent - 1)@.
--
-- Haskell counterpart of [@Halide::Range@](https://halide-lang.org/docs/struct_halide_1_1_range.html).
-- data Range
--   = Range
--       !(Expr Int32)
--       -- ^ min
--       !(Expr Int32)
--       -- ^ extent
--   deriving stock (Show)

-- newtype Region (n :: Nat) = Region [Range]

-- region :: (KnownNat n, IsHalideType a) => Func 'ParamTy n a -> IO (Region n)
-- region = undefined

repeatEdge :: (KnownNat n, IsHalideType a) => Func 'ParamTy n (Expr a) -> IO (Func 'FuncTy n (Expr a))
repeatEdge source =
  withBufferParam source $ \source' ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* { new Halide::Func{
            Halide::BoundaryConditions::repeat_edge(*$(const Halide::ImageParam* source'))} } |]

constantExterior :: (KnownNat n, IsHalideType a) => Expr a -> Func 'ParamTy n (Expr a) -> IO (Func 'FuncTy n (Expr a))
constantExterior value source =
  withBufferParam source $ \source' ->
    asExpr value $ \value' ->
      wrapCxxFunc
        =<< [CU.exp| Halide::Func* { new Halide::Func{
              Halide::BoundaryConditions::constant_exterior(
                *$(const Halide::ImageParam* source'), *$(const Halide::Expr* value'))} } |]
