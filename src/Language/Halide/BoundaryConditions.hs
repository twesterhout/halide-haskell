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

-- | Impose a boundary condition such that the nearest edge sample is returned everywhere outside the given region.
--
-- For more information, see [@Halide::repeat_edge@](https://halide-lang.org/docs/namespace_halide_1_1_boundary_conditions.html#a0548f23db36e4a8a03690bc8bee1e850).
repeatEdge :: (KnownNat n, IsHalideType a) => Func 'ParamTy n (Expr a) -> IO (Func 'FuncTy n (Expr a))
repeatEdge source =
  withBufferParam source $ \source' ->
    wrapCxxFunc
      =<< [CU.exp| Halide::Func* { new Halide::Func{
            Halide::BoundaryConditions::repeat_edge(*$(const Halide::ImageParam* source'))} } |]

-- | Impose a boundary condition such that a given expression is returned everywhere outside the boundary.
--
-- For more information, see [@Halide::constant_exterior@](https://halide-lang.org/docs/namespace_halide_1_1_boundary_conditions.html#aa4ed713b5f9a6f13e6323f2a21d41d5e).
constantExterior :: (KnownNat n, IsHalideType a) => Expr a -> Func 'ParamTy n (Expr a) -> IO (Func 'FuncTy n (Expr a))
constantExterior value source =
  withBufferParam source $ \source' ->
    asExpr value $ \value' ->
      wrapCxxFunc
        =<< [CU.exp| Halide::Func* { new Halide::Func{
              Halide::BoundaryConditions::constant_exterior(
                *$(const Halide::ImageParam* source'), *$(const Halide::Expr* value'))} } |]
