{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Halide.Internal.Context (CxxExpr, CxxExprVector, CxxFunc, CxxFuncRef, defineExprConstructors, IsCxxExpr, toCxxExpr) where

import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data CxxExpr

data CxxFunc

data CxxFuncRef

data CxxExprVector

class IsCxxExpr a where
  toCxxExpr :: a -> IO (Ptr CxxExpr)

createExprFrom :: String -> DecsQ
createExprFrom cType =
  C.substitute
    [("T", const cType)]
    [d|
      instance IsCxxExpr $(C.getHaskellType False cType) where
        toCxxExpr :: $(C.getHaskellType False cType) -> IO (Ptr CxxExpr)
        toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(@T() x)} } |]
      |]

defineExprConstructors :: DecsQ
defineExprConstructors =
  concat
    <$> mapM
      createExprFrom
      [ "float",
        "double",
        "int8_t",
        "int16_t",
        "int32_t",
        "int64_t",
        "uint8_t",
        "uint16_t",
        "uint32_t",
        "uint64_t"
      ]