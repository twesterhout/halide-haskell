{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Halide.Internal.Context (CxxExprVector, CxxFunc, CxxFuncRef, defineCastFromTo) where

import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Type
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data CxxFunc

data CxxFuncRef

data CxxExprVector

defineCastFromTo :: String -> String -> DecsQ
defineCastFromTo fromType toType =
  C.substitute
    [("From", const fromType), ("To", const toType)]
    [d|
      instance Castable $(C.getHaskellType False fromType) $(C.getHaskellType False toType) where
        castImpl :: proxy $(C.getHaskellType False fromType) -> proxy $(C.getHaskellType False toType) -> Ptr CxxExpr -> IO (Ptr CxxExpr)
        castImpl _ _ x = [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cast<@To()>(*$(Halide::Expr* x))} } |]
      |]

-- defineExprConstructors :: DecsQ
-- defineExprConstructors =
--   concat
--     <$> mapM
--       createExprFrom
--       [ "float",
--         "double",
--         "int8_t",
--         "int16_t",
--         "int32_t",
--         "int64_t",
--         "uint8_t",
--         "uint16_t",
--         "uint32_t",
--         "uint64_t"
--       ]