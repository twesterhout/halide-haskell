{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Language.Halide.Internal.Context (CxxExprVector, CxxFunc, CxxFuncRef, deriveIsHalideTypeCFloat) where

import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Type
import Language.Haskell.TH

-- C.context $
--   C.cppCtx
--     <> C.cppTypePairs
--       [ ("Halide::Expr", [t|CxxExpr|]),
--         ("Halide::Func", [t|CxxFunc|]),
--         ("Halide::Param", [t|CxxParam|]),
--         ("halide_type_t", [t|HalideType|])
--       ]
--
-- C.include "<Halide.h>"

-- defineCastFromTo :: String -> String -> DecsQ
-- defineCastFromTo fromType toType =
--   C.substitute
--     [("From", const fromType), ("To", const toType)]
--     [d|
--       instance Castable $(C.getHaskellType False fromType) $(C.getHaskellType False toType) where
--         castImpl :: proxy $(C.getHaskellType False fromType) -> proxy $(C.getHaskellType False toType) -> Ptr CxxExpr -> IO (Ptr CxxExpr)
--         castImpl _ _ x = [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cast<@To()>(*$(Halide::Expr* x))} } |]
--       |]

deriveIsHalideTypeCFloat :: DecsQ
deriveIsHalideTypeCFloat =
  [d|
    instance IsHalideType C.CFloat where
      type CxxType C.CFloat = C.CFloat
      -- halideTypeFor _ = [CU.exp| halide_type_t { halide_type_of<float>() } |]
      -- toCxxExpr value = [CU.exp| Halide::Expr* { new Halide::Expr{$(float value)} } |]
    |]

-- stdVectorCtx :: C.Context
-- stdVectorCtx = C.cppCtx `mappend` C.cppTypePairs [("std::vector", [t|CStdVector|])]

-- class IsHalideType a where
--   type CxxType a :: Type
--   halideTypeFor :: proxy a -> HalideType
--   toCxxExpr :: a -> IO (Ptr CxxExpr)
--   toCxxParam :: a -> IO (Ptr (CxxParam (CxxType a)))

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