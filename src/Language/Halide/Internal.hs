{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Halide.Internal (Expr (..), mkExpr) where

import Control.Monad ((<=<))
import Data.Bits (toIntegralSized)
import Data.Int
import Data.Word
import Foreign.C.Types (CDouble (..), CFloat (..))
import Foreign.ForeignPtr
import Foreign.Ptr (FunPtr, Ptr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Internal.Context (CxxExpr, IsCxxExpr, defineExprConstructors, toCxxExpr)
import System.IO.Unsafe (unsafePerformIO)

-- import qualified Language.C.Inline.Context as CC
-- import qualified Language.C.Types as CT
-- import qualified Language.C.Inline.Cpp.Exception as C
-- import qualified Language.C.Inline.Cpp.Exceptions as Legacy

C.context $
  C.cppCtx
    <> C.fptrCtx
    <> C.cppTypePairs
      [("Halide::Expr", [t|CxxExpr|])]

C.include "<Halide.h>"

newtype Expr = Expr (ForeignPtr CxxExpr)

defineExprConstructors

plusCxxExpr :: Ptr CxxExpr -> Ptr CxxExpr -> IO (Ptr CxxExpr)
plusCxxExpr a b = [CU.exp| Halide::Expr* { new Halide::Expr{*$(Halide::Expr* a) + *$(Halide::Expr* b)} } |]

wrapCxxExpr :: Ptr CxxExpr -> IO Expr
wrapCxxExpr = fmap Expr . newForeignPtr deleteCxxExpr

deleteCxxExpr :: FunPtr (Ptr CxxExpr -> IO ())
deleteCxxExpr = [C.funPtr| void deleteExpr(Halide::Expr *x) { delete x; } |]

withExpr :: Expr -> (Ptr CxxExpr -> IO a) -> IO a
withExpr (Expr fp) = withForeignPtr fp

mkExpr :: IsCxxExpr a => a -> Expr
mkExpr x = unsafePerformIO $! wrapCxxExpr =<< toCxxExpr x

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger x = case toIntegralSized x of
    Just (n :: Int64) -> mkExpr n
    Nothing -> error $ "integer overflow when converting " <> show x <> " to Expr"
  (+) :: Expr -> Expr -> Expr
  a + b = unsafePerformIO $!
    withExpr a $ \aPtr ->
      withExpr b $
        wrapCxxExpr <=< plusCxxExpr aPtr