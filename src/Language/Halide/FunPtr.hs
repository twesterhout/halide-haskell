{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Language.Halide.FunPtr
  ( callableToFunPtrQ
  , CoercibleCallable
  , funPtrClosureDeleter
  , freeFunPtrClosure
  )
where

import Data.Coerce (Coercible)
import Data.List qualified
import Data.Maybe (fromMaybe)
import Foreign (FunPtr, Ptr, newStablePtr)
import GHC.Stable (StablePtr)
import Language.C.Inline.Context qualified as C
import Language.C.Inline.Internal qualified as C
import Language.Halide.Context
import Language.Halide.Type
import Language.Haskell.TH (Q, lookupTypeName)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

validArgumentType :: TH.Type -> Q (Maybe String)
validArgumentType t = do
  types <- mapM (\(c, hs, _) -> (,c) <$> hs) halideTypes
  case Data.List.lookup t types of
    Just s -> pure $ Just s
    Nothing ->
      lookupTypeName "RawHalideBuffer" >>= \case
        Just buffer | t == TH.AppT (TH.ConT ''Ptr) (TH.ConT buffer) -> pure $ Just "halide_buffer_t*"
        Nothing -> fail "RawHalideBuffer not found in scope :("
        _ -> pure Nothing

validArgumentType' :: TH.Type -> Q String
validArgumentType' t =
  validArgumentType t >>= \case
    Just s -> pure s
    Nothing -> fail $ show t <> " is not a valid kernel argument type"

parseFunctionType :: TH.Type -> Q [TH.Type]
parseFunctionType (TH.AppT (TH.ConT a) (TH.TupleT 0)) | a == ''IO = pure []
parseFunctionType (TH.AppT (TH.AppT TH.ArrowT a) b) = validArgumentType' a >> (a :) <$> parseFunctionType b
parseFunctionType tp = error $ "parseFunctionType: invalid function type: " <> show tp

uniqueClosureName :: Q TH.Name
uniqueClosureName = TH.newName "halide_closure"

extractArg :: String -> String -> String
extractArg tp name
  | tp == "int8_t" = tp <> " const " <> name <> " = va_arg_schar(alist);"
  | tp == "int16_t" = tp <> " const " <> name <> " = va_arg_short(alist);"
  | tp == "int32_t" = tp <> " const " <> name <> " = va_arg_int(alist);"
  | tp == "int64_t" = tp <> " const " <> name <> " = va_arg_long(alist);"
  | tp == "uint8_t" = tp <> " const " <> name <> " = va_arg_uchar(alist);"
  | tp == "uint16_t" = tp <> " const " <> name <> " = va_arg_ushort(alist);"
  | tp == "uint32_t" = tp <> " const " <> name <> " = va_arg_uint(alist);"
  | tp == "uint64_t" = tp <> " const " <> name <> " = va_arg_ulong(alist);"
  | tp == "float" = tp <> " const " <> name <> " = va_arg_float(alist);"
  | tp == "double" = tp <> " const " <> name <> " = va_arg_double(alist);"
  | tp == "halide_buffer_t*" = tp <> " const " <> name <> " = va_arg_ptr(alist, halide_buffer_t*);"
  | otherwise = error $ "extractArg: unknown type: " <> tp <> "; this should never happen"

callableToFunPtrQ :: TH.TypeQ -> TH.ExpQ
callableToFunPtrQ kernel = do
  here <- TH.location
  hsTypes <- parseFunctionType =<< kernel
  cTypes <- mapM validArgumentType' hsTypes
  closureName <- uniqueClosureName
  let cFun = show closureName
      cFunPtr = cFun <> "_t"
      cNames = take (length cTypes) $ ("arg" <>) . show <$> [(0 :: Int) ..]
      cSource =
        ""
          <> ("typedef void(*" <> cFunPtr <> ")(" <> Data.List.intercalate ", " cTypes <> ");\n\n")
          <> ("void " <> cFun <> "(void* data, va_alist alist) {\n")
          <> "  auto const& ctx = *static_cast<std::tuple<Halide::Callable, void*>*>(data);\n"
          <> "  auto const& fn = std::get<0>(ctx);\n"
          <> "  va_start_void(alist);\n"
          <> (Data.List.unlines . fmap ("  " <>)) (zipWith extractArg cTypes cNames)
          <> ("  fn(" <> Data.List.intercalate ", " cNames <> ");\n")
          <> "  va_return_void(alist);\n"
          <> "}\n\n"
          <> (cFunPtr <> " mk_" <> cFun <> "(Halide::Callable* fn, void* keep_alive) {\n")
          <> "  auto const ctx = new std::tuple<Halide::Callable, void*>{*fn, keep_alive};\n"
          <> ("  callback_t callback = alloc_callback(&" <> cFun <> ", static_cast<void*>(ctx));\n")
          <> ("  return (" <> cFunPtr <> ")callback;\n")
          <> "}\n\n"
  out <- fromMaybe id . C.ctxOutput <$> C.getContext
  _ <- C.emitVerbatim . out $ C.lineDirective here ++ cSource
  ffiImportName <- TH.newName ("mk_" <> cFun)
  dynImportName <- TH.newName ("toFun_" <> cFun)
  ffiDec <- TH.forImpD TH.CCall TH.Unsafe ("mk_" <> cFun) ffiImportName [t|forall a. Ptr CxxCallable -> StablePtr a -> IO (FunPtr $(kernel))|]
  dynDec <- TH.forImpD TH.CCall TH.Unsafe "dynamic" dynImportName [t|FunPtr $(kernel) -> $(kernel)|]
  TH.addTopDecls [ffiDec, dynDec]
  [e|
    let _cfc_builder :: (CoercibleCallable f $(kernel)) => Maybe resource -> Callable f -> IO (FunPtr f, f)
        _cfc_builder resource (Callable fp) = do
          stablePtr <- maybe (pure (castPtrToStablePtr nullPtr)) newStablePtr resource
          funPtr <- withForeignPtr fp $ \cxxCallable -> $(TH.varE ffiImportName) cxxCallable stablePtr
          pure (coerce funPtr, coerce ($(TH.varE dynImportName) funPtr))
     in _cfc_builder
    |]

class (Coercible f g) => CoercibleCallable f g

instance (a ~ b) => CoercibleCallable (IO a) (IO b)

instance
  {-# OVERLAPPABLE #-}
  (CoercibleCallable f g, IsHalideType a, IsHalideType b, a ~ b)
  => CoercibleCallable (a -> f) (b -> g)

importHalide

defineClosureDeleter

foreign import ccall unsafe "&destroy_closure_and_callable" funPtrClosureDeleter :: FunPtr (FunPtr f -> IO ())

foreign import ccall unsafe "dynamic" mkFreeFunPtrClosure :: FunPtr (FunPtr f -> IO ()) -> FunPtr f -> IO ()

freeFunPtrClosure :: FunPtr f -> IO ()
freeFunPtrClosure = mkFreeFunPtrClosure funPtrClosureDeleter
