{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      : Language.Halide.Context
-- Description : Helpers to setup inline-c for Halide
-- Copyright   : (c) Tom Westerhout, 2023
--
-- This module defines a Template Haskell function 'importHalide' that sets up everything you need
-- to call Halide functions from 'Language.C.Inline' and 'Language.C.Inlinde.Cpp' quasiquotes.
--
-- We also define two C++ functions:
--
-- > template <class Func>
-- > auto handle_halide_exceptions(Func&& func);
-- >
-- > template <class T>
-- > auto to_string_via_iostream(T const& x) -> std::string*;
--
-- @handle_halide_exceptions@ can be used to catch various Halide exceptions and convert them to
-- [@std::runtime_error@](https://en.cppreference.com/w/cpp/error/runtime_error). It can be used
-- inside 'C.tryBlock' or 'C.catchBlock' to properly re-throw Halide errors.
--
-- @
-- [C.catchBlock| void {
--   handle_halide_exceptions([=]() {
--     Halide::Func f;
--     Halide::Var i;
--     f(i) = *$(Halide::Expr* e);
--     f.realize(Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)});
--   });
-- } |]
-- @
--
-- @to_string_via_iostream@ is a helper that converts a variable into a string by relying on
-- [iostreams](https://en.cppreference.com/w/cpp/io). It returns a pointer to
-- [@std::string@](https://en.cppreference.com/w/cpp/string/basic_string) that it allocated using the @new@
-- keyword. To convert it to a Haskell string, use the 'Language.Halide.Utils.peekCxxString' and
-- 'Language.Halide.Utils.peekAndDeleteCxxString' functions.
module Language.Halide.Context
  ( importHalide
  )
where

import Language.C.Inline qualified as C
import Language.C.Inline.Cpp qualified as C
import Language.C.Types (CIdentifier)
import Language.Halide.Type
import Language.Haskell.TH (DecsQ, Q, TypeQ, lookupTypeName)
import Language.Haskell.TH qualified as TH

-- | One stop function to include all the neccessary machinery to call Halide functions via inline-c.
--
-- Put @importHalide@ somewhere at the beginning of the file and enjoy using the C++ interface of
-- Halide via inline-c quasiquotes.
importHalide :: DecsQ
importHalide =
  concat
    <$> sequence
      [ C.context =<< halideCxt
      , C.include "<Halide.h>"
      , C.include "<HalideRuntimeOpenCL.h>"
      , C.include "<HalideRuntimeCuda.h>"
      , C.include "<cxxabi.h>"
      , C.include "<dlfcn.h>"
      , defineExceptionHandler
      ]

halideCxt :: Q C.Context
halideCxt = do
  typePairs <- C.cppTypePairs <$> halideTypePairs
  pure (C.cppCtx <> C.fptrCtx <> C.bsCtx <> typePairs)

halideTypePairs :: Q [(CIdentifier, TypeQ)]
halideTypePairs = do
  fmap concat . sequence $ [core, other]
  where
    core =
      pure
        [ ("Halide::Expr", [t|CxxExpr|])
        , ("Halide::Var", [t|CxxVar|])
        , ("Halide::RVar", [t|CxxRVar|])
        , ("Halide::VarOrRVar", [t|CxxVarOrRVar|])
        , ("Halide::Func", [t|CxxFunc|])
        , ("Halide::Internal::Parameter", [t|CxxParameter|])
        , ("Halide::ImageParam", [t|CxxImageParam|])
        , ("Halide::Callable", [t|CxxCallable|])
        , ("Halide::Target", [t|CxxTarget|])
        , ("Halide::JITUserContext", [t|CxxUserContext|])
        , ("Halide::Argument", [t|CxxArgument|])
        , ("std::vector", [t|CxxVector|])
        , ("std::string", [t|CxxString|])
        , ("halide_type_t", [t|HalideType|])
        ]
    other =
      optionals
        [ ("Halide::Internal::StageSchedule", "CxxStageSchedule")
        , ("Halide::Internal::Dim", "Language.Halide.Schedule.Dim")
        , ("Halide::Internal::Split", "Language.Halide.Schedule.Split")
        , ("halide_buffer_t", "Language.Halide.Buffer.RawHalideBuffer")
        , ("halide_device_interface_t", "HalideDeviceInterface")
        , ("Halide::Internal::Dimension", "CxxDimension")
        , ("Halide::LoopLevel", "CxxLoopLevel")
        , ("Halide::Stage", "CxxStage")
        , ("Halide::Buffer", "CxxBuffer")
        , ("Halide::Internal::FusedPair", "FusedPair")
        , ("Halide::Internal::ReductionVariable", "ReductionVariable")
        , ("Halide::Internal::PrefetchDirective", "PrefetchDirective")
        , ("halide_trace_event_t", "TraceEvent")
        ]
    optional :: (CIdentifier, String) -> Q [(CIdentifier, TypeQ)]
    optional (cName, hsName) = do
      hsType <- lookupTypeName hsName
      pure $ maybe [] (\x -> [(cName, pure (TH.ConT x))]) hsType
    optionals :: [(CIdentifier, String)] -> Q [(CIdentifier, TypeQ)]
    optionals pairs = concat <$> mapM optional pairs

defineExceptionHandler :: DecsQ
defineExceptionHandler =
  C.verbatim
    "\
    \template <class Func>                               \n\
    \auto handle_halide_exceptions(Func&& func) {        \n\
    \  try {                                             \n\
    \    return func();                                  \n\
    \  } catch(Halide::RuntimeError& e) {                \n\
    \    throw std::runtime_error{e.what()};             \n\
    \  } catch(Halide::CompileError& e) {                \n\
    \    throw std::runtime_error{e.what()};             \n\
    \  } catch(Halide::InternalError& e) {               \n\
    \    throw std::runtime_error{e.what()};             \n\
    \  } catch(Halide::Error& e) {                       \n\
    \    throw std::runtime_error{e.what()};             \n\
    \  }                                                 \n\
    \}                                                   \n\
    \                                                    \n\
    \template <class T>                                               \n\
    \auto to_string_via_iostream(T const& x) -> std::string* {        \n\
    \  std::ostringstream stream;                                     \n\
    \  stream << x;                                                   \n\
    \  return new std::string{stream.str()};                          \n\
    \}                                                                \n\
    \\n\
    \namespace Halide { namespace Internal {\n\
    \  std::string print_loop_nest(const std::vector<Function> &);\n\
    \} }\n\
    \"
