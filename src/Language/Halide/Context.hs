{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      : Language.Halide.Context
-- Description : Helpers to setup inline-c context for Halide
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Context
  ( importHalide
  , handleHalideExceptions
  , handleHalideExceptionsM
  )
where

import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack (HasCallStack)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Cpp.Exception as C
import Language.Halide.Buffer
import Language.Halide.Type
import Language.Haskell.TH (DecsQ)

-- | One stop function to include all the neccessary machinery to call Halide
-- functions via inline-c.
importHalide :: DecsQ
importHalide =
  concat
    <$> sequence
      [ C.context halideCxt
      , C.include "<Halide.h>"
      , defineExceptionHandler
      ]

-- | Convert Halide C++ exceptions into calls to 'error'.
--
-- Normally, you would use it like this:
--
-- > halideHandleExceptions
-- >   =<< [C.tryBlock| void {
-- >         handle_halide_exceptions([=]() {
-- >           Halide::Func f;
-- >           f() = *$(Halide::Expr* e);
-- >           f.realize(Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)});
-- >         });
-- >       } |]
handleHalideExceptions :: HasCallStack => Either C.CppException a -> IO a
handleHalideExceptions (Right x) = pure x
handleHalideExceptions (Left (C.CppStdException _ msg _)) = error $ unpack (decodeUtf8 msg)
handleHalideExceptions (Left err) = error $ "Halide error: " <> show err

-- | Similar to 'handleHalideExceptions' but takes a monadic action.
handleHalideExceptionsM :: HasCallStack => IO (Either C.CppException a) -> IO a
handleHalideExceptionsM action = action >>= handleHalideExceptions

-- | Define @inline-c@ context for Halide types.
halideCxt :: C.Context
halideCxt =
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> C.cppTypePairs
      [ ("Halide::Expr", [t|CxxExpr|])
      , ("Halide::Var", [t|CxxVar|])
      , ("Halide::RVar", [t|CxxRVar|])
      , ("Halide::VarOrRVar", [t|CxxVarOrRVar|])
      , ("Halide::Func", [t|CxxFunc|])
      , ("Halide::Internal::Parameter", [t|CxxParameter|])
      , ("Halide::ImageParam", [t|CxxImageParam|])
      , ("Halide::Callable", [t|CxxCallable|])
      , ("Halide::JITUserContext", [t|CxxUserContext|])
      , ("Halide::Argument", [t|CxxArgument|])
      , ("std::vector", [t|CxxVector|])
      , ("halide_buffer_t", [t|RawHalideBuffer|])
      , ("halide_type_t", [t|HalideType|])
      ]

-- | Define a C++ function @halide_handle_exception@ that converts Halide
-- exceptions into @std::runtime_error@. It can be used inside 'C.tryBlock' or
-- 'C.catchBlock' to properly re-throw Halide errors (otherwise we'll get a
-- call to @std::terminate@).
--
-- E.g.
--
--    [C.catchBlock| void {
--      handle_halide_exceptions([=]() {
--        Halide::Func f;
--        Halide::Var i;
--        f(i) = *$(Halide::Expr* e);
--        f.realize(Halide::Pipeline::RealizationArg{$(halide_buffer_t* b)});
--      });
--    } |]
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
    \"