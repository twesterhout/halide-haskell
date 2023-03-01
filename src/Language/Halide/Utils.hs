{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Language.Halide.Utils
-- Description : Utilities for writing FFI code
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Utils
  ( peekCxxString
  , peekAndDeleteCxxString
  ) where

import Data.ByteString (packCString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Foreign.Ptr (Ptr)
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
import Language.Halide.Type

importHalide

-- | Convert a pointer to @std::string@ into a string.
--
-- It properly handles unicode characters.
peekCxxString :: Ptr CxxString -> IO Text
peekCxxString p =
  fmap T.decodeUtf8 $
    packCString
      =<< [CU.exp| char const* { $(const std::string* p)->c_str() } |]

-- | Call 'peekCxxString' and @delete@ the pointer.
peekAndDeleteCxxString :: Ptr CxxString -> IO Text
peekAndDeleteCxxString p = do
  s <- peekCxxString p
  [CU.exp| void { delete $(const std::string* p) } |]
  pure s
