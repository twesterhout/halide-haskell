{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Playground (awesomeSequence) where

import Language.Halide

awesomeSequence :: IO (Ptr (HalideBuffer 1 Int32) -> IO ())
awesomeSequence = do
  i <- mkVar "i"
  compile $ do
    define "sequence" i (i * i)
