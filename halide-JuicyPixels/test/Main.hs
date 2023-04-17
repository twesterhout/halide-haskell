{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Codec.Picture
import Codec.Picture.Types
import Data.Word (Word8)
import Language.Halide
import Language.Halide.JuicyPixels
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (min)

brighten :: Expr Float -> Parameter 3 Word8 -> IO (Function 3 Word8)
brighten factor input = do
  [x, y, c] <- mapM mkVar ["x", "y", "c"]
  let value = cast @Word8 . min 255 . (factor *) . cast @Float $ input ! (c, x, y)
  define "brighter" (c, x, y) value

main :: IO ()
main = do
  kernel <- compile brighten

  readImage "test/cat.jpg" >>= \case
    Right image -> do
      let rgb@(Image width height _) = convertRGB8 image
      output <- newMutableImage width height

      withHalideBuffer @3 @Word8 rgb $ \input ->
        withHalideBuffer @3 @Word8 output $ \output' ->
          kernel 2.5 input output'

      savePngImage "test.png" . ImageRGB8 =<< unsafeFreezeImage output
    Left e -> error e
