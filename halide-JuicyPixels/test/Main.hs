{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Codec.Picture
import Codec.Picture.Types
import Data.Word (Word8)
import Language.Halide
import Language.Halide.JuicyPixels
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (min)

brighten :: Ptr (HalideBuffer 3 Word8) -> Ptr (HalideBuffer 3 Word8) -> IO ()
brighten = unsafePerformIO . compile $ \input -> do
  [x, y, c] <- mapM mkVar ["x", "y", "c"]
  let value = cast @Word8 . min 255 . (1.5 *) . cast @Float $ input ! (c, x, y)
  define "brighter" (c, x, y) value
{-# NOINLINE brighten #-}

main :: IO ()
main = do
  readImage "cat.jpg" >>= \case
    Left e -> error e
    Right image -> do
      let rgb@(Image width height _) = convertRGB8 image
      output <- newMutableImage width height

      withHalideBuffer @3 @Word8 (HalideImage rgb) $ \input ->
        withHalideBuffer @3 @Word8 (HalideMutableImage output) $ \output' ->
          brighten input output'

      savePngImage "test.png" . ImageRGB8 =<< unsafeFreezeImage output
