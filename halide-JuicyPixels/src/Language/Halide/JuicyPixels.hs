-- |
-- Module      : Language.Halide.JuicyPixels
-- Copyright   : (c) Tom Westerhout, 2023
--
-- This package allows you to use [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels)
-- together with [halide-haskell](https://hackage.haskell.org/package/halide-haskell).
-- It defines 'Language.Halide.IsHalideBuffer' instances for 'Image' and
-- 'MutableImage' types from the JuicyPixels library.
--
-- That allows you to write code such as:
--
-- @
-- kernel :: Ptr ('HalideBuffer' 3 Word8) -> Ptr ('HalideBuffer' 3 Word8) -> IO ()
-- kernel = ...
--
-- brighten :: 'Image' 'PixelRGB8' -> 'MutableImage' 'RealWorld' 'PixelRGB8' -> IO ()
-- brighten input output = do
--   'withHalideBuffer' @3 @Word8 input $ \input' ->
--     'withHalideBuffer' @3 @Word8 output $ \output' ->
--       kernel input' output'
-- @
module Language.Halide.JuicyPixels ()
where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST (RealWorld)
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Language.Halide

instance (Pixel a, r ~ PixelBaseComponent a, IsHalideType r) => IsHalideBuffer (Image a) 3 r where
  withHalideBufferImpl :: Image a -> (Ptr (HalideBuffer 3 r) -> IO b) -> IO b
  withHalideBufferImpl im action =
    S.unsafeWith im.imageData $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [componentCount (undefined :: a), im.imageWidth, im.imageHeight] action

instance (Pixel a, r ~ PixelBaseComponent a, IsHalideType r) => IsHalideBuffer (MutableImage RealWorld a) 3 r where
  withHalideBufferImpl :: MutableImage RealWorld a -> (Ptr (HalideBuffer 3 r) -> IO b) -> IO b
  withHalideBufferImpl im action =
    SM.unsafeWith im.mutableImageData $ \cpuPtr ->
      bufferFromPtrShape
        cpuPtr
        [componentCount (undefined :: a), im.mutableImageWidth, im.mutableImageHeight]
        action
