-- |
-- Module      : Language.Halide.JuicyPixels
-- Copyright   : (c) Tom Westerhout, 2023
--
-- This package allows you to use [JuicyPixels](https://hackage.haskell.org/package/JuicyPixels)
-- together with [halide-haskell](https://hackage.haskell.org/package/halide-haskell).
-- It defines 'HalideImage' and 'HalideMutableImage' newtypes and defines instances of
-- 'Language.Halide.IsHalideBuffer' for them.
--
-- That allows you to write code such as:
--
-- @
-- kernel :: Ptr ('HalideBuffer' 3 Word8) -> Ptr ('HalideBuffer' 3 Word8) -> IO ()
-- kernel = ...
--
-- brighten :: 'Image' 'PixelRGB8' -> 'MutableImage' 'RealWorld' 'PixelRGB8' -> IO ()
-- brighten input output = do
--   'withHalideBuffer' @3 @Word8 ('HalideImage' input) $ \input' ->
--     'withHalideBuffer' @3 @Word8 ('HalideMutableImage' output) $ \output' ->
--       brighten input' output'
-- @
module Language.Halide.JuicyPixels
  ( HalideImage (..)
  , HalideMutableImage (..)
  )
where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST (RealWorld)
import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Language.Halide

-- | A newtype wrapper around 'Image'.
--
-- It's required to avoid an orphan instance of 'IsHalideBuffer'.
newtype HalideImage a = HalideImage {unHalideImage :: Image a}

-- | A newtype wrapper around 'MutableImage'.
--
-- It's required to avoid an orphan instance of 'IsHalideBuffer'.
newtype HalideMutableImage s a = HalideMutableImage {unHalideMutableImage :: MutableImage s a}

instance (Pixel a, r ~ PixelBaseComponent a, IsHalideType r) => IsHalideBuffer (HalideImage a) 3 r where
  withHalideBufferImpl :: HalideImage a -> (Ptr (HalideBuffer 3 r) -> IO b) -> IO b
  withHalideBufferImpl (HalideImage im) action =
    S.unsafeWith im.imageData $ \cpuPtr ->
      bufferFromPtrShape cpuPtr [componentCount (undefined :: a), im.imageWidth, im.imageHeight] action

instance (Pixel a, r ~ PixelBaseComponent a, IsHalideType r) => IsHalideBuffer (HalideMutableImage RealWorld a) 3 r where
  withHalideBufferImpl :: HalideMutableImage RealWorld a -> (Ptr (HalideBuffer 3 r) -> IO b) -> IO b
  withHalideBufferImpl (HalideMutableImage im) action =
    SM.unsafeWith im.mutableImageData $ \cpuPtr ->
      bufferFromPtrShape
        cpuPtr
        [componentCount (undefined :: a), im.mutableImageWidth, im.mutableImageHeight]
        action
