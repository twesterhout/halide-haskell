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

newtype HalideImage a = HalideImage {unHalideImage :: Image a}

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

-- realizeToImage

halideWithImage
  :: (Pixel a, IsHalideType (PixelBaseComponent a))
  => Ptr (HalideBuffer 3 (PixelBaseComponent a))
  -> (Image a -> IO b)
  -> IO b
halideWithImage = undefined

-- halideWithImage
--   :: (Pixel a, IsHalideType (PixelBaseComponent a))
--   => Ptr (HalideBuffer 3 (PixelBaseComponent a))
--   -> (Image a -> IO b)
--   -> IO b
-- halideWithImage = undefined
