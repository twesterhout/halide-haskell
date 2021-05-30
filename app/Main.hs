{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import Control.Monad.ST
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Data.Vector.Storable (MVector (..), Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Foreign.ForeignPtr
import Foreign.Ptr (Ptr)
import Foreign.Storable
import Halide

instance (Storable a, IsHalideType a) => IsHalideBuffer (Vector a) where
  withHalideBuffer v action = V.unsafeWith v $ \p ->
    bufferFromPtrShape p [V.length v] action

instance IsHalideType a => IsHalideBuffer (MVector s a) where
  withHalideBuffer (MVector n fp) action = withForeignPtr fp $ \p ->
    bufferFromPtrShape p [n] action

foreign import ccall "foo"
  c_foo :: Float -> Ptr HalideBuffer -> Ptr HalideBuffer -> IO ()

foo :: Float -> Vector Float -> Vector Float
foo α x = runST $ do
  y <- MV.unsafeNew (V.length x)
  unsafeIOToST $
    withHalideBuffer x $ \xBuffer ->
      withHalideBuffer y $ \yBuffer ->
        c_foo α xBuffer yBuffer
  V.unsafeFreeze y

main :: IO ()
main = do
  let x = V.generate 10 fromIntegral :: Vector Float
  print $ foo 0.5 x
