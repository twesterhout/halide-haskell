module Main (main) where

import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import Language.Halide
import System.IO.Unsafe (unsafePerformIO)

mkVectorPlus :: forall a. (IsHalideType a, Num a) => IO (S.Vector a -> S.Vector a -> S.Vector a)
mkVectorPlus = do
  -- First, compile the kernel
  kernel <- compile $ \(buffer "a" -> a) b -> do
    -- Create an index variable
    i <- mkVar "i"
    -- Define the resulting function. We call it "out".
    -- In pseudocode it's equivalent to the following: out[i] = a[i] + b[i]
    define "out" i $ a ! i + b ! i
  -- Create a Haskell function that will invoke the kernel
  pure $ \v1 v2 -> unsafePerformIO $ do
    out <- SM.new (S.length v1)
    withHalideBuffer @1 @a v1 $ \a ->
      withHalideBuffer @1 @a v2 $ \b ->
        withHalideBuffer @1 @a out $ \out' ->
          kernel a b out'
    S.unsafeFreeze out

main :: IO ()
main = do
  let a, b :: S.Vector Float
      a = S.fromList [1, 2, 3]
      b = S.fromList [4, 5, 6]
  vectorPlus <- mkVectorPlus
  print (vectorPlus a b)
