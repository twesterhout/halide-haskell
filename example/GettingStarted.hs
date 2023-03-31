module Main (main) where

import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as SM
import GHC.Exts
import Language.Halide
import System.IO.Unsafe (unsafePerformIO)

mkVectorPlus :: forall a. (IsHalideType a, Num a) => IO (S.Vector a -> S.Vector a -> S.Vector a)
mkVectorPlus = do
  -- First, compile the kernel
  kernel <- compile $ \a b -> do
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

data E = E

data Ty = A | B

data D (a :: Ty) where
  DA :: D 'A
  DB :: D 'B

foo :: D t -> IO ()
foo DA = putStrLn "DA"
foo DB = putStrLn "DB"

bar :: E -> IO ()
bar _ = putStrLn "E"

class Call f where
  call :: f -> IO ()

instance Call r => Call (E -> r) where call f = call (f E)

instance (t ~ 'B, Call r) => Call (D t -> r) where call f = call (f DB)

instance Call (IO ()) where call f = f

default (Int, Float)

run :: IO ()
run = call $ \a b -> do
  bar a
  foo b

main :: IO ()
main = do
  let a, b :: S.Vector Float
      a = S.fromList [1, 2, 3]
      b = S.fromList [4, 5, 6]
  vectorPlus <- mkVectorPlus
  print (vectorPlus a b)
