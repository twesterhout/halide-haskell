module Language.Halide.Tutorial01Spec (spec) where

import Language.Halide
import Test.Hspec

spec :: Spec
spec = do
  describe "Tutorial 1" $ do
    it "" $ do
      -- This program defines a single-stage imaging pipeline that
      -- outputs a grayscale diagonal gradient.
      --
      -- Var objects are names to use as variables in the definition of a Func.
      -- They have no meaning by themselves. In the following, we define two Vars
      -- named "x" and "y". These names will be used internally by Halide during code
      -- generation.
      x <- mkVar "x"
      y <- mkVar "y"
      -- A Func object represents a pipeline stage. It's a pure function that defines
      -- what value each pixel should have. You can think of it as a computed image.
      --
      -- Funcs are defined at any integer coordinate of its variables as an Expr in
      -- terms of those variables and other functions. Here, we'll define an Expr which
      -- has the value x + y.
      let e :: Expr Int32
          e = x + y
      -- Now we'll add a definition for the Func object. At pixel x, y, the image will
      -- have the value of the Expr e.
      gradient <- define "gradient" (x, y) e
      -- Now we 'realize' the Func, which JIT compiles some code that implements the pipeline
      -- we've defined, and then runs it.  We also need to tell Halide the domain over which
      -- to evaluate the Func, which determines the range of x and y above, and the resolution
      -- of the output image. Since there's no preferred multi-dimensional array data type in
      -- Haskell, Halide gives us a pointer to the internal buffer such that we can convert it to
      -- our data type of choice. Here, we simply cast it into a list [[Int32]] and compare the
      -- result against a simple list comprehension.
      realize gradient [20, 10] $ \buf ->
        peekToList buf `shouldReturn` [[x' + y' | y' <- [0 .. 9]] | x' <- [0 .. 19]]
