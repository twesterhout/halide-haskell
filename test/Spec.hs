module Main (main) where

import Control.Exception (SomeException, catch)
import Data.Int
import Language.Halide.Buffer
import Language.Halide.Internal
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Hello" $
    it "World" $
      True `shouldBe` True
  describe "Expr" $
    it "has a Num instance" $ do
      evaluate ((1 :: Expr Int32) + 2) `shouldReturn` 3
      evaluate ((5 :: Expr Int32) - 2) `shouldReturn` 3
      evaluate (-(5 :: Expr Int32) * 2) `shouldReturn` (-10)
      evaluate (abs $ -(5 :: Expr Int32) * 2) `shouldReturn` 10

-- `shouldReturn` 3