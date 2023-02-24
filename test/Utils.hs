module Utils
  ( shouldContainText
  , shouldNotContainText
  , appearsBeforeText
  , shouldApproxBe
  , testOnGpu
  , approx
  , approx'
  , (&)
  , void
  , T.hPutStrLn
  , stderr
  , HasEpsilon
  , eps
  , showInCodeLenses
  )
where

import Control.Monad (void)
import Data.Function ((&))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Halide.Target
import System.IO (stderr)
import Test.HUnit (assertFailure)
import Test.Hspec

shouldContainText :: Text -> Text -> Expectation
a `shouldContainText` b = T.unpack a `shouldContain` T.unpack b

shouldNotContainText :: Text -> Text -> Expectation
a `shouldNotContainText` b = T.unpack a `shouldNotContain` T.unpack b

appearsBeforeText :: Text -> Text -> Text -> Expectation
appearsBeforeText a b t = do
  t `shouldContainText` b
  fst (T.breakOn b t) `shouldContainText` a

testOnGpu :: (Target -> Expectation) -> Expectation
testOnGpu f =
  case gpuTarget of
    Just t -> f t
    Nothing -> pendingWith "no GPU target available"

class Num a => HasEpsilon a where
  eps :: a

instance HasEpsilon Float where
  eps = 1.1920929e-7

instance HasEpsilon Double where
  eps = 2.220446049250313e-16

approx :: (Ord a, Num a) => a -> a -> a -> a -> Bool
approx rtol atol a b = abs (a - b) <= max atol (rtol * max (abs a) (abs b))

approx' :: (Ord a, HasEpsilon a) => a -> a -> Bool
approx' a b = approx (2 * eps * max (abs a) (abs b)) (4 * eps) a b

shouldApproxBe :: (Ord a, Num a, Show a) => a -> a -> a -> a -> Expectation
shouldApproxBe rtol atol a b
  | approx rtol atol a b = pure ()
  | otherwise = assertFailure $ "expected " <> show a <> ", but got " <> show b

showInCodeLenses :: Text -> IO String
showInCodeLenses v = error (unpack v)
