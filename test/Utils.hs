{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

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
  , EqForTesting (..)
  , shouldBeEqForTesting
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, void)
import Data.Function ((&))
import Data.Text (Text, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Exts (IsList (..))
import GHC.Stack
import Language.Halide.Expr
import Language.Halide.Schedule
import Language.Halide.Target
import System.IO (stderr)
import Test.HUnit
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
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

class EqForTesting a where
  equalForTesting :: a -> a -> Bool
  default equalForTesting :: Eq a => a -> a -> Bool
  a `equalForTesting` b = a == b

instance EqForTesting a => EqForTesting [a] where
  as `equalForTesting` bs = and $ zipWith equalForTesting as bs

instance EqForTesting (Expr Int32) where
  a `equalForTesting` b
    | (Just aInt, Just bInt) <- (toIntImm a, toIntImm b) = aInt == bInt
    | otherwise = show a == show b

instance EqForTesting SplitContents where
  a `equalForTesting` b =
    and
      [ a.old == b.old
      , a.outer == b.outer
      , a.inner == b.inner
      , a.factor `equalForTesting` b.factor
      , a.exact == b.exact
      , a.tail == b.tail
      ]

instance EqForTesting Split where
  (SplitVar a) `equalForTesting` (SplitVar b) = a `equalForTesting` b
  (FuseVars a) `equalForTesting` (FuseVars b) = a == b
  _ `equalForTesting` _ = False

instance EqForTesting ReductionVariable where
  a `equalForTesting` b = a.var == b.var && a.min `equalForTesting` b.min && a.extent `equalForTesting` b.extent

instance EqForTesting PrefetchDirective where
  a `equalForTesting` b =
    and
      [ a.funcName == b.funcName
      , a.atVar == b.atVar
      , a.fromVar == b.fromVar
      , a.offset `equalForTesting` b.offset
      , a.strategy == b.strategy
      ]

instance EqForTesting StageSchedule where
  a `equalForTesting` b =
    and
      [ a.rvars `equalForTesting` b.rvars
      , a.dims == b.dims
      , a.prefetches `equalForTesting` b.prefetches
      , a.fuseLevel == b.fuseLevel
      , a.fusedPairs == b.fusedPairs
      , a.allowRaceConditions == b.allowRaceConditions
      ]

shouldBeEqForTesting :: (HasCallStack, EqForTesting a, Show a) => a -> a -> Expectation
shouldBeEqForTesting actual expected =
  unless (actual `equalForTesting` expected) $ do
    throwIO (HUnitFailure location $ ExpectedButGot Nothing expectedMsg actualMsg)
  where
    expectedMsg = show expected
    actualMsg = show actual
    location = case reverse (toList callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing
