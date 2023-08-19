{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE UndecidableInstances #-}

module Utils
  ( shouldContainText
  , shouldNotContainText
  , appearsBeforeText
  , shouldBeApprox
  , shouldBeEqForTesting
  , testOnGpu
  , approx
  , approxWith
  , (&)
  , void
  , T.hPutStrLn
  , stderr
  , HasEpsilon
  , eps
  , showInCodeLenses
  , EqForTesting (..)
  )
where

import Control.Exception (throwIO)
import Control.Monad (unless, void)
import Data.Function ((&))
import Data.Text (Text, unpack)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import GHC.Exts (IsList (..))
import GHC.Stack
import Language.Halide hiding (and, max)
import System.IO (stderr)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (..))
import Test.Hspec
import Prelude hiding (Eq (..))
import Prelude qualified

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

compareWith :: (HasCallStack, Show a) => (a -> a -> Bool) -> a -> a -> Expectation
compareWith comparator result expected =
  unless (comparator result expected) $ do
    throwIO (HUnitFailure location $ ExpectedButGot Nothing expectedMsg actualMsg)
  where
    expectedMsg = show expected
    actualMsg = show result
    location = case reverse (toList callStack) of
      (_, loc) : _ -> Just loc
      [] -> Nothing

class Num a => HasEpsilon a where
  eps :: a

instance HasEpsilon Float where
  eps = 1.1920929e-7

instance HasEpsilon Double where
  eps = 2.220446049250313e-16

approxWith :: (Ord a, Num a) => a -> a -> a -> a -> Bool
approxWith rtol atol a b = abs (a - b) <= max atol (rtol * max (abs a) (abs b))

approx :: (Ord a, HasEpsilon a) => a -> a -> Bool
approx a b = approxWith (2 * eps * max (abs a) (abs b)) (4 * eps) a b

shouldBeApprox :: (Ord a, HasEpsilon a, Show a) => a -> a -> Expectation
shouldBeApprox = compareWith approx

shouldBeEqForTesting :: (HasCallStack, EqForTesting a, Show a) => a -> a -> Expectation
shouldBeEqForTesting = compareWith equalForTesting

showInCodeLenses :: Text -> IO String
showInCodeLenses v = error (unpack v)

class EqForTesting a where
  equalForTesting :: a -> a -> Bool
  default equalForTesting :: Prelude.Eq a => a -> a -> Bool
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
      [ a.splitOld == b.splitOld
      , a.splitOuter == b.splitOuter
      , a.splitInner == b.splitInner
      , a.splitFactor `equalForTesting` b.splitFactor
      , a.splitExact == b.splitExact
      , a.splitTail == b.splitTail
      ]

instance EqForTesting Split where
  (SplitVar a) `equalForTesting` (SplitVar b) = a `equalForTesting` b
  (FuseVars a) `equalForTesting` (FuseVars b) = a == b
  _ `equalForTesting` _ = False

instance EqForTesting ReductionVariable where
  a `equalForTesting` b = a.varName == b.varName && a.minExpr `equalForTesting` b.minExpr && a.extentExpr `equalForTesting` b.extentExpr

instance EqForTesting PrefetchDirective where
  a `equalForTesting` b =
    and
      [ a.prefetchFunc == b.prefetchFunc
      , a.prefetchAt == b.prefetchAt
      , a.prefetchFrom == b.prefetchFrom
      , a.prefetchOffset `equalForTesting` b.prefetchOffset
      , a.prefetchStrategy == b.prefetchStrategy
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
