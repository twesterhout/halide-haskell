{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Language.Halide.LoopLevel
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.LoopLevel
  ( LoopLevel (..)
  , LoopLevelTy (..)
  , SomeLoopLevel (..)
  , LoopAlignStrategy (..)

    -- * Internal
  , CxxLoopLevel
  , withCxxLoopLevel
  , wrapCxxLoopLevel
  )
where

import Control.Exception (bracket)
import Data.Text (Text, unpack)
import Foreign.ForeignPtr
import Foreign.Marshal (toBool)
import Foreign.Ptr (Ptr)
import GHC.Records (HasField (..))
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp.Exception as C
import qualified Language.C.Inline.Unsafe as CU
import Language.Halide.Context
import Language.Halide.Expr
import Language.Halide.Type
import Language.Halide.Utils
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (min, tail)

-- | Haskell counterpart of @Halide::LoopLevel@
data CxxLoopLevel

importHalide

data LoopLevelTy = InlinedTy | RootTy | LockedTy

-- | A reference to a site in a Halide statement at the top of the body of a particular for loop.
data LoopLevel (t :: LoopLevelTy) where
  InlinedLoopLevel :: LoopLevel 'InlinedTy
  RootLoopLevel :: LoopLevel 'RootTy
  LoopLevel :: !(ForeignPtr CxxLoopLevel) -> LoopLevel 'LockedTy

data SomeLoopLevel where
  SomeLoopLevel :: LoopLevel t -> SomeLoopLevel

deriving stock instance Show SomeLoopLevel

instance Eq SomeLoopLevel where
  (SomeLoopLevel InlinedLoopLevel) == (SomeLoopLevel InlinedLoopLevel) = True
  (SomeLoopLevel RootLoopLevel) == (SomeLoopLevel RootLoopLevel) = True
  (SomeLoopLevel a@(LoopLevel _)) == (SomeLoopLevel b@(LoopLevel _)) = a == b
  _ == _ = False

instance Eq (LoopLevel t) where
  level1 == level2 =
    toBool . unsafePerformIO $
      withCxxLoopLevel level1 $ \l1 ->
        withCxxLoopLevel level2 $ \l2 ->
          [CU.exp| bool { *$(const Halide::LoopLevel* l1) == *$(const Halide::LoopLevel* l2) } |]

instance Show (LoopLevel t) where
  showsPrec _ InlinedLoopLevel = showString "InlinedLoopLevel"
  showsPrec _ RootLoopLevel = showString "RootLoopLevel"
  showsPrec d level@(LoopLevel _) =
    showParen (d > 10) $
      showString "LoopLevel {func = "
        . shows (level.func :: Text)
        . showString ", var = "
        . shows (level.var :: Expr Int32)
        . showString "}"

-- desc
--   where
--     desc = unpack . unsafePerformIO $
--       withCxxLoopLevel level $ \l ->
--         peekAndDeleteCxxString
--           =<< [C.throwBlock| std::string* {
--                 return handle_halide_exceptions([=](){
--                   return new std::string{$(const Halide::LoopLevel* l)->to_string()};
--                 });
--               } |]

-- | Different ways to handle the case when the start/end of the loops of stages computed with (fused)
-- are not aligned.
data LoopAlignStrategy
  = -- | Shift the start of the fused loops to align.
    LoopAlignStart
  | -- | Shift the end of the fused loops to align.
    LoopAlignEnd
  | -- | 'computeWith' will make no attempt to align the start/end of the fused loops.
    LoopNoAlign
  | -- | By default, LoopAlignStrategy is set to 'LoopNoAlign'.
    LoopAlignAuto
  deriving stock (Show, Eq, Ord)

instance Enum LoopAlignStrategy where
  fromEnum =
    fromIntegral . \case
      LoopAlignStart -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignStart) } |]
      LoopAlignEnd -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignEnd) } |]
      LoopNoAlign -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::NoAlign) } |]
      LoopAlignAuto -> [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::Auto) } |]
  toEnum k
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignStart) } |] = LoopAlignStart
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::AlignEnd) } |] = LoopAlignEnd
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::NoAlign) } |] = LoopNoAlign
    | fromIntegral k == [CU.pure| int { static_cast<int>(Halide::LoopAlignStrategy::Auto) } |] = LoopAlignAuto
    | otherwise = error $ "invalid LoopAlignStrategy: " <> show k

isInlined :: LoopLevel t -> Bool
isInlined InlinedLoopLevel = True
isInlined _ = False

isRoot :: LoopLevel t -> Bool
isRoot RootLoopLevel = True
isRoot _ = False

instance HasField "func" (LoopLevel 'LockedTy) Text where
  getField level = unsafePerformIO $
    withCxxLoopLevel level $ \level' ->
      peekAndDeleteCxxString
        =<< [CU.exp| std::string* {
              new std::string{$(const Halide::LoopLevel* level')->func()} } |]

instance HasField "var" (LoopLevel 'LockedTy) (Expr Int32) where
  getField level = unsafePerformIO $
    withCxxLoopLevel level $ \level' ->
      wrapCxxVarOrRVar
        =<< [CU.exp| Halide::VarOrRVar* {
              new Halide::VarOrRVar{$(const Halide::LoopLevel* level')->var()} } |]

wrapCxxLoopLevel :: Ptr CxxLoopLevel -> IO SomeLoopLevel
wrapCxxLoopLevel p = do
  [C.throwBlock| void { handle_halide_exceptions([=]() { $(Halide::LoopLevel* p)->lock(); }); } |]
  inlined <-
    toBool
      <$> [C.throwBlock| bool {
            return handle_halide_exceptions([=](){
              return $(const Halide::LoopLevel* p)->is_inlined(); });
          } |]
  root <-
    toBool
      <$> [C.throwBlock| bool {
            return handle_halide_exceptions([=](){
              return $(const Halide::LoopLevel* p)->is_root(); });
          } |]
  let level
        | inlined = [CU.exp| void { delete $(Halide::LoopLevel *p) } |] >> pure (SomeLoopLevel InlinedLoopLevel)
        | root = [CU.exp| void { delete $(Halide::LoopLevel *p) } |] >> pure (SomeLoopLevel RootLoopLevel)
        | otherwise = do
            let deleter = [C.funPtr| void deleteLoopLevel(Halide::LoopLevel* p) { delete p; } |]
            SomeLoopLevel . LoopLevel <$> newForeignPtr deleter p
  level

withCxxLoopLevel :: LoopLevel t -> (Ptr CxxLoopLevel -> IO a) -> IO a
withCxxLoopLevel (LoopLevel fp) action = withForeignPtr fp action
withCxxLoopLevel level action = do
  let allocate
        | isInlined level = [CU.exp| Halide::LoopLevel* { new Halide::LoopLevel{Halide::LoopLevel::inlined()} } |]
        | isRoot level = [CU.exp| Halide::LoopLevel* { new Halide::LoopLevel{Halide::LoopLevel::root()} } |]
        | otherwise = error "this should never happen"
      destroy p = [CU.exp| void { delete $(Halide::LoopLevel *p) } |]
  bracket allocate destroy action
