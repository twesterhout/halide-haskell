{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Halide.Type
  ( HalideTypeCode (..),
    HalideType (..),
    IsHalideType (..),
    CxxExpr (..),
    Castable (..),
  )
where

import Control.Monad (when)
import Data.Coerce
import Data.Int
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

data CxxExpr

C.context $
  C.cppCtx
    <> C.cppTypePairs
      [ ("Halide::Expr", [t|CxxExpr|])
      ]

C.include "<Halide.h>"

data HalideTypeCode
  = HalideTypeInt
  | HalideTypeUInt
  | HalideTypeFloat
  | HalideTypeHandle
  | HalideTypeBfloat
  deriving stock (Read, Show, Eq)

instance Enum HalideTypeCode where
  fromEnum :: HalideTypeCode -> Int
  fromEnum x = case x of
    HalideTypeInt -> 0
    HalideTypeUInt -> 1
    HalideTypeFloat -> 2
    HalideTypeHandle -> 3
    HalideTypeBfloat -> 4
  toEnum :: Int -> HalideTypeCode
  toEnum x = case x of
    0 -> HalideTypeInt
    1 -> HalideTypeUInt
    2 -> HalideTypeFloat
    3 -> HalideTypeHandle
    4 -> HalideTypeBfloat
    _ -> error $ "invalid HalideTypeCode: " <> show x

data HalideType = HalideType
  { halideTypeCode :: !HalideTypeCode,
    halideTypeBits :: {-# UNPACK #-} !Word8,
    halideTypeLanes :: {-# UNPACK #-} !Word16
  }
  deriving stock (Read, Show, Eq)

instance Storable HalideType where
  sizeOf :: HalideType -> Int
  sizeOf _ = 4
  alignment :: HalideType -> Int
  alignment _ = 4
  peek :: Ptr HalideType -> IO HalideType
  peek p =
    HalideType
      <$> (toEnum . (fromIntegral :: Word8 -> Int) <$> peekByteOff p 0)
      <*> peekByteOff p 1
      <*> peekByteOff p 2
  poke :: Ptr HalideType -> HalideType -> IO ()
  poke p (HalideType code bits lanes) = do
    pokeByteOff p 0 . (fromIntegral :: Int -> Word8) . fromEnum $ code
    pokeByteOff p 1 bits
    pokeByteOff p 2 lanes

-- instance IsCxxExpr $(C.getHaskellType False cType) where
--   toCxxExpr :: $(C.getHaskellType False cType) -> IO (Ptr CxxExpr)
--   toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(@T() x)} } |]

class IsHalideType a where
  halideTypeFor :: proxy a -> HalideType
  toCxxExpr :: a -> IO (Ptr CxxExpr)

instance IsHalideType CFloat where
  halideTypeFor :: proxy CFloat -> HalideType
  halideTypeFor _ = HalideType HalideTypeFloat 32 1
  toCxxExpr :: CFloat -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(float x)} } |]

instance IsHalideType Float where
  halideTypeFor :: proxy Float -> HalideType
  halideTypeFor _ = halideTypeFor (Proxy :: Proxy CFloat)
  toCxxExpr :: Float -> IO (Ptr CxxExpr)
  toCxxExpr x = toCxxExpr (coerce x :: CFloat)

instance IsHalideType CDouble where
  halideTypeFor :: proxy CDouble -> HalideType
  halideTypeFor _ = HalideType HalideTypeFloat 64 1
  toCxxExpr :: CDouble -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(double x)} } |]

instance IsHalideType Double where
  halideTypeFor :: proxy Double -> HalideType
  halideTypeFor _ = halideTypeFor (Proxy :: Proxy CDouble)
  toCxxExpr :: Double -> IO (Ptr CxxExpr)
  toCxxExpr x = toCxxExpr (coerce x :: CDouble)

instance IsHalideType Bool where
  halideTypeFor :: proxy Bool -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 1 1
  toCxxExpr :: Bool -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(bool c_x)} } |]
    where
      c_x = fromIntegral $ fromEnum x

instance IsHalideType Word8 where
  halideTypeFor :: proxy Word8 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 8 1
  toCxxExpr :: Word8 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint8_t x)} } |]

instance IsHalideType Word16 where
  halideTypeFor :: proxy Word16 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 16 1
  toCxxExpr :: Word16 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint16_t x)} } |]

instance IsHalideType Word32 where
  halideTypeFor :: proxy Word32 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 32 1
  toCxxExpr :: Word32 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint32_t x)} } |]

instance IsHalideType Word64 where
  halideTypeFor :: proxy Word64 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 64 1
  toCxxExpr :: Word64 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint64_t x)} } |]

instance IsHalideType Int8 where
  halideTypeFor :: proxy Int8 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 8 1
  toCxxExpr :: Int8 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int8_t x)} } |]

instance IsHalideType Int16 where
  halideTypeFor :: proxy Int16 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 16 1
  toCxxExpr :: Int16 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int16_t x)} } |]

instance IsHalideType Int32 where
  halideTypeFor :: proxy Int32 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 32 1
  toCxxExpr :: Int32 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int32_t x)} } |]

instance IsHalideType Int64 where
  halideTypeFor :: proxy Int64 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 64 1
  toCxxExpr :: Int64 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int64_t x)} } |]

class (IsHalideType a, IsHalideType b) => Castable a b where
  castImpl :: proxy a -> proxy b -> Ptr CxxExpr -> IO (Ptr CxxExpr)
