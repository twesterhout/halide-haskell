{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Halide.Type
  ( HalideTypeCode (..),
    HalideType (..),
    IsHalideType (..),
    Castable (..),
    CxxExpr,
    CxxFunc,
    CxxParam,
    CxxParameter,
    CxxArgument,
    CxxImageParam,
    CxxVector,
    CxxUserContext,
    CxxCallable,
  )
where

-- import Control.Monad (when)
import Data.Coerce
import Data.Int
import Data.Kind (Type)
import Data.Proxy
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline.Unsafe as CU

data CxxExpr

data CxxParam a

data CxxParameter

data CxxArgument

data CxxImageParam

data CxxFunc

data CxxUserContext

data CxxCallable

data CxxVector a

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

C.context $
  C.cppCtx
    <> C.fptrCtx
    <> C.bsCtx
    <> C.cppTypePairs
      [ ("Halide::Expr", [t|CxxExpr|]),
        ("Halide::Param", [t|CxxParam|]),
        ("halide_type_t", [t|HalideType|])
      ]

C.include "<Halide.h>"

class IsHalideType a where
  type CxxType a :: Type
  halideTypeFor :: proxy a -> HalideType
  toCxxExpr :: a -> IO (Ptr CxxExpr)
  toCxxParam :: a -> IO (Ptr (CxxParam (CxxType a)))

class (IsHalideType to, IsHalideType from) => Castable to from where
  castImpl :: proxy to -> proxy from -> Ptr CxxExpr -> IO (Ptr CxxExpr)

instance IsHalideType CFloat where
  type CxxType CFloat = CFloat
  halideTypeFor :: proxy CFloat -> HalideType
  halideTypeFor _ = HalideType HalideTypeFloat 32 1
  toCxxExpr :: CFloat -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(float x)} } |]
  toCxxParam :: CFloat -> IO (Ptr (CxxParam CFloat))
  toCxxParam x = [CU.exp| Halide::Param<float>* { new Halide::Param<float>{$(float x)} } |]

instance IsHalideType Float where
  type CxxType Float = CFloat
  halideTypeFor :: proxy Float -> HalideType
  halideTypeFor _ = halideTypeFor (Proxy :: Proxy CFloat)
  toCxxExpr :: Float -> IO (Ptr CxxExpr)
  toCxxExpr x = toCxxExpr (coerce x :: CFloat)
  toCxxParam :: Float -> IO (Ptr (CxxParam CFloat))
  toCxxParam x = toCxxParam (coerce x :: CFloat)

instance IsHalideType CDouble where
  type CxxType CDouble = CDouble
  halideTypeFor :: proxy CDouble -> HalideType
  halideTypeFor _ = HalideType HalideTypeFloat 64 1
  toCxxExpr :: CDouble -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(double x)} } |]
  toCxxParam :: CDouble -> IO (Ptr (CxxParam CDouble))
  toCxxParam x = [CU.exp| Halide::Param<double>* { new Halide::Param<double>{$(double x)} } |]

instance IsHalideType Double where
  type CxxType Double = CDouble
  halideTypeFor :: proxy Double -> HalideType
  halideTypeFor _ = halideTypeFor (Proxy :: Proxy CDouble)
  toCxxExpr :: Double -> IO (Ptr CxxExpr)
  toCxxExpr x = toCxxExpr (coerce x :: CDouble)
  toCxxParam :: Double -> IO (Ptr (CxxParam CDouble))
  toCxxParam x = toCxxParam (coerce x :: CDouble)

-- instance IsHalideType Bool where
--   type CxxType Bool = CBool
--   halideTypeFor :: proxy Bool -> HalideType
--   halideTypeFor _ = HalideType HalideTypeUInt 1 1
--   toCxxExpr :: Bool -> IO (Ptr CxxExpr)
--   toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(bool c_x)} } |]
--     where
--       c_x = fromIntegral $ fromEnum x

instance IsHalideType Word8 where
  type CxxType Word8 = Word8
  halideTypeFor :: proxy Word8 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 8 1
  toCxxExpr :: Word8 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint8_t x)} } |]
  toCxxParam :: Word8 -> IO (Ptr (CxxParam Word8))
  toCxxParam x = [CU.exp| Halide::Param<uint8_t>* { new Halide::Param<uint8_t>{$(uint8_t x)} } |]

instance IsHalideType Word16 where
  type CxxType Word16 = Word16
  halideTypeFor :: proxy Word16 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 16 1
  toCxxExpr :: Word16 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint16_t x)} } |]
  toCxxParam :: Word16 -> IO (Ptr (CxxParam Word16))
  toCxxParam x = [CU.exp| Halide::Param<uint16_t>* { new Halide::Param<uint16_t>{$(uint16_t x)} } |]

instance IsHalideType Word32 where
  type CxxType Word32 = Word32
  halideTypeFor :: proxy Word32 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 32 1
  toCxxExpr :: Word32 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint32_t x)} } |]
  toCxxParam :: Word32 -> IO (Ptr (CxxParam Word32))
  toCxxParam x = [CU.exp| Halide::Param<uint32_t>* { new Halide::Param<uint32_t>{$(uint32_t x)} } |]

instance IsHalideType Word64 where
  type CxxType Word64 = Word64
  halideTypeFor :: proxy Word64 -> HalideType
  halideTypeFor _ = HalideType HalideTypeUInt 64 1
  toCxxExpr :: Word64 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(uint64_t x)} } |]
  toCxxParam :: Word64 -> IO (Ptr (CxxParam Word64))
  toCxxParam x = [CU.exp| Halide::Param<uint64_t>* { new Halide::Param<uint64_t>{$(uint64_t x)} } |]

instance IsHalideType Int8 where
  type CxxType Int8 = Int8
  halideTypeFor :: proxy Int8 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 8 1
  toCxxExpr :: Int8 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int8_t x)} } |]
  toCxxParam :: Int8 -> IO (Ptr (CxxParam Int8))
  toCxxParam x = [CU.exp| Halide::Param<int8_t>* { new Halide::Param<int8_t>{$(int8_t x)} } |]

instance IsHalideType Int16 where
  type CxxType Int16 = Int16
  halideTypeFor :: proxy Int16 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 16 1
  toCxxExpr :: Int16 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int16_t x)} } |]
  toCxxParam :: Int16 -> IO (Ptr (CxxParam Int16))
  toCxxParam x = [CU.exp| Halide::Param<int16_t>* { new Halide::Param<int16_t>{$(int16_t x)} } |]

instance IsHalideType Int32 where
  type CxxType Int32 = Int32
  halideTypeFor :: proxy Int32 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 32 1
  toCxxExpr :: Int32 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int32_t x)} } |]
  toCxxParam :: Int32 -> IO (Ptr (CxxParam Int32))
  toCxxParam x = [CU.exp| Halide::Param<int32_t>* { new Halide::Param<int32_t>{$(int32_t x)} } |]

instance IsHalideType Int64 where
  type CxxType Int64 = Int64
  halideTypeFor :: proxy Int64 -> HalideType
  halideTypeFor _ = HalideType HalideTypeInt 64 1
  toCxxExpr :: Int64 -> IO (Ptr CxxExpr)
  toCxxExpr x = [CU.exp| Halide::Expr* { new Halide::Expr{$(int64_t x)} } |]
  toCxxParam :: Int64 -> IO (Ptr (CxxParam Int64))
  toCxxParam x = [CU.exp| Halide::Param<int64_t>* { new Halide::Param<int64_t>{$(int64_t x)} } |]

instance Castable Float Int32 where
  castImpl :: proxy Float -> proxy Int32 -> Ptr CxxExpr -> IO (Ptr CxxExpr)
  castImpl _ _ x = [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cast<int32_t>(*$(Halide::Expr* x))} } |]

instance Castable Double Int32 where
  castImpl :: proxy Double -> proxy Int32 -> Ptr CxxExpr -> IO (Ptr CxxExpr)
  castImpl _ _ x = [CU.exp| Halide::Expr* { new Halide::Expr{Halide::cast<int32_t>(*$(Halide::Expr* x))} } |]