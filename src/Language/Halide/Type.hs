{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

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
    defineIsHalideTypeInstances,
    defineCastableInstances,
  )
where

import Data.Coerce
import Data.Int
import Data.Primitive.Types (Prim)
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Syntax (Lift)

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
  deriving stock (Read, Show, Eq, Lift)

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

class (Prim a, Storable a) => IsHalideType a where
  halideTypeFor :: proxy a -> HalideType
  toCxxExpr :: a -> IO (Ptr CxxExpr)

optionallyCast :: String -> TH.TypeQ -> TH.ExpQ
optionallyCast cType hsType' = do
  hsType <- hsType'
  hsTargetType <- C.getHaskellType False cType
  if hsType == hsTargetType then [e|id|] else [e|coerce|]

instanceIsHalideType :: (String, TH.TypeQ, HalideTypeCode) -> TH.DecsQ
instanceIsHalideType (cType, hsType, typeCode) =
  C.substitute
    [("T", \x -> "$(" <> cType <> " " <> x <> ")")]
    [d|
      instance IsHalideType $hsType where
        halideTypeFor _ = HalideType typeCode bits 1
          where
            bits = fromIntegral $ 8 * sizeOf (undefined :: $hsType)
        toCxxExpr y = [CU.exp| Halide::Expr* { new Halide::Expr{@T(x)} } |]
          where
            x = $(optionallyCast cType hsType) y
      |]

defineIsHalideTypeInstances :: TH.DecsQ
defineIsHalideTypeInstances = concat <$> mapM instanceIsHalideType halideTypes

class (IsHalideType to, IsHalideType from) => Castable to from where
  castImpl :: proxy to -> proxy from -> Ptr CxxExpr -> IO (Ptr CxxExpr)

instanceCastable :: (String, TH.TypeQ, TH.TypeQ) -> TH.DecsQ
instanceCastable (toType, toHsType, fromHsType) =
  C.substitute
    [("To", const toType)]
    [d|
      instance Castable $toHsType $fromHsType where
        castImpl _ _ x =
          [CU.exp| Halide::Expr* {
            new Halide::Expr{Halide::cast<@To()>(*$(Halide::Expr* x))} } |]
      |]

halideTypes :: [(String, TH.TypeQ, HalideTypeCode)]
halideTypes =
  [ ("float", [t|Float|], HalideTypeFloat),
    ("float", [t|CFloat|], HalideTypeFloat),
    ("double", [t|Double|], HalideTypeFloat),
    ("double", [t|CDouble|], HalideTypeFloat),
    ("int8_t", [t|Int8|], HalideTypeInt),
    ("int16_t", [t|Int16|], HalideTypeInt),
    ("int32_t", [t|Int32|], HalideTypeInt),
    ("int64_t", [t|Int64|], HalideTypeInt),
    ("uint8_t", [t|Word8|], HalideTypeUInt),
    ("uint16_t", [t|Word16|], HalideTypeUInt),
    ("uint32_t", [t|Word32|], HalideTypeUInt),
    ("uint64_t", [t|Word64|], HalideTypeUInt)
  ]

defineCastableInstances :: TH.DecsQ
defineCastableInstances =
  fmap concat
    <$> sequence
    $ [ instanceCastable (toType, toHsType, fromHsType)
        | (toType, toHsType, _) <- halideTypes,
          (fromType, fromHsType, _) <- halideTypes,
          toType /= fromType
      ]
      <> [instanceCastable (toType, toHsType, toHsType) | (toType, toHsType, _) <- halideTypes]
