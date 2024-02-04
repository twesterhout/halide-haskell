{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}

-- |
-- Module      : Language.Halide.Type
-- Description : Low-level types
-- Copyright   : (c) Tom Westerhout, 2023
module Language.Halide.Type
  ( HalideTypeCode (..)
  , HalideType (..)
  , IsHalideType (..)
  , Callable (..)
  , CxxExpr
  , CxxVar
  , CxxRVar
  , CxxVarOrRVar
  , CxxFunc
  , CxxParameter
  , CxxImageParam
  , CxxVector
  , CxxUserContext
  , CxxCallable
  , CxxTarget
  , CxxStageSchedule
  , CxxString
  , Arguments (..)
  , Length
  , Append
  , Concat
  , argumentsAppend
  , FunctionArguments
  , FunctionReturn
  , All
  , UnCurry (..)
  , Curry (..)
  , defineIsHalideTypeInstances
  , instanceHasCxxVector
  , HasCxxVector (..)
  , instanceCxxConstructible
  , CxxConstructible (..)
  , halideTypes
  -- defineCastableInstances,
  -- defineCurriedTypeFamily,
  -- defineUnCurriedTypeFamily,
  -- defineCurryInstances,
  -- defineUnCurryInstances,
  )
where

import Data.Coerce
import Data.Constraint
import Data.Int
import Data.Kind (Type)
import Data.Text qualified as T
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import GHC.ForeignPtr (mallocForeignPtrAlignedBytes)
import GHC.TypeLits
import Language.C.Inline qualified as C
import Language.C.Inline.Unsafe qualified as CU
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax (Lift)

-- | Haskell counterpart of @Halide::Expr@.
data CxxExpr

-- | Haskell counterpart of @Halide::Var@.
data CxxVar

-- | Haskell counterpart of @Halide::RVar@.
data CxxRVar

-- | Haskell counterpart of @Halide::VarOrRVar@.
data CxxVarOrRVar

-- | Haskell counterpart of @Halide::Internal::Parameter@.
data CxxParameter

-- | Haskell counterpart of @Halide::ImageParam@.
data CxxImageParam

-- | Haskell counterpart of @Halide::Func@.
data CxxFunc

-- | Haskell counterpart of @Halide::JITUserContext@.
data CxxUserContext

-- | Haskell counterpart of @Halide::Callable@.
data CxxCallable

-- | Haskell counterpart of @Halide::Target@.
data CxxTarget

-- | Haskell counterpart of @std::vector@.
data CxxVector a

-- | Haskell counterpart of @Halide::Internal::StageSchedule@.
data CxxStageSchedule

-- | Haskell counterpart of @std::string@
data CxxString

class CxxConstructible a where
  cxxSizeOf :: Int
  cxxConstruct :: (Ptr a -> IO ()) -> IO (ForeignPtr a)

cxxConstructWithDeleter :: Int -> FinalizerPtr a -> (Ptr a -> IO ()) -> IO (ForeignPtr a)
cxxConstructWithDeleter size deleter constructor = do
  fp <- mallocForeignPtrAlignedBytes size align
  withForeignPtr fp constructor
  addForeignPtrFinalizer deleter fp
  pure fp
  where
    align = 64

-- data Split =
--   SplitVar !Text !Text !Text !(Expr Int32) !

newtype Callable (signature :: Type) = Callable (ForeignPtr CxxCallable)

-- | Haskell counterpart of @halide_type_code_t@.
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

-- | Haskell counterpart of @halide_type_t@.
data HalideType = HalideType
  { halideTypeCode :: !HalideTypeCode
  , halideTypeBits :: {-# UNPACK #-} !Word8
  , halideTypeLanes :: {-# UNPACK #-} !Word16
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

-- | Specifies that a type is supported by Halide.
class (Storable a) => IsHalideType a where
  halideTypeFor :: proxy a -> HalideType
  toCxxExpr :: a -> IO (ForeignPtr CxxExpr)

-- | Helper function to coerce 'Float' to 'CFloat' and 'Double' to 'CDouble'
-- before passing them to inline-c quasiquotes. This is needed because inline-c
-- assumes that @float@ in C corresponds to 'CFloat' in Haskell.
optionallyCast :: String -> TH.TypeQ -> TH.ExpQ
optionallyCast cType hsType' = do
  hsType <- hsType'
  hsTargetType <- C.getHaskellType False cType
  if hsType == hsTargetType then [e|id|] else [e|coerce|]

-- | Template Haskell splice that defines instances of 'IsHalideType' for a
-- given Haskell type.
instanceIsHalideType :: (String, TH.TypeQ, HalideTypeCode) -> TH.DecsQ
instanceIsHalideType (cType, hsType, typeCode) =
  C.substitute
    [("T", \x -> "$(" <> cType <> " " <> x <> ")")]
    [d|
      instance IsHalideType $hsType where
        halideTypeFor _ = HalideType typeCode bits 1
          where
            bits = fromIntegral $ 8 * sizeOf (undefined :: $hsType)
        toCxxExpr y =
          cxxConstruct $ \ptr ->
            [CU.exp| void { new ($(Halide::Expr* ptr)) Halide::Expr{@T(x)} } |]
          where
            x = $(optionallyCast cType hsType) y
      |]

-- | Derive 'IsHalideType' instances for all supported types.
defineIsHalideTypeInstances :: TH.DecsQ
defineIsHalideTypeInstances = concat <$> mapM instanceIsHalideType halideTypes

instanceCxxConstructible :: String -> TH.DecsQ
instanceCxxConstructible cType =
  C.substitute
    [ ("T", const cType)
    , ("Deleter", const $ "deleter(" <> cType <> "* p)")
    , ("Class", const . T.unpack . snd $ T.breakOnEnd "::" (T.pack cType))
    ]
    [d|
      instance CxxConstructible $(C.getHaskellType False cType) where
        cxxSizeOf = fromIntegral [CU.pure| size_t { sizeof(@T()) } |]
        cxxConstruct = cxxConstructWithDeleter size deleter
          where
            size = fromIntegral [CU.pure| size_t { sizeof(@T()) } |]
            deleter = [C.funPtr| void @Deleter() { p->~@Class()(); } |]
      |]

-- | Specifies that a given Haskell type can be used with @std::vector@.
--
-- E.g. if we have @HasCxxVector Int16@, then using @std::vector<int16_t>*@
-- in inline-c quotes will work.
class HasCxxVector a where
  newCxxVector :: Maybe Int -> IO (Ptr (CxxVector a))
  deleteCxxVector :: Ptr (CxxVector a) -> IO ()
  cxxVectorSize :: Ptr (CxxVector a) -> IO Int
  cxxVectorPushBack :: Ptr (CxxVector a) -> Ptr a -> IO ()
  cxxVectorData :: Ptr (CxxVector a) -> IO (Ptr a)
  peekCxxVector :: (Storable a) => Ptr (CxxVector a) -> IO [a]

-- | Template Haskell splice that defines an instance of 'HasCxxVector' for a given C type name.
instanceHasCxxVector :: String -> TH.DecsQ
instanceHasCxxVector cType =
  C.substitute
    [ ("T", const cType)
    , ("VEC", \var -> "$(std::vector<" ++ cType ++ ">* " ++ var ++ ")")
    ]
    [d|
      instance HasCxxVector $(C.getHaskellType False cType) where
        newCxxVector maybeSize = do
          v <- [CU.exp| std::vector<@T()>* { new std::vector<@T()>() } |]
          case maybeSize of
            Just size ->
              let n = fromIntegral size
               in [CU.exp| void { @VEC(v)->reserve($(size_t n)) } |]
            Nothing -> pure ()
          pure v
        deleteCxxVector vec = [CU.exp| void { delete @VEC(vec) } |]
        cxxVectorSize vec = fromIntegral <$> [CU.exp| size_t { @VEC(vec)->size() } |]
        cxxVectorPushBack vec x = [CU.exp| void { @VEC(vec)->push_back(*$(@T()* x)) } |]
        cxxVectorData vec = [CU.exp| @T()* { @VEC(vec)->data() } |]
        peekCxxVector vec = do
          n <- cxxVectorSize vec
          allocaArray n $ \out -> do
            [CU.block| void {
              auto const& vec = *@VEC(vec);
              auto* out = $(@T()* out);
              std::uninitialized_copy(std::begin(vec), std::end(vec), out);
            } |]
            peekArray n out
      |]

-- | List of all supported types.
halideTypes :: [(String, TH.TypeQ, HalideTypeCode)]
halideTypes =
  [ ("float", [t|Float|], HalideTypeFloat)
  , ("float", [t|CFloat|], HalideTypeFloat)
  , ("double", [t|Double|], HalideTypeFloat)
  , ("double", [t|CDouble|], HalideTypeFloat)
  , ("int8_t", [t|Int8|], HalideTypeInt)
  , ("int16_t", [t|Int16|], HalideTypeInt)
  , ("int32_t", [t|Int32|], HalideTypeInt)
  , ("int64_t", [t|Int64|], HalideTypeInt)
  , ("uint8_t", [t|Word8|], HalideTypeUInt)
  , ("uint16_t", [t|Word16|], HalideTypeUInt)
  , ("uint32_t", [t|Word32|], HalideTypeUInt)
  , ("uint64_t", [t|Word64|], HalideTypeUInt)
  ]

infixr 5 :::

-- | A heterogeneous list.
data Arguments (k :: [Type]) where
  Nil :: Arguments '[]
  (:::) :: !t -> !(Arguments ts) -> Arguments (t ': ts)

-- | A type family that returns the length of a type-level list.
type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

-- | Append to a type-level list.
type family Append (xs :: [k]) (y :: k) :: [k] where
  Append '[] y = '[y]
  Append (x ': xs) y = x ': Append xs y

type family Concat (xs :: [k]) (ys :: [k]) :: [k] where
  Concat '[] ys = ys
  Concat (x ': xs) ys = x ': Concat xs ys

-- | Append a value to 'Arguments'
argumentsAppend :: Arguments xs -> t -> Arguments (Append xs t)
argumentsAppend = go
  where
    go :: forall xs t. Arguments xs -> t -> Arguments (Append xs t)
    go Nil y = y ::: Nil
    go (x ::: xs) y = x ::: go xs y

-- | Return the list of arguments to of a function type.
type family FunctionArguments (f :: Type) :: [Type] where
  FunctionArguments (a -> b) = a ': FunctionArguments b
  FunctionArguments a = '[]

-- | Get the return type of a function.
type family FunctionReturn (f :: Type) :: Type where
  FunctionReturn (a -> b) = FunctionReturn b
  FunctionReturn a = a

-- | Apply constraint to all types in a list.
type family All (c :: Type -> Constraint) (ts :: [Type]) = (p :: Constraint) | p -> ts where
  All c '[] = ()
  All c (t ': ts) = (c t, All c ts)

-- | A helper typeclass to convert a normal curried function to a function that
-- takes 'Arguments' as input.
--
-- For instance, if we have a function @f :: Int -> Float -> Double@, then it
-- will be converted to @f' :: Arguments '[Int, Float] -> Double@.
class UnCurry (f :: Type) (args :: [Type]) (r :: Type) | args r -> f, args f -> r where
  uncurryG :: f -> Arguments args -> r

instance (FunctionArguments f ~ '[], FunctionReturn f ~ r, f ~ r) => UnCurry f '[] r where
  uncurryG f Nil = f
  {-# INLINE uncurryG #-}

instance (UnCurry f args r) => UnCurry (a -> f) (a ': args) r where
  uncurryG f (a ::: args) = uncurryG (f a) args
  {-# INLINE uncurryG #-}

-- | A helper typeclass to convert a function that takes 'Arguments' as input
-- into a normal curried function. This is the inverse of 'UnCurry'.
--
-- For instance, if we have a function @f :: Arguments '[Int, Float] -> Double@, then
-- it will be converted to @f' :: Int -> Float -> Double@.
class Curry (args :: [Type]) (r :: Type) (f :: Type) | args r -> f where
  curryG :: (Arguments args -> r) -> f

instance Curry '[] r r where
  curryG f = f Nil
  {-# INLINE curryG #-}

instance (Curry args r f) => Curry (a ': args) r (a -> f) where
  curryG f a = curryG (\args -> f (a ::: args))
  {-# INLINE curryG #-}
