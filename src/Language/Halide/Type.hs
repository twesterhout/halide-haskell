{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Language.Halide.Type
  ( HalideTypeCode (..)
  , HalideType (..)
  , IsHalideType (..)
  , CxxExpr
  , CxxFunc
  , CxxParam
  , CxxParameter
  , CxxArgument
  , CxxImageParam
  , CxxVector
  , CxxUserContext
  , CxxCallable
  , Arguments (..)
  , Length
  , Append
  , argumentsAppend
  , FunctionArguments
  , FunctionReturn
  , UnCurry (..)
  , Curry (..)
  , defineIsHalideTypeInstances
  , Named (..)
  -- defineCastableInstances,
  -- defineCurriedTypeFamily,
  -- defineUnCurriedTypeFamily,
  -- defineCurryInstances,
  -- defineUnCurryInstances,
  )
where

import Data.Coerce
import Data.Int
import Data.Kind (Type)
import Data.Text (Text)
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import GHC.Stack (HasCallStack)
import GHC.TypeLits
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

class Storable a => IsHalideType a where
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

-- class Castable to from where
--   castImpl :: proxy to -> proxy from -> Ptr CxxExpr -> IO (Ptr CxxExpr)
--
-- instanceCastable :: (String, TH.TypeQ, TH.TypeQ) -> TH.DecsQ
-- instanceCastable (toType, toHsType, fromHsType) =
--   C.substitute
--     [("To", const toType)]
--     [d|
--       instance Castable $toHsType $fromHsType where
--         castImpl _ _ x =
--           [CU.exp| Halide::Expr* {
--             new Halide::Expr{Halide::cast<@To()>(*$(Halide::Expr* x))} } |]
--       |]

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

-- defineCastableInstances :: TH.DecsQ
-- defineCastableInstances =
--   fmap concat
--     <$> sequence
--     $ [ instanceCastable (toType, toHsType, fromHsType)
--         | (toType, toHsType, _) <- halideTypes,
--           (fromType, fromHsType, _) <- halideTypes,
--           toType /= fromType
--       ]
--       <> [instanceCastable (toType, toHsType, toHsType) | (toType, toHsType, _) <- halideTypes]

infixr 5 :::

data Arguments (k :: [Type]) where
  Nil :: Arguments '[]
  (:::) :: !t -> !(Arguments ts) -> Arguments (t ': ts)

type family Length (xs :: [k]) :: Nat where
  Length '[] = 0
  Length (x ': xs) = 1 + Length xs

type family Append (xs :: [k]) (y :: k) :: [k] where
  Append '[] y = '[y]
  Append (x ': xs) y = x ': Append xs y

argumentsAppend :: Arguments xs -> t -> Arguments (Append xs t)
argumentsAppend = go
  where
    go :: forall xs t. Arguments xs -> t -> Arguments (Append xs t)
    go Nil y = y ::: Nil
    go (x ::: xs) y = x ::: go xs y

type family FunctionArguments (f :: Type) :: [Type] where
  FunctionArguments (a -> b) = a ': FunctionArguments b
  FunctionArguments a = '[]

type family FunctionReturn (f :: Type) :: Type where
  FunctionReturn (a -> b) = FunctionReturn b
  FunctionReturn a = a

class UnCurry (f :: Type) (args :: [Type]) (r :: Type) | args r -> f where
  uncurryG :: f -> Arguments args -> r

instance (FunctionArguments f ~ '[], FunctionReturn f ~ r, f ~ r) => UnCurry f '[] r where
  uncurryG f Nil = f
  {-# INLINE uncurryG #-}

instance (UnCurry f args r) => UnCurry (a -> f) (a ': args) r where
  uncurryG f (a ::: args) = uncurryG (f a) args
  {-# INLINE uncurryG #-}

class Curry (args :: [Type]) (r :: Type) (f :: Type) | args r -> f where
  curryG :: (Arguments args -> r) -> f

instance Curry '[] r r where
  curryG f = f Nil
  {-# INLINE curryG #-}

instance Curry args r f => Curry (a ': args) r (a -> f) where
  curryG f a = curryG (\args -> f (a ::: args))

class Named a where
  setName :: HasCallStack => a -> Text -> IO ()

{-
defineCurriedTypeFamily :: TH.DecsQ
defineCurriedTypeFamily = do
  familyEqns <- mapM defineEqn [0 .. 20]
  pure [TH.ClosedTypeFamilyD familyHead familyEqns]
  where
    name = TH.mkName "Curried"
    f = TH.mkName "f"
    familyHead = TH.TypeFamilyHead name [TH.PlainTV f ()] (TH.KindSig TH.StarT) Nothing
    defineEqn :: Int -> TH.TySynEqnQ
    defineEqn n = do
      let xs = TH.VarT <$> [TH.mkName ("x" <> show i) | i <- [1 .. n]]
          r = TH.VarT . TH.mkName $ "r"
          -- argc = pure . TH.LitT . TH.NumTyLit . fromIntegral $ n
          types = foldr (\x g -> [t|$x ': $g|]) [t|'[]|] $ fmap pure xs
          func = foldr (TH.AppT . TH.AppT TH.ArrowT) r xs
      args <- [t|Arguments $types -> $(pure r)|]
      pure $ TH.TySynEqn Nothing (TH.AppT (TH.ConT name) args) func
-}

{-
defineUnCurriedTypeFamily :: TH.DecsQ
defineUnCurriedTypeFamily = do
  familyEqns <- mapM defineEqn (enumFromThenTo 20 19 0)
  pure [TH.ClosedTypeFamilyD familyHead familyEqns]
  where
    name = TH.mkName "UnCurried"
    f = TH.mkName "f"
    familyHead = TH.TypeFamilyHead name [TH.PlainTV f ()] (TH.KindSig TH.StarT) Nothing
    defineEqn :: Int -> TH.TySynEqnQ
    defineEqn n = do
      let xs = TH.VarT <$> [TH.mkName ("x" <> show i) | i <- [1 .. n]]
          r = TH.VarT . TH.mkName $ "r"
          -- argc = pure . TH.LitT . TH.NumTyLit . fromIntegral $ n
          types = foldr (\x g -> [t|$x ': $g|]) [t|'[]|] $ fmap pure xs
          func = foldr (TH.AppT . TH.AppT TH.ArrowT) r xs
      TH.TySynEqn Nothing (TH.AppT (TH.ConT name) func)
        <$> [t|Arguments $types -> $(pure r)|]
-}

{-
defineCurryInstance :: Int -> TH.DecsQ
defineCurryInstance n = do
  let xs = [TH.mkName ("x" <> show i) | i <- [1 .. n]]
      f = TH.mkName "f"
      r = pure . TH.VarT . TH.mkName $ "r"
      args = foldr (\x g -> [e|$x ::: $g|]) [e|Nil|] $ fmap (pure . TH.VarE) xs
      patterns = fmap TH.VarP $ f : xs
      types :: TH.TypeQ
      types = foldr (\x g -> [t|$x ': $g|]) [t|'[]|] $ fmap (pure . TH.VarT) xs
  -- argc = pure . TH.LitT . TH.NumTyLit . fromIntegral $ n
  body <- TH.NormalB <$> [e|$(pure (TH.VarE f)) $args|]
  funcType <- [t|Arguments $types -> $r|]
  let func = TH.FunD (TH.mkName "curry'") [TH.Clause patterns body []]
  pure $ [TH.InstanceD Nothing [] (TH.AppT (TH.ConT (TH.mkName "Curry")) funcType) [func]]

defineCurryInstances :: TH.DecsQ
defineCurryInstances = concat <$> mapM defineCurryInstance [0 .. 20]
-}

{-
defineUnCurryInstance :: Int -> TH.DecsQ
defineUnCurryInstance n = do
  let xs = [TH.mkName ("x" <> show i) | i <- [1 .. n]]
      f = TH.mkName "f"
      r = TH.AppT (TH.ConT ''IO) . TH.VarT . TH.mkName $ "r"
      args = foldl TH.AppE (TH.VarE f) $ fmap TH.VarE xs
      funcType = foldr (TH.AppT . TH.AppT TH.ArrowT) r $ fmap TH.VarT xs
      body = TH.NormalB args
  patterns <- foldr (\x g -> [p|$x ::: $g|]) [p|Nil|] $ fmap (pure . TH.VarP) xs
  let func = TH.FunD (TH.mkName "uncurry'") [TH.Clause [TH.VarP f, patterns] body []]
  pure $ [TH.InstanceD Nothing [] (TH.AppT (TH.ConT (TH.mkName "UnCurry")) funcType) [func]]

defineUnCurryInstances :: TH.DecsQ
defineUnCurryInstances = concat <$> mapM defineUnCurryInstance [0 .. 20]
-}
