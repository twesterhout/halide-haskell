module Language.Halide.Prelude
  ( (==)
  , (/=)
  , (+)
  , (-)
  )
where

import Data.Kind (Type)
import Language.Halide.Expr
import Language.Halide.Type
import Prelude (Bool, undefined)

type family Promoted a b :: Type

infix 4 ==, /=

(==) :: Expr a -> Expr b -> Expr Bool
(==) = undefined

(/=) :: Expr a -> Expr b -> Expr Bool
(/=) = undefined

infix 6 +, -

(+) :: Expr a -> Expr b -> Expr (Promoted a b)
(+) = undefined

(-) :: Expr a -> Expr b -> Expr (Promoted a b)
(-) = undefined

infix 7 *, /

(*) :: Expr a -> Expr b -> Expr (Promoted a b)
(*) = undefined

(/) :: Expr a -> Expr b -> Expr (Promoted a b)
(/) = undefined

mkExpr :: IsHalideType a => a -> Expr a
mkExpr = undefined
