module Simple.Expr.Typed where

import Simple.Expr (Id)
import qualified Simple.Type as Type

data Expr
  = Unit
  | Var Id
  | Abs Id Type.Type Expr
  | App Expr Expr
  | TAbs Type.Id Expr
  | TApp Expr Type.Type
  deriving (Show, Eq)
