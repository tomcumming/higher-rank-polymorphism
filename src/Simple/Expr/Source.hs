module Simple.Expr.Source where

import Simple.Expr (Id)
import Simple.Type (Type)

data Expr
  = Unit
  | Var Id
  | Ann Expr Type
  | Abs Id Expr
  | App Expr Expr
  deriving (Show, Eq)
