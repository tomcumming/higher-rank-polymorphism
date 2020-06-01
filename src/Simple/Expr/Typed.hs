module Simple.Expr.Typed where

import Simple.Expr (Id)
import qualified Simple.Type as Type
import qualified Simple.Unify as Unify

data Expr
  = Unit
  | Var Id
  | Abs Id Type.Type Expr
  | App Expr Expr
  | TAbs Type.Id Expr
  | TApp Expr Type.Type
  deriving (Show, Eq)

{-
subs :: Unify.Subs -> Expr -> Expr
subs s e = case e of
  Abs x t e -> Abs x (Unify.subs s t) (subs s e)
  App e1 e2 -> App (subs s e1) (subs s e2)
  TAbs x e -> TAbs x (subs s e)
  TApp e t -> TApp (subs s e) (Unify.subs s t)
  e -> e
-}

-- deep eta reduction
simplify :: Expr -> Expr
simplify e = case e of
  TAbs x (TApp e (Type.Var y)) | x == y -> simplify e
  Abs x t e -> Abs x t (simplify e)
  App e1 e2 -> App (simplify e1) (simplify e2)
  TAbs x e -> TAbs x (simplify e)
  TApp e t -> TApp (simplify e) t
  e -> e
