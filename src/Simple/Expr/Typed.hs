module Simple.Expr.Typed where

import qualified Simple.Ctx as Ctx
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

subs :: Ctx.Ctx -> Expr -> Expr
subs ctx e = case e of
  Abs x t e -> Abs x (Ctx.subs ctx t) (subs ctx e)
  App e1 e2 -> App (subs ctx e1) (subs ctx e2)
  TAbs x e -> TAbs x (subs ctx e)
  TApp e t -> TApp (subs ctx e) (Ctx.subs ctx t)
  e -> e

-- deep eta reduction
simplify :: Expr -> Expr
simplify e = case e of
  TAbs x (TApp e (Type.Var y)) | x == y -> simplify e
  Abs x t e -> Abs x t (simplify e)
  App e1 e2 -> App (simplify e1) (simplify e2)
  TAbs x e -> TAbs x (simplify e)
  TApp e t -> TApp (simplify e) t
  e -> e
