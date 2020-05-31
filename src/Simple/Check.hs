module Simple.Check where

import qualified Data.Map as Map
import qualified Simple.Expr as Expr
import qualified Simple.Expr.Source as Source
import qualified Simple.Expr.Typed as Typed
import Simple.TI (TI, fresh)
import qualified Simple.Type as Type
import qualified Simple.Unify as Unify

type Ctx = Map.Map (Expr.Id) Type.Type

infer :: Ctx -> Source.Expr -> TI (Unify.Subs, Type.Type, Typed.Expr)
infer = error "todo infer"

check :: Ctx -> Source.Expr -> Type.Type -> TI (Unify.Subs, Typed.Expr)
check ctx e t = case (e, t) of
  (e, Type.Forall x t) -> do
    (s, ce) <- check ctx e t
    return (s, Typed.TAbs x ce) -- todo Eta reduction
  (Source.Abs x e, Type.Arrow ta tr) -> check (Map.insert x ta ctx) e ta
  (e, t) -> do
    (s, t2, ce2) <- infer ctx e
    (t3, ce3) <- instantiate t2 ce2
    let s2 = Unify.unify t t3
    return (Unify.combineSubs s s2, ce3)

instantiate :: Type.Type -> Typed.Expr -> TI (Type.Type, Typed.Expr)
instantiate t ce = case t of
  Type.Forall x t -> do
    y <- fresh
    (t2, ce2) <- instantiate (Unify.subsVar x (Type.Unsolved y) t) ce
    return (t2, Typed.TApp ce2 (Type.Unsolved y))
  t -> return (t, ce)
