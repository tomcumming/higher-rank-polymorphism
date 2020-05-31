module Simple.Check where

import qualified Data.Map as Map
import qualified Simple.Expr as Expr
import qualified Simple.Expr.Source as Source
import qualified Simple.Expr.Typed as Typed
import Simple.TI (TI, fresh)
import qualified Simple.Type as Type
import qualified Simple.Unify as Unify

type Ctx = Map.Map (Expr.Id) Type.Type

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

infer :: Ctx -> Source.Expr -> TI (Unify.Subs, Type.Type, Typed.Expr)
infer ctx e = case e of
  Source.Unit -> return (Map.empty, Type.Unit, Typed.Unit)
  Source.Var x -> case Map.lookup x ctx of
    Nothing -> error $ "Unknown variable: " ++ x
    Just t -> return (Map.empty, t, Typed.Var x)
  Source.Ann e t -> do
    -- do we need to check t ?
    (s, ce) <- check ctx e t
    return (s, t, ce)
  Source.Abs x e -> do
    y <- fresh
    (s, t, ce) <- infer (Map.insert x (Type.Unsolved y) ctx) e
    return (s, Type.Arrow (Type.Unsolved y) t, Typed.Abs x (Type.Unsolved y) ce)
  Source.App e1 e2 -> do
    (s1, t, ce) <- infer ctx e1
    (t2, ce2) <- instantiate t ce
    -- this is the only place we need to subs? (outside of combineSubs)
    case Unify.subs s1 t2 of
      Type.Arrow ta tr -> do
        (s2, ce3) <- check ctx e2 ta
        return (Unify.combineSubs s1 s2, tr, Typed.App ce2 ce3)
      t2 -> error $ "Tried to apply to non function: " ++ show t2

instantiate :: Type.Type -> Typed.Expr -> TI (Type.Type, Typed.Expr)
instantiate t ce = case t of
  Type.Forall x t -> do
    y <- fresh
    (t2, ce2) <- instantiate (Unify.subsVar x (Type.Unsolved y) t) ce
    return (t2, Typed.TApp ce2 (Type.Unsolved y))
  t -> return (t, ce)
