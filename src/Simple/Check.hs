module Simple.Check where

import qualified Data.Map as Map
import qualified Simple.Ctx as Ctx
import qualified Simple.Expr as Expr
import qualified Simple.Expr.Source as Source
import qualified Simple.Expr.Typed as Typed
import Simple.TI (TI, fresh)
import qualified Simple.Type as Type
import qualified Simple.Unify as Unify

check :: Ctx.Ctx -> Source.Expr -> Type.Type -> TI (Ctx.Ctx, Typed.Expr)
check ctx e t = case (e, t) of
  (e, Type.Forall x t) -> do
    (ctx2, ce) <- check ctx e t
    return (ctx2, Typed.TAbs x ce)
  (Source.Abs x e, Type.Arrow ta tr) -> do
    let binding = Ctx.Binding x ta
    (ctx2, ce) <- check (binding : ctx) e tr
    case Ctx.splitPart ctx2 binding of
      Just (_, ts) -> return (ts, Typed.Abs x ta ce)
      Nothing -> error $ "Expected binding in ctx: " ++ show binding
  (e, t) -> do
    (ctx2, t2, ce2) <- infer ctx e
    (ctx3, t3, ce3) <- applyUnsolved ctx2 t2 ce2
    ctx4 <- Unify.unify ctx3 (Ctx.subs ctx3 t) (Ctx.subs ctx3 t3)
    -- do we need to apply ctx4 to ce3?
    return (ctx4, ce3)

infer :: Ctx.Ctx -> Source.Expr -> TI (Ctx.Ctx, Type.Type, Typed.Expr)
infer ctx e = case e of
  Source.Unit -> return (ctx, Type.Unit, Typed.Unit)
  Source.Var x -> case Ctx.lookupBinding ctx x of
    Nothing -> error $ "Unknown variable: " ++ x
    Just t -> return (ctx, t, Typed.Var x)
  Source.Ann e t -> do
    -- check t, subs t?
    (ctx2, ce) <- check ctx e t
    return (ctx2, t, ce)
  Source.Abs x e -> do
    y <- fresh
    let binding = Ctx.Binding x (Type.Unsolved y)
    (ctx2, tr, ce) <- infer (binding : ctx) e
    case Ctx.splitPart ctx2 binding of
      Just (_, ts) -> return (ts, Type.Arrow (Type.Unsolved y) tr, ce)
      Nothing -> error $ "Expected binding in ctx: " ++ show binding
  Source.App e1 e2 -> do
    (ctx2, t2, ce2) <- infer ctx e1
    (ctx3, t3, ce3) <- applyUnsolved ctx2 t2 ce2
    case Ctx.subs ctx3 t3 of
      Type.Arrow ta tr -> do
        (ctx4, ce4) <- check ctx3 e2 ta
        return (ctx4, tr, Typed.App ce3 ce4)
      t2 -> error $ "Tried to apply to non function: " ++ show t2

applyUnsolved :: Ctx.Ctx -> Type.Type -> Typed.Expr -> TI (Ctx.Ctx, Type.Type, Typed.Expr)
applyUnsolved ctx t ce = case t of
  Type.Forall x t -> do
    y <- fresh
    let ctx2 = Ctx.Unsolved y : ctx
    (ctx3, t3, ce3) <- applyUnsolved ctx2 (Type.subsVar x (Type.Unsolved y) t) ce
    return (ctx3, t3, Typed.TApp ce3 (Type.Unsolved y))
  t -> return (ctx, t, ce)
