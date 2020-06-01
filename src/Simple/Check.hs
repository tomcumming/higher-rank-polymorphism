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
      Just (_, ts) -> return (ts, ce)
      Nothing -> error $ "Expected binding in ctx: " ++ show binding
  (e, t) -> do
    (ctx2, t2, ce2) <- infer ctx e
    (t3, ce3) <- applyUnsolved t2 ce2
    ctx4 <- Unify.unify ctx2 (Ctx.subs ctx2 t) (Ctx.subs ctx2 t3)
    -- do we need to apply ctx4 to ce3?
    return (ctx4, ce3)

infer :: Ctx.Ctx -> Source.Expr -> TI (Ctx.Ctx, Type.Type, Typed.Expr)
infer ctx e = undefined

applyUnsolved :: Type.Type -> Typed.Expr -> TI (Type.Type, Typed.Expr)
applyUnsolved t ce = undefined
