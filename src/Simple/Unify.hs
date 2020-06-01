module Simple.Unify where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Simple.Ctx as Ctx
import Simple.TI (TI, fresh)
import qualified Simple.Type as Type

type Subs = Map.Map Type.Ext Type.Type

unify :: Ctx.Ctx -> Type.Type -> Type.Type -> TI Ctx.Ctx
unify ctx t1 t2 = case (t1, t2) of
  (Type.Unit, Type.Unit) -> return ctx
  (Type.Var x, Type.Var y) | x == y -> return ctx
  (Type.Unsolved x, Type.Unsolved y) | x == y -> return ctx
  (Type.Arrow t1a t1r, Type.Arrow t2a t2r) -> do
    ctx2 <- unify ctx t1a t2a
    unify ctx2 (Ctx.subs ctx2 t1r) (Ctx.subs ctx2 t2r)
  (Type.Forall x1 t1, Type.Forall x2 t2) -> do
    f <- fresh
    ctx3 <- unify (Ctx.Unsolved f : ctx) (Type.subsVar x1 (Type.Unsolved f) t1) t2
    case Ctx.splitMarker ctx3 f of
      Nothing -> error $ "Can't find marker: " ++ show f
      Just (_, ctx4) -> return ctx
  (Type.Unsolved x, t2) -> inst ctx x t2
  (t1, Type.Unsolved x) -> inst ctx x t1
  (t1, t2) -> error $ unwords ["Can't unify", show t1, show t2]

inst :: Ctx.Ctx -> Type.Ext -> Type.Type -> TI Ctx.Ctx
inst ctx x t = case Ctx.splitUnsolved ctx x of
  Nothing -> error $ "Can't find unsolved in ctx: " ++ show x
  Just (hs, ts) -> case t of
    Type.Forall _ _ -> error $ "Only monotype inst allowed!"
    Type.Arrow ta tr -> do
      fa <- fresh
      fr <- fresh
      let t2 = Type.Arrow (Type.Unsolved fa) (Type.Unsolved fr)
      return $ hs ++ Ctx.Solved x t2 : Ctx.Unsolved fa : Ctx.Unsolved fr : ts
    Type.Unsolved y -> case Ctx.splitUnsolved hs y of
      Just (hs, ms) ->
        return $
          hs
            ++ Ctx.Solved y (Type.Unsolved x) : ms
            ++ Ctx.Unsolved x : ts
      Nothing -> case Ctx.splitUnsolved ts y of
        Just (ms, ts) ->
          return $
            hs
              ++ Ctx.Solved x (Type.Unsolved y) : ms
              ++ Ctx.Unsolved y : ts
        Nothing -> error $ "Can't find unsolved in ctx: " ++ show y
    -- t must be a simple type like var or unit
    t -> return $ hs ++ (Ctx.Solved x t) : ts
