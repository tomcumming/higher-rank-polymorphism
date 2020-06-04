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
    ctx3 <- unify (Ctx.UnsolvedExt f : ctx) (Type.subsVar x1 (Type.Unsolved f) t1) t2
    case Ctx.splitMarker ctx3 f of
      Nothing -> error $ "Can't find marker: " ++ show f
      Just (_, ctx4) -> return ctx
  (Type.Unsolved x, t2) -> inst ctx x t2
  (t1, Type.Unsolved x) -> inst ctx x t1
  (t1, t2) -> error $ unwords ["Can't unify", show t1, show t2]

inst :: Ctx.Ctx -> Type.Ext -> Type.Type -> TI Ctx.Ctx
inst ctx x t = case Ctx.splitUnsolved ctx x of
  Nothing -> error $ show (ctx, x, t) -- "Can't find unsolved in ctx: " ++ show x
  Just (hs, ts) -> case t of
    Type.Forall _ _ -> error $ "Only monotype inst allowed!"
    Type.Arrow ta tr -> do
      fa <- fresh
      fr <- fresh
      let t2 = Type.Arrow (Type.Unsolved fa) (Type.Unsolved fr)
      return $ hs ++ Ctx.SolvedExt x t2 : Ctx.UnsolvedExt fa : Ctx.UnsolvedExt fr : ts
    Type.Unsolved y -> case Ctx.splitUnsolved hs y of
      Just (hs, ms) ->
        return $
          hs
            ++ Ctx.SolvedExt y (Type.Unsolved x) : ms
            ++ Ctx.UnsolvedExt x : ts
      Nothing -> case Ctx.splitUnsolved ts y of
        Just (ms, ts) ->
          return $
            hs
              ++ Ctx.SolvedExt x (Type.Unsolved y) : ms
              ++ Ctx.UnsolvedExt y : ts
        Nothing -> error $ "Can't find unsolved in ctx: " ++ show y
    t -> case monoValid ts t of
      True -> return $ hs ++ (Ctx.SolvedExt x t) : ts
      False -> error $ show (t, ts)

monoValid :: Ctx.Ctx -> Type.Type -> Bool
monoValid ctx t = case t of
  Type.Var x -> varInCtx ctx x
  Type.Unsolved x -> unsolvedInCtx ctx x
  Type.Forall x t -> False
  Type.Arrow t1 t2 -> monoValid ctx t1 && monoValid ctx t2
  Type.Unit -> True
  where
    varInCtx :: Ctx.Ctx -> Type.Id -> Bool
    varInCtx ctx x = case ctx of
      [] -> False
      (Ctx.Var y : _) | x == y -> True
      (_ : ctx) -> varInCtx ctx x
    unsolvedInCtx :: Ctx.Ctx -> Type.Ext -> Bool
    unsolvedInCtx ctx x = case ctx of
      [] -> False
      (Ctx.UnsolvedExt y : _) | x == y -> True
      (Ctx.SolvedExt y _ : _) | x == y -> True
      (_ : ctx) -> unsolvedInCtx ctx x
