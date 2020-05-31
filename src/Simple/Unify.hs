module Simple.Unify where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Simple.Type as Type

type Subs = Map.Map Type.Ext Type.Type

unify :: Type.Type -> Type.Type -> Subs
unify t1 t2 = case (t1, t2) of
  (Type.Unit, Type.Unit) -> Map.empty
  (Type.Var x, Type.Var y) | x == y -> Map.empty
  (Type.Unsolved x, Type.Unsolved y) | x == y -> Map.empty
  (Type.Arrow t1a t1r, Type.Arrow t2a t2r) ->
    combineSubs (unify t1a t2a) (unify t1r t2r)
  (Type.Forall x1 t1, Type.Forall x2 t2) -> unifyForall x1 t1 x2 t2
  (Type.Unsolved x, t) -> unifyUnsolved x t
  (t, Type.Unsolved x) -> unifyUnsolved x t
  _ -> error $ unwords $ ["Can't unify", show t1, show t2]

unifyUnsolved :: Type.Ext -> Type.Type -> Subs
unifyUnsolved x t = case Type.mono t of
  False -> error $ "Can't substitute unsolved for a poly type"
  True -> case Set.member x (Type.unsolved t) of
    True -> error $ "Occurs check"
    False -> Map.singleton x t

unifyForall :: Type.Id -> Type.Type -> Type.Id -> Type.Type -> Subs
unifyForall x1 t1 x2 t2 =
  let x3 = freshName (Set.union (Type.free t1) (Type.free t2))
   in unify (subsVar x1 (Type.Var x3) t1) (subsVar x2 (Type.Var x3) t2)

subs :: Subs -> Type.Type -> Type.Type
subs s t = case t of
  Type.Unsolved x | Just t2 <- Map.lookup x s -> t2
  Type.Forall x t -> Type.Forall x (subs s t)
  Type.Arrow t1 t2 -> Type.Arrow (subs s t1) (subs s t2)
  t -> t

subsVar :: Type.Id -> Type.Type -> Type.Type -> Type.Type
subsVar x t2 t = case t of
  Type.Var y | x == y -> t2
  Type.Forall y t | x /= y -> Type.Forall y (subsVar x t2 t)
  Type.Arrow ta tr -> Type.Arrow (subsVar x t2 ta) (subsVar x t2 tr)
  t -> t

combineSubs :: Subs -> Subs -> Subs
combineSubs s1 s2 = foldl addSub s1 (Map.toList s2)
  where
    addSub :: Subs -> (Type.Ext, Type.Type) -> Subs
    addSub s (x, t) = case Map.lookup x s of
      Nothing -> Map.insert x (subs s t) s
      Just t2 -> combineSubs s (unify (subs s t) t2)

freshName :: Set.Set Type.Id -> Type.Id
freshName xs = head $ filter (\x -> not $ Set.member x xs) $ map return $ ['a' .. 'z']
