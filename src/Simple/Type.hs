module Simple.Type where

import qualified Data.Set as Set

type Id = String

type Ext = Int

data Type
  = Var Id
  | Unsolved Ext
  | Forall Id Type
  | Arrow Type Type
  | Unit
  deriving (Show, Eq)

mono :: Type -> Bool
mono t = case t of
  Forall _ _ -> False
  Arrow t1 t2 -> mono t1 && mono t2
  _ -> True

unsolved :: Type -> Set.Set Ext
unsolved t = case t of
  Unsolved x -> Set.singleton x
  Forall _ t -> unsolved t
  Arrow t1 t2 -> unsolved t1 `Set.union` unsolved t2
  _ -> Set.empty

subsVar :: Id -> Type -> Type -> Type
subsVar x t2 t = case t of
  Var y | x == y -> t2
  Forall y t | x /= y -> Forall y (subsVar x t2 t)
  Arrow ta tr -> Arrow (subsVar x t2 ta) (subsVar x t2 tr)
  t -> t

subsExt :: Ext -> Type -> Type -> Type
subsExt x t2 t = case t of
  Unsolved y | x == y -> t2
  Forall y t -> Forall y (subsExt x t2 t)
  Arrow ta tr -> Arrow (subsExt x t2 ta) (subsExt x t2 tr)
  t -> t
