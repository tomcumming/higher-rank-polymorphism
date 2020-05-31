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

free :: Type -> Set.Set Id
free t = case t of
  Var x -> Set.singleton x
  Forall x t -> Set.delete x (free t)
  Arrow t1 t2 -> free t1 `Set.union` free t2
  _ -> Set.empty
