module HR where

import qualified Simple.Type as Type
import qualified Simple.Unify as Unify

t1 = Type.Forall "x" $ Type.Arrow (Type.Var "x") (Type.Unsolved 123)

t2 = Type.Forall "y" $ Type.Arrow (Type.Var "y") Type.Unit

s = Unify.unify t1 t2
