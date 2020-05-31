module HR where

import qualified Data.Map as Map
import qualified Simple.Check as Check
import qualified Simple.Expr.Source as Source
import Simple.TI (runTI)
import qualified Simple.Type as Type
import qualified Simple.Unify as Unify

ctx :: Map.Map String Type.Type
ctx =
  Map.fromList
    [ ("id", Type.Forall "a" $ Type.Arrow (Type.Var "a") (Type.Var "a")),
      ("f", Type.Arrow (Type.Arrow Type.Unit Type.Unit) Type.Unit),
      ("g", Type.Arrow (Type.Forall "b" $ Type.Arrow (Type.Var "b") (Type.Var "b")) Type.Unit)
    ]

testE :: Source.Expr
testE = Source.App (Source.Var "g") (Source.Var "id")

testE2 :: Source.Expr
testE2 = Source.App (Source.Var "f") (Source.Var "id")
