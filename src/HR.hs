module HR where

import qualified Data.Map as Map
import qualified Simple.Check as Check
import qualified Simple.Ctx as Ctx
import qualified Simple.Expr.Source as Source
import qualified Simple.Expr.Typed as Typed
import Simple.TI (runTI)
import qualified Simple.Type as Type
import qualified Simple.Unify as Unify

ctx :: Ctx.Ctx
ctx =
  [ Ctx.Binding "id" $ Type.Forall "a" $ Type.Arrow (Type.Var "a") (Type.Var "a"),
    Ctx.Binding "f" $ Type.Arrow (Type.Arrow Type.Unit Type.Unit) Type.Unit,
    Ctx.Binding "g" $ Type.Arrow (Type.Forall "b" $ Type.Arrow (Type.Var "b") (Type.Var "b")) Type.Unit,
    Ctx.Binding "escape" $ Type.Forall "b" $
      Type.Arrow
        (Type.Forall "a" $ Type.Arrow (Type.Var "a") (Type.Var "b"))
        (Type.Var "b")
  ]

testE :: Source.Expr
testE = Source.App (Source.Var "g") (Source.Var "id")

testE2 :: Source.Expr
testE2 = Source.App (Source.Var "f") (Source.Var "id")

testE3 = Source.App (Source.Var "g") (Source.Abs "x" $ Source.Var "x")

testE4 = Source.App (Source.Var "f") (Source.Abs "x" $ Source.Var "x")

testEscape = Source.App (Source.Var "escape") (Source.Abs "x" $ Source.Var "x")

checkExpr :: Source.Expr -> IO ()
checkExpr e = do
  let (s, t, ce) = runTI $ Check.infer ctx e
  print (s, t, ce)
