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
check ctx e t = undefined

infer :: Ctx.Ctx -> Source.Expr -> TI (Ctx.Ctx, Type.Type, Typed.Expr)
infer ctx e = undefined
