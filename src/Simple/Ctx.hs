module Simple.Ctx where

import qualified Simple.Expr as Expr
import qualified Simple.Type as Type

data Part
  = UnsolvedExt Type.Ext
  | SolvedExt Type.Ext Type.Type
  | Var Type.Id
  | Binding Expr.Id Type.Type
  | Marker Type.Ext
  deriving (Show, Eq)

type Ctx = [Part]

subs :: Ctx -> Type.Type -> Type.Type
subs ctx t = case ctx of
  [] -> t
  (SolvedExt x t2 : ctx) -> subs ctx (Type.subsExt x t2 t)
  (_ : ctx) -> subs ctx t

lookupBinding :: Ctx -> Expr.Id -> Maybe Type.Type
lookupBinding ctx x = case ctx of
  [] -> Nothing
  (Binding y t : _) | x == y -> Just t
  (_ : ctx) -> lookupBinding ctx x

splitPart :: Ctx -> Part -> Maybe (Ctx, Ctx)
splitPart ctx p = case ctx of
  [] -> Nothing
  (p2 : ctx) | p == p2 -> Just ([], ctx)
  (p2 : ctx) -> do
    (hs, ts) <- splitPart ctx p
    return (p2 : hs, ts)

splitMarker :: Ctx -> Type.Ext -> Maybe (Ctx, Ctx)
splitMarker ctx x = splitPart ctx (Marker x)

splitUnsolved :: Ctx -> Type.Ext -> Maybe (Ctx, Ctx)
splitUnsolved ctx x = splitPart ctx (UnsolvedExt x)
