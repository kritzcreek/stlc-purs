module AST where

import Prelude

data Type
  = IntTy
  | BoolTy
  | FuncTy Type Type

derive instance eqType :: Eq Type

data Expr a
  = IntLit Int
  | BoolLit Boolean
  | Var a
  | Lambda a Type (Expr a)
  | App (Expr a) (Expr a)

derive instance eqExpr :: Eq a => Eq (Expr a)
