module Check where

import Prelude

import AST (Expr(..), Type(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Dodo (Doc, (<+>), (</>))
import Dodo as D
import Pretty as Pretty

type Ann = { name :: String, ty :: Type }
type Env = Map String Type

data TyErr
  = WrongArg { arg :: Expr Ann, actual :: Type, expected :: Type }
  | WrongFunc { expr :: Expr Ann, ty :: Type }
  | UnknownVar String

prettyError :: TyErr -> String
prettyError = Pretty.printTy <<< pErr

pAnn :: Ann -> Doc Type
pAnn { name, ty } = D.annotate ty (D.text name)
  -- Pretty.paren (D.text name <+> D.text ":" <+> Pretty.pTyp ty)

pErr :: TyErr -> Doc Type
pErr = case _ of
  WrongArg { arg, actual, expected } ->
    (D.text "Expected type" <+> Pretty.pTyp expected <> D.text ",") </>
    (D.text "but" <+> Pretty.pExpr pAnn arg <+> D.text "has type" <+> Pretty.pTyp actual)
  WrongFunc { expr, ty } ->
    (D.text "Expected a function, but" <+> Pretty.pExpr pAnn expr) </>
    (D.text "has type" <+> Pretty.pTyp ty)
  UnknownVar v -> D.text "Unknown variable:" <+> D.text v

checkExpr :: Expr String -> Either String { ty :: Type, expr :: Expr Ann }
checkExpr = lmap prettyError <<< check Map.empty

check :: Env -> Expr String -> Either TyErr { ty :: Type, expr :: Expr Ann }
check env e = case e of
  IntLit x ->
    Right { ty: IntTy, expr: IntLit x }
  BoolLit x ->
    Right { ty: BoolTy, expr: BoolLit x }
  Var v ->
    case Map.lookup v env of
      Nothing ->
        Left (UnknownVar v)
      Just ty ->
        Right { ty, expr: Var { name: v, ty } }

  Lambda name tyBinder b -> ado
    body <- check (Map.insert name tyBinder env) b
    in { ty: FuncTy tyBinder body.ty, expr: Lambda { name, ty: tyBinder } tyBinder body.expr }
  App f a -> do
    func <- check env f
    case func.ty of
      FuncTy argTy resTy -> do
        arg <- check env a
        if arg.ty == argTy
          then Right { ty: resTy, expr: App func.expr arg.expr }
          else Left (WrongArg { arg: arg.expr, actual: arg.ty, expected: argTy })
      _ ->
        Left (WrongFunc func)
