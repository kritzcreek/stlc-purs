module Pretty where

import Prelude

import AST (Expr(..), Type(..))
import Dodo (Doc, (<+>))
import Dodo as D

print' :: ∀ a. Doc a -> String
print' = D.print D.plainText D.twoSpaces

annTy :: D.Printer String Type String
annTy = D.Printer
  { emptyBuffer: ""
  , writeText: \_ str buff -> buff <> str
  , writeIndent: \_ str buff -> buff <> str
  , writeBreak: \buff -> buff <> "\n"
  , enterAnnotation: \_ _ buff -> buff <> "("
  , leaveAnnotation: \ty _ buff -> buff <> " : " <> typ ty <> ")"
  , flushBuffer: \buff -> buff
  }

printTy :: Doc Type -> String
printTy = D.print annTy D.twoSpaces

expr' :: ∀ a. Show a => Expr a -> String
expr' = expr (D.text <<< show)

expr :: ∀ a b. (a -> Doc b) -> Expr a -> String
expr p = print' <<< pExpr p

typ :: Type -> String
typ = print' <<< pTyp

pExpr' :: ∀ a b. Show a => Expr a -> Doc b
pExpr' = pExpr (D.text <<< show)

pExpr :: ∀ a b. (a -> Doc b) -> Expr a -> Doc b
pExpr pVar = go false
  where
    go nested = case _ of
      IntLit l ->
        D.text (show l)
      BoolLit l ->
        D.text (show l)
      Var v ->
        pVar v
      Lambda v t b ->
        D.text "\\" <> pVar v <+> D.text ":" <+> pTyp t <> D.text "." <+> pExpr pVar b
      App f a ->
        (if nested then paren else identity) (go true f <+> go true a)

paren :: ∀ a. D.Doc a -> D.Doc a
paren = D.enclose (D.text "(") (D.text ")")

pTyp :: ∀ a. Type -> Doc a
pTyp = go false
  where
    go nested = case _ of
      IntTy ->
        D.text "Int"
      BoolTy ->
        D.text "Bool"
      FuncTy a r ->
        (if nested then paren else identity)
        (go true a <+> D.text "→" <+> go false r)
