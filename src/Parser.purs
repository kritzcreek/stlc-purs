module Parser (parseExpr, parseType) where

import Prelude

import AST (Expr(..), Type(..))
import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Identity (Identity)
import Data.List as List
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser as P
import Text.Parsing.Parser.Language (javaStyle)
import Text.Parsing.Parser.String as PS
import Text.Parsing.Parser.Token as T

type Parser = P.Parser String

l :: T.GenTokenParser String Identity
l = T.makeTokenParser langStyle
  where
    javaDef = T.unGenLanguageDef javaStyle
    langStyle = T.LanguageDef langDef
    langDef =
      javaDef
        { reservedNames = [ "true", "false", "if", "else", "let" ]
        , reservedOpNames = [ "+", "-", "*", "/", "<", ">", "<=", ">=", "==" ]
        }

expr :: Parser (Expr String)
expr = fix \e ->
  List.some (exprAtom e) >>= case _ of
    List.Nil ->
      unsafeCrashWith "Empty expr atoms"
    List.Cons x List.Nil ->
      pure x
    List.Cons f args ->
      pure (List.foldl App f args)


-- expr :: Parser (Expr String)
-- expr = fix \e ->
--   buildExprParser
--     [ [ Infix (l.reservedOp "/" $> BinOp Div) AssocRight
--       , Infix (l.reservedOp "*" $> BinOp Mul) AssocRight
--       ]
--     , [ Infix (l.reservedOp "-" $> BinOp Sub) AssocRight
--       , Infix (l.reservedOp "+" $> BinOp Add) AssocRight
--       ]
--     , [ Infix (l.reservedOp "<" $> BinOp Lt) AssocRight
--       , Infix (l.reservedOp ">" $> BinOp Gt) AssocRight
--       ]
--     , [ Infix (l.reservedOp "<=" $> BinOp Lte) AssocRight
--       , Infix (l.reservedOp ">=" $> BinOp Gte) AssocRight
--       , Infix (l.reservedOp "==" $> BinOp Eq) AssocRight
--       ]
--     ] (atom e)

exprAtom :: Parser (Expr String) -> Parser (Expr String)
exprAtom e =
  map IntLit l.integer <|>
  l.reserved "true" $> BoolLit true <|>
  l.reserved "false" $> BoolLit true <|>
  Var <$> l.identifier <|>
  l.symbol "(" *>  expr <* l.symbol ")" <|>
  Lambda
    <$> (l.symbol "\\" *> l.identifier)
    <*> (l.symbol ":" *> typ)
    <*> (l.symbol "." *> expr)

typ :: Parser Type
typ = fix \t -> do
  t' <- typAtom t
  FuncTy t' <$> (l.symbol "->" *> t) <|> pure t'

typAtom :: Parser Type -> Parser Type
typAtom t =
  l.reserved "Int" $> IntTy <|>
  l.reserved "Bool" $> BoolTy <|>
  l.symbol "(" *> typ <* l.symbol ")"

parseExpr :: String -> Either String (Expr String)
parseExpr i = lmap P.parseErrorMessage (P.runParser i (l.whiteSpace *> expr <* PS.eof))

parseType :: String -> Either String Type
parseType i = lmap P.parseErrorMessage (P.runParser i (l.whiteSpace *> typ <* PS.eof))
