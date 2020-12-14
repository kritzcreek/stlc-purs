module Main where

import Prelude

import Check as Check
import Data.Either (Either(..))
import Dodo ((<+>))
import Dodo as Dodo
import Effect (Effect)
import Effect.Class.Console as Console
import Parser as Parser
import Pretty as Pretty

exp :: String
exp = """
\g : Bool -> Int. \f : Int -> Int. \y : Int. g (f y)
"""

compile :: String -> Either String String
compile input = do
  parsed <- Parser.parseExpr input
  { ty, expr: checked } <- Check.checkExpr parsed
  pure (Pretty.print' (Pretty.pExpr Check.pAnn checked <+> Dodo.text ":" <+> Pretty.pTyp ty))

main :: Effect Unit
main =
  case compile exp of
    Left err ->
      Console.log err
    Right r ->
      Console.log r
