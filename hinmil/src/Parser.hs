module Parser (Parser, expr, name, equals) where

{-
<name> is a nonempty Latin-alphabetic string, other than "let" and "in"
<var> ::= <name>
<apply> ::= <paren-func> <paren-arg>
<lambda> ::= '\' <name> "->" <expr>
<let> ::= "let" <name> '=' <expr> "in" <expr>
<compound> = <apply> | <lambda> | <let>
<compound-func> = <lambda> | <let>
<paren-func> = <var> | <apply> | '(' <compound-func> ')'
<paren-arg> = <var> | '(' <compound> ')'
<expr> ::= <var> | <compound>
-}
import Expr

import           Control.Applicative (liftA2)
import           Control.Monad (void)
import           Control.Applicative ((<|>))
import           Data.Void (Void)
import           Text.Megaparsec ( Parsec, between, try, some )
import           Text.Megaparsec.Char (space, letterChar)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

type Name = String

symbol' :: String -> Parser String
symbol' = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol' "(") (symbol' ")")

equals :: Parser ()
equals = void $ symbol' "="

arrow :: Parser ()
arrow = void $ symbol' "->"

backslash :: Parser ()
backslash = void $ symbol' "\\"

letKW :: Parser ()
letKW = void $ symbol' "let"

inKW :: Parser ()
inKW = void $ symbol' "in"

name :: Parser Name
name = L.lexeme space $ some letterChar

var :: Parser Expr
var = do
  x <- name
  () <- case x of
          "let" -> fail "'let' is a reserved keyword"
          "in" -> fail "'in' is a reserved keyword"
          _ -> return ()
  return $ Var x

parenFunc :: Parser Expr
parenFunc = try var <|> try (parens compound) <|> apply

parenArg :: Parser Expr
parenArg = try var <|> parens compound

apply :: Parser Expr
apply = liftA2 Apply parenFunc parenArg

lambda :: Parser Expr
lambda = do
  try backslash
  param <- name
  arrow
  body <- expr
  return $ Lambda param body

letIn :: Parser Expr
letIn = do
  try letKW
  lhs <- name
  equals
  rhs <- expr
  inKW
  body <- expr
  return $ Let lhs rhs body

compound :: Parser Expr
compound = letIn <|> lambda <|> apply

expr :: Parser Expr
expr = try compound <|> var
