module Ennalleen.Parser where

import Data.Char (isAlpha)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Functor (($>), (<&>))
import Data.Void (Void)
import Ennalleen.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

keyword :: String -> Parser String
keyword = symbol

tBool :: Parser Ty
tBool = symbol "Bool" $> TBool

tInt :: Parser Ty
tInt = symbol "Int" $> TInt

terminalType :: Parser Ty
terminalType = try tBool <|> tInt

ty :: Parser Ty
-- TODO support parentheticals
ty = makeExprParser terminalType [[InfixR (symbol "->" $> TFunc)]]

name :: Parser Name
-- TODO better identifier parsing
name = L.lexeme space (takeWhile1P (Just "name") isAlpha) <&> Name

eLet :: Parser Expr
eLet =
  pure                ELet
  <* keyword "$let" <*> name
  <* keyword "="   <*> expr
  <* keyword "$in"  <*> expr

eVar :: Parser Expr
eVar = name <&> EVar

eInt :: Parser Expr
eInt = L.lexeme space L.decimal <&> EInt

operators :: [[Operator Parser Expr]]
operators =
  -- Brent Yorgey's hack for parsing function application
  -- https://github.com/mrkkrp/megaparsec/issues/245#issue-249916596
  -- The naive solution leads to infinite left-recursion and eats up all your
  -- computer's memory
  [ [InfixN (symbol "" $> EApply)]
  , [InfixN (symbol "*" $> EBinOp Times)]
  , [InfixN (symbol "+" $> EBinOp Plus), InfixL (symbol "-" $> EBinOp Minus)]
  , [InfixN (symbol "<" $> EBinOp Less), InfixN (symbol "==" $> EBinOp Equal)]
  ]

terminalExpr :: Parser Expr
terminalExpr = try eVar <|> eInt

eIf :: Parser Expr
eIf =
  pure                   EIf
  <* keyword "$if"   <*> expr
  <* keyword "$then" <*> expr
  <* keyword "$else" <*> expr

eLambda :: Parser Expr
eLambda =
  pure ELambda    <*> name
  <* keyword "=>" <*> expr

nonInfixExpr :: Parser Expr
-- TODO support parentheticals
nonInfixExpr = try eIf <|> try eLambda <|> try eLet <|> terminalExpr

expr :: Parser Expr
expr = makeExprParser nonInfixExpr operators
