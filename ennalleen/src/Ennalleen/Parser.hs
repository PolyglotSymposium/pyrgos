module Ennalleen.Parser (ParsedExpr(..), parseType, parseExpr) where

import Data.Char (isAlpha)
import Data.Bifunctor (first)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Functor (($>), (<&>))
import Data.Void (Void)
import Ennalleen.BaseSyntax
{- One significant improvement to Haskell's module system would be the ability
   to mark imports private, so that the compiler would enforce you didn't
   re-export any of their types (or anything else). That is the real meaning of
   the law of Demeter. A certain number of imports---base, and anything that is
   conceptually just an extension of base---have to be the pond that all modules
   swim in together. But some dependencies should only affect a localized part
   of the codebase, and the blast radius of changes in those dependencies should
   be actively limited to that part of the codebase. The rest of the code just
   wants a parser here; it doesn't care that we are using Megaparsec. Note that
   this is not the same thing as saying we need to build the code to be abstract
   over any number of parser combinator libraries. We don't need to abstract
   over our dependencies, just encapsulate them.
-}
import Text.Megaparsec
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

class ParsedExpr a where
  injectLet :: Name -> a -> a -> a
  injectVar :: Name -> a
  injectInt :: Int -> a
  injectLam :: Name -> a -> a
  injectIfT :: a -> a -> a -> a
  injectAp :: a -> a -> a
  injectOp :: BinOp -> a -> a -> a

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

parseType :: String -> Either String Ty
parseType = first errorBundlePretty . parse ty "<input>"

name :: Parser Name
-- TODO better identifier parsing
name = L.lexeme space (takeWhile1P (Just "name") isAlpha) <&> Name

eLet :: ParsedExpr a => Parser a
eLet =
  pure injectLet
  <* keyword "$let" <*> name
  <* keyword "="    <*> expr
  <* keyword "$in"  <*> expr

eVar :: ParsedExpr a => Parser a
eVar = name <&> injectVar

eInt :: ParsedExpr a => Parser a
eInt = L.lexeme space L.decimal <&> injectInt

operators :: ParsedExpr a => [[Operator Parser a]]
operators =
  -- Brent Yorgey's hack for parsing function application
  -- https://github.com/mrkkrp/megaparsec/issues/245#issue-249916596
  -- The naive solution leads to infinite left-recursion and eats up all your
  -- computer's memory
  [ [InfixN (symbol "" $> injectAp)]
  , [InfixN (symbol "*" $> injectOp Times)]
  , [InfixN (symbol "+" $> injectOp Plus), InfixL (symbol "-" $> injectOp Minus)]
  , [InfixN (symbol "<" $> injectOp Less), InfixN (symbol "==" $> injectOp Equal)]
  ]

terminalExpr :: ParsedExpr a => Parser a
terminalExpr = try eVar <|> eInt

eIf :: ParsedExpr a => Parser a
eIf =
  pure injectIfT
  <* keyword "$if"   <*> expr
  <* keyword "$then" <*> expr
  <* keyword "$else" <*> expr

eLambda :: ParsedExpr a => Parser a
eLambda =
  pure injectLam  <*> name
  <* keyword "=>" <*> expr

nonInfixExpr :: ParsedExpr a => Parser a
-- TODO support parentheticals
nonInfixExpr = try eIf <|> try eLambda <|> try eLet <|> terminalExpr

expr :: ParsedExpr a => Parser a
expr = makeExprParser nonInfixExpr operators

parseExpr :: ParsedExpr a => String -> Either String a
parseExpr = first errorBundlePretty . parse expr "<input>"
