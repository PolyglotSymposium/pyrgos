module Parser (Parser, expr, name, equals, decl, topLevel) where

import Expr

import           Control.Monad (void)
import           Control.Applicative ((<|>))
import           Data.Foldable (foldl')
import           Data.Void (Void)
import           Text.Megaparsec (Parsec, between, try, some, manyTill)
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

strMark :: Parser ()
strMark = void $ symbol' "\""

letKW :: Parser ()
letKW = void $ symbol' "let"

inKW :: Parser ()
inKW = void $ symbol' "in"

name :: Parser Name
name = L.lexeme space $ some letterChar

var :: Parser Expr
var = try $ do
  x <- name
  () <- case x of
          "let" -> fail "'let' is a reserved keyword"
          "in" -> fail "'in' is a reserved keyword"
          _ -> return ()
  return $ Var x

int :: Parser Expr
int = (Lit . IntLit) <$> L.lexeme space L.decimal

string :: Parser Expr
string =
  let p = try strMark >> manyTill L.charLiteral strMark
  in (Lit . StringLit) <$> L.lexeme space p

terminal :: Parser Expr
terminal = int <|> string <|> var

parenFunc :: Parser Expr
parenFunc = terminal <|> try (parens compound) <|> apply

-- | Parenthesized argument (except if terminal)
parenArg :: Parser Expr
parenArg = terminal <|> parens compound

apply :: Parser Expr
apply = do
  f <- parenFunc
  xs <- some parenArg
  return $ foldl' Apply f xs

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
expr = try compound <|> terminal

define :: Parser Decl
define = do
  lhs <- try (name <* equals)
  rhs <- expr
  return $ Define lhs rhs

decl :: Parser Decl
decl = define

tlExpr :: Parser TopLevel
tlExpr = TLExpr <$> expr

tlDecl :: Parser TopLevel
tlDecl = TLDecl <$> decl

topLevel :: Parser TopLevel
topLevel = tlDecl <|> tlExpr
