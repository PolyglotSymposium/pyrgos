-- | The parser for the meta/human language, which is simply a
-- | minimally-human-friendly syntax for meta/eval.
module Metacore.Human.Parser (topLevel) where

import           Control.Applicative (liftA2)
import           Control.Monad (void)
import           Data.Char (toUpper)
import           Data.MExpr.Radix46 (is46, decode46)
import           Data.MExpr (Symbol(..))
import           Data.Void (Void)
import           Metacore.Eval.AST (TopLevel(..), Terminal(..), Expr(..))
import           Text.Megaparsec ( Parsec, between, takeWhile1P, try, (<|>)
                                 , manyTill)
import           Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

msymbol :: Parser Symbol
msymbol = do
  s <- L.lexeme space $ takeWhile1P (Just "symbol") (is46 . toUpper)
  case decode46 $ fmap toUpper s of
    Nothing -> fail "Symbol literal too large"
    Just x -> return $ Symbol x

symbolMark :: Parser ()
symbolMark = void $ symbol "#"

charMark :: Parser ()
charMark = void $ symbol "'"

natMark :: Parser ()
natMark = void $ symbol "+"

strMark :: Parser ()
strMark = void $ symbol "\""

-- | The tilde ~ is not a valid symbol character, so we use it to prefix
-- | syntactic operators.
defMark :: Parser ()
defMark = void $ symbol "~="

-- | The tilde ~ is not a valid symbol character, so we use it to prefix
-- | syntactic operators.
funMark :: Parser ()
funMark = void $ symbol "~>"

ap :: Parser (Expr Terminal)
ap = liftA2 Ap expr $ parens expr

fun :: Parser (Expr Terminal)
fun = liftA2 Lambda (msymbol <* funMark) expr

def :: Parser TopLevel
def = liftA2 Def (msymbol <* defMark) expr

stringLiteral :: Parser String
stringLiteral = strMark >> manyTill L.charLiteral strMark

symbolLiteral :: Parser Symbol
symbolLiteral = symbolMark *> msymbol

characterLiteral :: Parser Char
characterLiteral = between charMark charMark L.charLiteral

nat :: Parser Integer
nat = natMark *> L.decimal

terminal :: Parser Terminal
terminal =
  try (String <$> stringLiteral)
  <|> try (Nat <$> nat)
  <|> try (TSymbol <$> symbolLiteral)
  <|> (Char <$> characterLiteral)

expr :: Parser (Expr Terminal)
expr =
  try fun
  <|> try (T <$> terminal)
  <|> try (Var <$> msymbol)
  <|> ap

topLevel :: Parser TopLevel
topLevel = try def <|> fmap Eval expr
