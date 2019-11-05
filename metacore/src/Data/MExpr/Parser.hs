module Data.MExpr.Parser (mexpr) where

import           Data.MExpr (MExpr(..))
import           Data.MExpr.Symbol (Symbol, isSymbolChar, avowSymbol)
import           Control.Applicative (liftA2)
import           Control.Monad (void)
import           Data.Void (Void)
import           Text.Megaparsec ( Parsec, between, takeWhile1P, try, (<|>)
                                 , manyTill, many)
import           Text.Megaparsec.Char (char, space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

msymbol :: Parser Symbol
msymbol = fmap avowSymbol $ takeWhile1P (Just "symbol") isSymbolChar

comma :: Parser ()
comma = void $ symbol ","

args :: Parser [MExpr]
args =
  let none = return []
      some = liftA2 (:) mexpr $ many (comma *> mexpr)
  in parens $ try some <|> none

stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

mexpr :: Parser MExpr
mexpr = try (Literal <$> stringLiteral) <|> liftA2 MExpr msymbol args
