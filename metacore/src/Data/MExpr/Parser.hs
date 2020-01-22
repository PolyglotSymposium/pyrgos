module Data.MExpr.Parser (mexpr) where

import           Control.Applicative (liftA2)
import           Control.Monad (void)
import           Data.MExpr (MExpr(..))
import           Data.MExpr.Sugar (is46, decode46)
import           Data.MExpr.Symbol (Symbol, isSymbolChar, avowSymbol)
import           Data.Void (Void)
import           Data.Word (Word64)
import           Text.Megaparsec ( Parsec, between, takeWhile1P, try, (<|>)
                                 , manyTill, many)
import           Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

msymbol :: Parser Symbol
msymbol = avowSymbol <$> takeWhile1P (Just "symbol") isSymbolChar

comma :: Parser ()
comma = void $ symbol ","

symbolMark :: Parser ()
symbolMark = void $ symbol "#"

charMark :: Parser ()
charMark = void $ symbol "'"

strMark :: Parser ()
strMark = void $ symbol "\""

args :: Parser [MExpr]
args =
  let none = return []
      some = liftA2 (:) mexpr $ many (comma *> mexpr)
  in parens $ try some <|> none

stringLiteral :: Parser String
stringLiteral = strMark >> manyTill L.charLiteral strMark

symbolLiteral :: Parser Word64
symbolLiteral = do
  symbolMark
  s <- takeWhile1P (Just "symbol") is46
  case decode46 s of
    Nothing -> fail "Symbol literal too large"
    Just x -> return x

characterLiteral :: Parser Char
characterLiteral = between charMark charMark L.charLiteral

mexpr :: Parser MExpr
mexpr =
  try (StrLit <$> stringLiteral)
  <|> try (NatLit <$> L.decimal)
  <|> try (SymLit <$> symbolLiteral)
  <|> try (ChrLit <$> characterLiteral)
  <|> liftA2 MExpr msymbol args
