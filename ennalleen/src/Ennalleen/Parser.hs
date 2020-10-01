module Ennalleen.Parser where

import Control.Applicative ((<|>))
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Functor (($>))
import Data.Void (Void)
import Ennalleen.Syntax
import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (space)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

symbol :: String -> Parser String
symbol = L.symbol space

tBoolP :: Parser Ty
tBoolP = symbol "Bool" $> TBool

tIntP :: Parser Ty
tIntP = symbol "Int" $> TInt

terminalTypeP :: Parser Ty
terminalTypeP = tBoolP <|> tIntP

typeP :: Parser Ty
typeP = makeExprParser terminalTypeP [[InfixR (symbol "->" $> TFunc)]]
