-- | The meta/eval language's parser
module Metacore.Eval.Parser (topLevel) where

import           Data.MExpr (deemify)
import qualified Data.MExpr.Parser as MEParser
import           Data.Void (Void)
import           Metacore.Eval.AST
import           Text.Megaparsec (Parsec)

type Parser = Parsec Void String

topLevel :: Parser TopLevel
topLevel = do
  mexpr <- MEParser.mexpr
  case deemify mexpr of
    Nothing -> fail "meta/eval syntax error"
    Just x -> return x
