module Lexer
where

import Text.ParserCombinators.Parsec.Number
import Text.Parsec.String
import Text.Parsec

import Control.Applicative hiding (many)

identifier :: Parser String
identifier = do
  -- spaces
  first <- lower
  rest <- many alphaNum
  spaces
  return $ first : rest


symbol :: Char -> Parser ()
symbol c = do
    -- spaces
    s <- char c
    spaces
    -- return s

double :: Parser Double
double = do
    f <- sign <*> floating
    spaces
    return f
