module Lexer
where

import Text.ParserCombinators.Parsec.Number

import ParserType
import Text.Parsec

identifier :: Parser String
identifier = do
  -- spaces
  first <- lower
  rest <- many alphaNum
  spaces
  return $ first : rest

symbol :: Char -> Parser ()
symbol c = do
    _ <- char c
    spaces

double :: Parser Double
double = do
    f <- parsec2parser $ sign <*> floating
    spaces
    return f
