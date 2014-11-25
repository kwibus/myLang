module Lexer
where


import Text.Parsec.String
import Text.Parsec

identifier :: Parser String
identifier = do
  spaces
  first <-lower
  rest <- many alphaNum
  spaces
  return $ first:rest


symbol :: Char -> Parser Char 
symbol c = do
    spaces
    s <-char c 
    spaces
    return s
