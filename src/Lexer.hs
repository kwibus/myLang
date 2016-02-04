module Lexer
where

import Text.ParserCombinators.Parsec.Number

import ParserType
import Text.Parsec
import Data.Char

identifier :: Parser String
identifier = do
  -- spaces
  first <- lower
  rest <- many alphaNum
  whiteSpaces
  return $ first : rest

symbol :: Char -> Parser ()
symbol c = do
    _ <- char c
    spaces

double :: Parser Double
double = do
    f <- parsec2parser $ sign <*> floating
    whiteSpaces
    return f

whiteSpace :: Parser Char
whiteSpace = satisfy (\ c -> isSpace c && ( c /= '\n' ))

whiteSpaces :: Parser ()
whiteSpaces = skipMany whiteSpace

indent :: Parser ()
indent = do
  newline <> whiteSpaces
  pos <- getPosition
  preIndet <- getState
  if head preIndet > sourceColumn pos
  then putState ( sourceColumn pos : preIndet)
  else unexpected "exptexted indentation"

newline :: Parser ()
newline = void $ char '\n'
