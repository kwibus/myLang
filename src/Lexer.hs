module Lexer (
  lexer,
  Token (..),
  TokenPos (getposition, getToken),
  ReservedSymbol (..)
  )
where

import Text.ParserCombinators.Parsec.Number
import Text.Parsec hiding (tokens, token)
import Data.Foldable (msum)

type Lexer a = Parsec String () a

data TokenPos = TokenPos {getToken :: Token, getposition :: SourcePos}
data Token = Identifier String
           | Number Double
           | ReservedS ReservedSymbol
           | ReservedW ReservedWord
           deriving Eq

data ReservedSymbol = Plus
                    | Multiply
                    | Equal
                    | BackSlash
                    | Dot
                    | LeftParenthesis
                    | RightParenthesis
                    deriving (Bounded, Enum, Eq)

instance Show TokenPos where
  show = show . getToken

instance Show Token where
  show (Identifier str) = show str
  show (Number n ) = show n
  show (ReservedS s) = "'" ++ (toChar s : "'")
  show (ReservedW w) = show w

reservedSymbols :: [ReservedSymbol]
reservedSymbols = [minBound .. maxBound]

toChar :: ReservedSymbol -> Char
toChar Plus = '+'
toChar Multiply = '*'
toChar Equal = '='
toChar BackSlash = '\\'
toChar Dot = '.'
toChar LeftParenthesis = '('
toChar RightParenthesis = ')'

data ReservedWord = Let | In deriving (Show, Eq)

lexer :: String -> Either ParseError [TokenPos]
lexer = parse tokens ""

parsePos :: Lexer Token -> Lexer TokenPos
parsePos p = TokenPos <$> p <*> getPosition

tokens :: Lexer [TokenPos]
tokens = spaces *> many (token <* spaces)

token :: Lexer TokenPos
token = parsePos ( msum (map symbol reservedSymbols) <|> identifier <|> double)

identifier :: Lexer Token
identifier = do
  first <- lower
  rest <- many alphaNum
  return $ Identifier $ first : rest

symbol :: ReservedSymbol -> Lexer Token
symbol s = char (toChar s) >> return ( ReservedS s)

double :: Lexer Token
double = do
    f <- sign <*> floating
    spaces
    return $ Number f
