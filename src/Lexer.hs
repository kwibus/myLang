module Lexer
where

import Text.ParserCombinators.Parsec.Number
import Text.Parsec hiding (tokens, token)
import Data.Foldable (msum)
import Data.Char

type Lexer a = Parsec String () a

data TokenPos = TokenPos {getToken :: Token, getposition :: SourcePos}
data Token = Identifier String
           | CapIdentifier String
           | Number Double
           | ReservedS ReservedSymbol
           | ReservedW ReservedWord
           deriving Eq

data ReservedSymbol = Plus
                    | Multiply
                    | Equal
                    | Is
                    | BackSlash
                    | Dot
                    | Semicolon
                    | LeftParenthesis
                    | RightParenthesis
                    deriving (Bounded, Enum, Eq)

instance Show TokenPos where
  show = show . getToken

instance Show Token where
  show (Identifier str) = show str -- show to enclose in "" to diferentiate with reservedWords
  show (CapIdentifier str) = show str
  show (Number n ) = show n
  show (ReservedS s) = "'" ++ fromSymbol s ++ "'"
  show (ReservedW w) = show w

reservedSymbols :: [ReservedSymbol]
reservedSymbols = [minBound .. maxBound]

fromSymbol :: ReservedSymbol -> String
fromSymbol Plus = "+"
fromSymbol Multiply = "*"
fromSymbol Equal = "=="
fromSymbol Is = "="
fromSymbol BackSlash = "\\"
fromSymbol Dot = "."
fromSymbol Semicolon = ";"
fromSymbol LeftParenthesis = "("
fromSymbol RightParenthesis = ")"

toString :: ReservedWord -> String
toString w = case show w of
        (x : xs) -> toLower x : xs
        [] -> error "incorrect resevedw: emptyString"
            -- thic can never happen with default show

data ReservedWord = Let | In deriving (Show, Bounded, Enum, Eq)

reservedWords :: [ReservedWord]
reservedWords = [minBound .. maxBound]

lexer :: String -> Either ParseError [TokenPos]
lexer = parse tokens ""

parsePos :: Lexer Token -> Lexer TokenPos
parsePos p = TokenPos <$> p <*> getPosition

tokens :: Lexer [TokenPos]
tokens = spaces *> many (token <* spaces)

token :: Lexer TokenPos
token = parsePos $ choice
     [ msum (map symbol reservedSymbols)
     , msum (map keyWord reservedWords)
     , capIdentifier
     , identifier
     , double
     ]

capIdentifier :: Lexer Token
capIdentifier = do
  first <- upper
  rest <- many alphaNum
  return $ CapIdentifier $ first : rest

identifier :: Lexer Token
identifier = do
  first <- lower
  rest <- many alphaNum
  return $ Identifier $ first : rest

symbol :: ReservedSymbol -> Lexer Token
symbol s = try $ string (fromSymbol s) >> return ( ReservedS s)

keyWord :: ReservedWord -> Lexer Token
keyWord w = try $ string (toString w) >> notFollowedBy alphaNum >> return (ReservedW w)

double :: Lexer Token
double = do
    f <- sign <*> floating
    spaces
    return $ Number f
