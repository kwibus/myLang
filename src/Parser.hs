module Parser (
  parseString,
  ParseError (..)
) where


import Data.Either.Unwrap
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Text.Parsec.Pos
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Combinator
import qualified Text.Parsec.Error as PS

import InfixFix
import Value
import Lambda
import Lexer
import Operator
import Info
import Name

type Parser a = ParsecT [TokenPos] () (Either InfixError) a
data ParseError = Infix InfixError
                | Parsec PS.ParseError
                | Lexer PS.ParseError
                deriving (Show, Eq)

parse :: Parser a -> String -> [TokenPos] -> Either ParseError a
parse parser file sting = case runParserT parser () file sting of
    Right a -> mapLeft Parsec a
    Left e -> Left $ Infix e

pSatisfy :: (Token -> Bool) -> Parser Token
pSatisfy f = getToken <$> tokenPrim showChar nextPos testChar
   where
     showChar = show . getToken
     testChar x = if f (getToken x) then Just x else Nothing
     nextPos _ x _ = getposition x

pSymbol :: ReservedSymbol -> Parser ()
pSymbol s = void $ pSatisfy (== ReservedS s)

pIdentifier :: Parser String
pIdentifier = do
  Identifier str <- pSatisfy (\ x -> case x of
    Identifier _ -> True
    _ -> False)
  return str

pDouble :: Parser Double
pDouble = do
  Number n <- pSatisfy (\ x -> case x of
    Number _ -> True
    _ -> False)
  return n

pLambda :: Parser Expresion
pLambda = do
    pos <- getPosition
    pSymbol BackSlash
    ns <- many pIdentifier -- Todo 1) fix location 2) give warning Shadowin variable names (\a a b.t)
    pSymbol Dot
    term <- pLambdaTerm
    loc <- pLoc pos
    return $ foldr (Lambda loc) term (Name <$> ns)

pApplication :: Parser Expresion
pApplication = do
    terms <- many pLambdaTerm'
    case fixInfix terms of
        Left erro -> lift $ Left erro
        Right exps -> return exps

pValue :: Parser Expresion
pValue = do
    pos <- getPosition
    v <- choice [fmap MyDouble pDouble]
    loc <- pLoc pos
    return $ Val loc v

pLambdaTerm' :: Parser (Expresion, Bool)
pLambdaTerm' = choice parsers
    where parsers = pOperator : fmap (fmap (\ p -> (p, False))) [pLambda, pVar, pParentheses, pValue]

pLambdaTerm :: Parser Expresion
pLambdaTerm = pApplication

pVar :: Parser Expresion
pVar = do
    pos <- getPosition
    n <- pIdentifier
    loc <- pLoc pos
    return $ Var loc (Name n)

pLine :: Parser Expresion
pLine = do
    term <- pLambdaTerm
    eof
    return term

pOperator :: Parser (Expresion, Bool)
pOperator = do
    pos <- getPosition
    o <- choice [pPlus, pMultiply ]
    loc <- pLoc pos
    return (Val loc o, True)

pLoc :: SourcePos -> Parser Loc
pLoc start = do
    end <- getPosition
    return Loc
        { srcFile = sourceName start
        , lineStart = sourceLine start
        , columnStart = sourceColumn start
        , lineEnd = sourceLine end
        , columnEnd = sourceColumn end}

pPlus :: Parser Value
pPlus = pSymbol Plus >> return plus

pMultiply :: Parser Value
pMultiply = pSymbol Multiply >> return multiply

pParentheses :: Parser Expresion
pParentheses = do
    pos <- getPosition
    pSymbol LeftParenthesis
    term <- pLambdaTerm
    pSymbol RightParenthesis
    loc <- pLoc pos
    return $ setInfo loc term

parseString :: String -> Either ParseError Expresion
parseString str = case lexer str of
      Right tokenStream -> parse pLine "" tokenStream
      Left e -> Left $ Lexer e
