module Parser ( parseString
)where

import Text.Parsec
import Text.Parsec.String

import InfixFix
import Expresion
import Vallue
import Lambda
import Lexer
import Opperator
import Info

pLambda :: Parser Expresion
pLambda = do
    symbol '\\'
    i <- getPosition
    n <- identifier
    symbol '.'
    term <- pLambdaTerm
    return $ Lambda i n term

pApplication :: Parser Expresion
pApplication = do
    terms <- many pLambdaTerm'
    return $ foldl1 (\e1 e2 -> Appl (getposition e1) e1 e2 )$ fixInfix terms

pVallue::  Parser Expresion
pVallue = do i <- getPosition 
             v <- choice [fmap MyDouble double]
             return $ Val i v

pLambdaTerm' :: Parser (Expresion,Bool)
pLambdaTerm' = choice parsers
    where parsers =  operator : fmap (fmap (\p-> (p,False)))  [pLambda , pVar, pParentheses,pVallue]

pLambdaTerm :: Parser Expresion
pLambdaTerm =  pApplication

pVar :: Parser Expresion
pVar = do i <- getPosition
          n <- identifier
          return $ Var i n

pLine :: Parser Expresion
pLine = do
    spaces
    term <- pLambdaTerm
    eof
    return term

operator :: Parser (Expresion, Bool)
operator = do 
    i <- getPosition 
    o <- choice  [pPlus,pMultiply ]
    return $(Val i o, True)

pPlus:: Parser Vallue
pPlus = symbol '+' >>  return plus

pMultiply :: Parser Vallue
pMultiply = symbol '*' >>  return multiply

pParentheses :: Parser Expresion
pParentheses = do
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    return term

parseString :: String -> Either ParseError Expresion
parseString = parse pLine ""
