module Parser ( parseString
)where

import Text.Parsec
import Text.Parsec.String

import InfixFix
import Vallue 
import Lambda 
import Lexer
import Opperator 

pLambda :: Parser (LamTerm Vallue Name)
pLambda = do
    symbol '\\'
    name <-identifier
    symbol '.'
    term <- pLambdaTerm
    return $ Lambda name term

pApplication :: Parser (LamTerm Vallue Name)
pApplication = do
    terms <- many pLambdaTerm'
    return $ foldl1 Appl $ fixInfix terms

pVallue::  Parser (LamTerm Vallue Name) 
pVallue = fmap Val $choice [fmap MyDouble double]

pLambdaTerm' :: Parser (LamTerm Vallue Name,Bool)
pLambdaTerm' = choice parsers 
    where parsers =  operator : fmap (fmap (\p-> (p,False)))  [pLambda , pVar, pParentheses,pVallue]

pLambdaTerm :: Parser (LamTerm Vallue Name)
pLambdaTerm =  pApplication

pVar :: Parser (LamTerm Vallue Name)
pVar = fmap Var  identifier 

pLine :: Parser (LamTerm Vallue Name)
pLine = do
    spaces
    term <- pLambdaTerm
    eof
    return term

operator :: Parser (LamTerm Vallue Name,Bool)
operator = fmap (\o->(Val o, True))$choice  [pPlus,pMultiply ]

pPlus:: Parser Vallue 
pPlus = symbol '+' >>  return plus

pMultiply :: Parser Vallue 
pMultiply = symbol '*' >>  return multiply

pParentheses :: Parser (LamTerm Vallue Name)
pParentheses = do
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    return term

parseString :: String -> Either ParseError (LamTerm Vallue Name)
parseString = parse pLine ""
