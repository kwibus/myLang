module Parser ( parseString
)where

import Text.Parsec
import Text.Parsec.String

import Lambda
import Lexer 


pLambda :: Parser LamTerm
pLambda = do
    symbol '\\'
    name <-identifier
    symbol '.'
    term <- pLambdaTerm
    return $ Lambda name term

pApplication :: Parser LamTerm 
pApplication = do
    terms <- many pLambdaTerm'
    return $ foldl1 Appl terms

pLambdaTerm' :: Parser LamTerm
pLambdaTerm' = choice [pLambda , pVar, pParentheses]

pLambdaTerm :: Parser LamTerm
pLambdaTerm = choice [pLambda, pApplication, pVar, pParentheses]

pVar :: Parser LamTerm
pVar = fmap (Var . VarVar) identifier

pLine :: Parser LamTerm
pLine = do
    term <- pLambdaTerm
    eof
    return term


pParentheses :: Parser LamTerm
pParentheses = do
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    return term

parseString :: String -> Either ParseError LamTerm
parseString  = parse pLine ""
