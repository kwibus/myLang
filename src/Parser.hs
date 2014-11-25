module Parser ( parseString
)where

import Text.Parsec
import Text.Parsec.String

import Lambda
import Lexer 


pLambda :: Parser (LamTerm a)
pLambda = do
    symbol '\\'
    name <-identifier
    symbol '.'
    term <- pLambdaTerm
    return $ Lambda name term

pApplication :: Parser (LamTerm a)
pApplication = do
    terms <- many pLambdaTerm'
    return $ foldl1 Appl terms

pLambdaTerm' :: Parser (LamTerm a)
pLambdaTerm' = choice [pLambda , pVar, pParentheses]

pLambdaTerm :: Parser (LamTerm a )
pLambdaTerm = choice [pLambda, pApplication, pVar, pParentheses]

pVar :: Parser (LamTerm a)
pVar = fmap (Var . VarVar) identifier

pLine :: Parser (LamTerm a)
pLine = do
    term <- pLambdaTerm
    eof
    return term


pParentheses :: Parser (LamTerm a)
pParentheses = do
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    return term

parseString :: String -> Either ParseError (LamTerm a)
parseString  = parse pLine ""
