module Parser ( parseString
)where

import Control.Monad.Identity(Identity)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

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
    return $ foldl1 Appl terms

pVallue::  Parser (LamTerm Vallue Name) 
pVallue = fmap Val $choice [fmap MyDouble double]

pLambdaTerm'' :: Parser (LamTerm Vallue Name)
pLambdaTerm'' = choice [pLambda, pVar, pParentheses,pVallue]

pLambdaTerm' :: Parser (LamTerm Vallue Name)
pLambdaTerm' = choice [operator,pLambda, pVar, pParentheses,pVallue]

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

tabel ::[[ Operator String () Identity (LamTerm Vallue Name)]]
tabel = [[oMulti ],[oPlus]]

binOperator :: Char -> Vallue -> Assoc-> Operator String () Identity (LamTerm Vallue Name)
binOperator s v a= Infix ( do
    symbol s 
    return (\t1 t2-> Appl(Appl (Val v ) t1 )t2)) a

oMulti :: Operator String () Identity (LamTerm Vallue Name)
oMulti =  binOperator '*' multiply AssocLeft

oPlus:: Operator String () Identity (LamTerm Vallue Name)
oPlus =  binOperator '+' plus AssocLeft

operator :: Parser (LamTerm Vallue Name)
operator =buildExpressionParser tabel  pLambdaTerm''

pParentheses :: Parser (LamTerm Vallue Name)
pParentheses = do
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    return term

parseString :: String -> Either ParseError (LamTerm Vallue Name)
parseString  = parse pLine ""
