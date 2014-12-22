module Parser ( parseString
)where

import Control.Monad.Identity(Identity)
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

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

pVallue::  Parser Variable
pVallue = fmap Val $ choice [fmap MyDouble double]

pLambdaTerm'' :: Parser LamTerm
pLambdaTerm'' = choice [pLambda, pVar, pParentheses]

pLambdaTerm' :: Parser LamTerm
pLambdaTerm' = choice [operator,pLambda, pVar, pParentheses]

pLambdaTerm :: Parser LamTerm
pLambdaTerm =  pApplication

pVar :: Parser LamTerm
pVar = fmap (Var .  VarVar) identifier <|> fmap Var  pVallue

pLine :: Parser LamTerm
pLine = do
    spaces
    term <- pLambdaTerm
    eof
    return term

tabel ::[[ Operator String () Identity LamTerm]]
tabel = [[oMulti ],[oPlus]]

oMulti :: Operator String () Identity LamTerm
oMulti = Infix ( do
    symbol '*'
    return (\t1 t2-> Appl(Appl (val plus) t1 )t2))AssocLeft

oPlus:: Operator String () Identity LamTerm
oPlus = Infix ( do
    symbol '+'
    return (\t1 t2-> Appl(Appl (val plus) t1 )t2))AssocLeft

operator :: Parser LamTerm
operator =buildExpressionParser tabel  pLambdaTerm''
-- term =  pParentheses <|> pVar

pParentheses :: Parser LamTerm
pParentheses = do
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    return term

parseString :: String -> Either ParseError LamTerm
parseString  = parse pLine ""
