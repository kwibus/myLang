module Parser ( parseString
) where

import Text.Parsec hiding (parse, ParseError)
import Control.Monad.Trans.Class

import ParserType
import InfixFix
import Value
import Lambda
import Lexer
import Operator
import Info
import Name

pLambda :: Parser Expresion
pLambda = do
    pos <- getPosition
    symbol '\\'
    ns <- many identifier -- Todo 1) fix location 2) give warning Shadowin variable names (\a a b.t)
    symbol '.'
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
    v <- choice [fmap MyDouble double]
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
    n <- identifier
    loc <- pLoc pos
    return $ Var loc (Name n)

pLine :: Parser Expresion
pLine = do
    spaces
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
pPlus = symbol '+' >> return plus

pMultiply :: Parser Value
pMultiply = symbol '*' >> return multiply

pParentheses :: Parser Expresion
pParentheses = do
    pos <- getPosition
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    loc <- pLoc pos
    return $ setInfo loc term

parseString :: String -> Either ParseError Expresion
parseString = parse pLine ""
