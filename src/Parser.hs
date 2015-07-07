module Parser ( parseString
) where

import Text.Parsec hiding (parse, ParseError)
import Control.Monad.Trans.Class

import ParserType
import InfixFix
import Expresion
import Vallue
import Lambda
import Lexer
import Opperator
import Info
import Names

pLambda :: Parser Expresion
pLambda = do
    pos <- getPosition
    symbol '\\'
    n <- identifier
    symbol '.'
    term <- pLambdaTerm
    loc <- getLoc pos
    return $ Lambda loc (Name n) term

pApplication :: Parser Expresion
pApplication = do
    terms <- many pLambdaTerm'
    case fixInfix terms of
        Left erro -> lift $ Left erro
        Right exps -> return $ foldl1 (\ e1 e2 -> Appl (mergLoc e1 e2) e1 e2 ) exps

mergLoc :: Expresion -> Expresion -> Loc
mergLoc e1 e2 = Loc { srcFile = srcFile start
                    , lineStart = lineStart start
                    , columnStart = columnStart start
                    , lineEnd = lineEnd end
                    , columnEnd = columnEnd end}
    where start = getposition e1
          end = getposition e2

pVallue :: Parser Expresion
pVallue = do
    pos <- getPosition
    v <- choice [fmap MyDouble double]
    loc <- getLoc pos
    return $ Val loc v

pLambdaTerm' :: Parser (Expresion, Bool)
pLambdaTerm' = choice parsers
    where parsers = operator : fmap (fmap (\ p -> (p, False))) [pLambda, pVar, pParentheses, pVallue]

pLambdaTerm :: Parser Expresion
pLambdaTerm = pApplication

pVar :: Parser Expresion
pVar = do
    pos <- getPosition
    n <- identifier
    loc <- getLoc pos
    return $ Var loc (Name n)

pLine :: Parser Expresion
pLine = do
    spaces
    term <- pLambdaTerm
    eof
    return term

operator :: Parser (Expresion, Bool)
operator = do
    pos <- getPosition
    o <- choice [pPlus, pMultiply ]
    loc <- getLoc pos
    return (Val loc o, True)

getLoc :: SourcePos -> Parser Loc
getLoc start = do
    end <- getPosition
    return Loc
        { srcFile = sourceName start
        , lineStart = sourceLine start
        , columnStart = sourceColumn start
        , lineEnd = sourceLine end
        , columnEnd = sourceColumn end}

pPlus :: Parser Vallue
pPlus = symbol '+' >> return plus

pMultiply :: Parser Vallue
pMultiply = symbol '*' >> return multiply

pParentheses :: Parser Expresion
pParentheses = do
    pos <- getPosition
    symbol '('
    term <- pLambdaTerm
    symbol ')'
    loc <- getLoc pos
    return $ setInfo loc term

parseString :: String -> Either ParseError Expresion
parseString = parse pLine ""
