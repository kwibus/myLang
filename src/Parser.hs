module Parser (
  parseString,
  ParseError (..)
) where

import Data.Bifunctor
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Text.Parsec.Pos
import qualified Text.Parsec.Prim as PS
import Text.Parsec.Combinator
import qualified Text.Parsec.Error as PS

import InfixFix
import Value
import Lambda
import Lexer
import Operator
import SourcePostion
import Name

-- TODO Remove p

type Parser a = PS.ParsecT [TokenPos] () (Either InfixError) a
data ParseError = Infix InfixError
                | Parsec PS.ParseError
                | Lexer PS.ParseError
                deriving (Show, Eq)

parseString :: String -> Either ParseError Expresion
parseString str = case lexer str of
      Right tokenStream -> parse pLine "" tokenStream
      Left e -> Left $ Lexer e

parse :: Parser a -> String -> [TokenPos] -> Either ParseError a
parse parser file sting = case PS.runParserT parser () file sting of
    Right a -> first Parsec a
    Left e -> Left $ Infix e

pToken :: (Token -> Maybe a) -> Parser a
pToken f = PS.tokenPrim showToken nextPos testToken
   where
     showToken = show . getToken
     testToken x = f (getToken x)
     nextPos _ x _ = getposition x

pSatisfy :: (Token -> Bool) -> Parser ()
pSatisfy f = pToken ( guard . f )

pSymbol :: ReservedSymbol -> Parser ()
pSymbol s = pSatisfy (== ReservedS s)

pKeyWord :: ReservedWord -> Parser ()
pKeyWord w = pSatisfy (== ReservedW w)

pIdentifier :: Parser String
pIdentifier = pToken (\ x -> case x of
    Identifier str -> Just str
    _ -> Nothing )

pBool :: Parser Bool
pBool = pToken (\ x -> case x of
        CapIdentifier "True" -> Just True
        CapIdentifier "False" -> Just False
        _ -> Nothing)

pDouble :: Parser Double
pDouble = pToken (\ x -> case x of
    Number n -> Just n
    _ -> Nothing )

pLambda :: Parser Expresion
pLambda = do
    pos <- PS.getPosition
    pSymbol BackSlash
    ns <- PS.many pIdentifier -- TODO 1) fix location 2) give warning Shadowin variable names (\a a b.t)
    pSymbol Dot
    term <- pLambdaTerm
    return $ foldr (Lambda pos) term (Name <$> ns)

pApplication :: Parser Expresion
pApplication = do
    terms <- PS.many pLambdaTerm'
    case fixInfix terms of
        Left erro -> lift $ Left erro
        Right exps -> return exps

pValue :: Parser Expresion
pValue = do
    pos <- PS.getPosition
    v <- choice [ fmap (Prim . MyDouble) pDouble
                , fmap (Prim . MyBool ) pBool]
    return $ Val pos v

pLambdaTerm' :: Parser (Expresion, Bool)
pLambdaTerm' = choice parsers
    where parsers = pOperator : fmap (fmap (\ p -> (p, False))) [pLet, pLambda, pVar, pParentheses, pValue]

-- TODO renoame Expresion
pLambdaTerm :: Parser Expresion
pLambdaTerm = pApplication

pVar :: Parser Expresion
pVar = do
    pos <- PS.getPosition
    n <- pIdentifier
    return $ Var pos (Name n)

pLet :: Parser Expresion
pLet = do
  pos <- PS.getPosition
  defs <- between
           (pKeyWord Lexer.Let)
           (pKeyWord In)
           (sepEndBy1 pDefinition (pSymbol Semicolon ))
  term <- pLambdaTerm
  return $ Lambda.Let pos defs term

pDefinition :: Parser (Def SourcePos Expresion )
pDefinition = do
  pos <- PS.getPosition
  str <- pIdentifier
  args <- PS.many $ withPos $ Name <$> pIdentifier
  pSymbol Equal
  term <- pLambdaTerm
  return $ Def pos (Name str) $ foldr (uncurry Lambda ) term args

withPos :: Parser a -> Parser (SourcePos, a)
withPos pars = do
    pos <- PS.getPosition
    a <- pars
    return (pos, a)

pLine :: Parser Expresion
pLine = do
    term <- pLambdaTerm
    eof
    return term


pOperator :: Parser (Expresion, Bool)
pOperator = do
    pos <- PS.getPosition
    o <- choice [pPlus, pMultiply ]
    return (Val pos o, True)

pPlus :: Parser Value
pPlus = pSymbol Plus >> return plus

pMultiply :: Parser Value
pMultiply = pSymbol Multiply >> return multiply

pParentheses :: Parser Expresion
pParentheses = do
    pSymbol LeftParenthesis
    term <- pLambdaTerm
    pSymbol RightParenthesis
    return term
