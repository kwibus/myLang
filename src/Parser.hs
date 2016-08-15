module Parser (
  parseString,
  ParseError (..)
) where


-- import Data.Either.Unwrap
import Data.Bifunctor
import Control.Monad.Identity
import Control.Monad.Trans.Class
import Text.Parsec.Pos
import qualified Text.Parsec.Prim as PS
import Text.Parsec.Combinator
import qualified Text.Parsec.Error as PS

import Lam1
import InfixFix
import Value
import Lambda
import Lexer
import Operator
import Name

-- TODO Remove p

type Parser a = PS.ParsecT [TokenPos] () (Either InfixError) a
data ParseError = Infix InfixError
                | Parsec PS.ParseError
                | Lexer PS.ParseError
                deriving (Show, Eq)

parseString :: String -> Either ParseError Lam1
parseString str = case lexer str of
      Right tokenStream -> parse pLine "" tokenStream
      Left e -> Left $ Lexer e

parse :: Parser a -> String -> [TokenPos] -> Either ParseError a
parse parser file sting = case PS.runParserT parser () file sting of
    Right a -> first Parsec a
    Left e -> Left $ Infix e

pSatisfy :: (Token -> Bool) -> Parser Token
pSatisfy f = getToken <$> PS.tokenPrim showToken nextPos testToken
   where
     showToken = show . getToken
     testToken x = if f (getToken x) then Just x else Nothing
     nextPos _ x _ = getposition x

pSymbol :: ReservedSymbol -> Parser ()
pSymbol s = void $ pSatisfy (== ReservedS s)

pKeyWord :: ReservedWord -> Parser ()
pKeyWord w = void $ pSatisfy (== ReservedW w)

pIdentifier :: Parser Name
pIdentifier = do
  Identifier str <- pSatisfy (\ x -> case x of
    Identifier _ -> True
    _ -> False)
  return $ Name str

pBool :: Parser Bool
pBool = do
  CapIdentifier c <- pSatisfy (\ x -> case x of
        CapIdentifier _ -> True
        _ -> False)
  case c of
    "True" -> return True
    "False" -> return False
    _ -> PS.parserZero

pDouble :: Parser Double
pDouble = do
  Number n <- pSatisfy (\ x -> case x of
    Number _ -> True
    _ -> False)
  return n

pLambda :: Parser Lam1
pLambda = do
    pSymbol BackSlash
    pat <- PS.many pPattern -- TODO 1) error message no pattern
                            --      2) give warning Shadowin variable names (\a a b.t)
    pSymbol Dot
    term <- pLambdaTerm
    return $ foldr Lambda term pat

pPattern :: Parser Pattern
pPattern = Pattern <$> PS.getPosition <*> pIdentifier

pApplication :: Parser Lam1
pApplication = do
    terms <- PS.many pLambdaTerm'
    case fixInfix terms of
        Left erro -> lift $ Left erro
        Right exps -> return exps

pLiteral :: Parser Lam1
pLiteral = do
    pos <- PS.getPosition
    v <- choice [ fmap (Prim . MyDouble) pDouble
                , fmap (Prim . MyBool ) pBool]
    return $ Lit pos v

pLambdaTerm' :: Parser (Lam1, Bool)
pLambdaTerm' = choice parsers
    where parsers = pOperator : fmap (fmap (\ p -> (p, False))) [pLet, pLambda, pVar, pParentheses, pLiteral]

-- TODO rename
pLambdaTerm :: Parser Lam1
pLambdaTerm = pApplication

pVar :: Parser Lam1
pVar = Var <$> PS.getPosition <*> pIdentifier

pLet :: Parser Lam1
pLet = do
  pos <- PS.getPosition
  defs <- between
           (pKeyWord Lexer.Let)
           (pKeyWord In)
           (sepEndBy1 pDefinition (pSymbol Semicolon ))
  term <- pLambdaTerm
  return $ Lambda.Let pos defs term

pDefinition :: Parser (Def Pattern SourcePos Name)
pDefinition = do
  pos <- PS.getPosition
  str <- pIdentifier
  pSymbol Equal
  term <- pLambdaTerm
  return $ Def (Pattern pos str) term

pLine :: Parser Lam1
pLine = do
    term <- pLambdaTerm
    eof
    return term

pOperator :: Parser (Lam1, Bool)
pOperator = do
    pos <- PS.getPosition
    o <- choice [pPlus, pMultiply ]
    return (Lit pos o, True)

pPlus :: Parser Value
pPlus = pSymbol Plus >> return plus

pMultiply :: Parser Value
pMultiply = pSymbol Multiply >> return multiply

pParentheses :: Parser Lam1
pParentheses = do
    pSymbol LeftParenthesis
    term <- pLambdaTerm
    pSymbol RightParenthesis
    return term
