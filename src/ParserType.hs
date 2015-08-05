module ParserType where

import Data.Either.Unwrap
import qualified Text.Parsec as PS
import InfixFix
import Control.Monad.Identity

type Parser a = PS.ParsecT String () (Either InfixError) a

data ParseError = Infix InfixError
                | Parsec PS.ParseError
                deriving (Show, Eq)

parsec2parser :: Monad m => PS.ParsecT s u Identity a -> PS.ParsecT s u m a
parsec2parser p = PS.mkPT $ \ s -> return $ repack $ runIdentity $ PS.runParsecT p s
 where repack x = case x of
            PS.Consumed a -> PS.Consumed $ return (runIdentity a)
            PS.Empty a -> PS.Empty $ return (runIdentity a)

parse :: Parser a -> String -> String -> Either ParseError a
parse parser file sting = case PS.runParserT parser () file sting of
    Right a -> mapLeft Parsec a
    Left e -> Left $ Infix e
