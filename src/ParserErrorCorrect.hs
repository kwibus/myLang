module ParserErrorCorrect where

import Text.Parsec

errorExtract :: Monad m => ParsecT s u m a -> ParsecT s u m (Either ParseError a)
errorExtract parser =  mkPT (\state -> do
       consumed  <- runParsecT parser state
       return $ fmap (fmap (getError state)) consumed)
 where
    getError ::State s u ->  Reply s u a -> Reply s u(Either ParseError a)
    getError state (Error e) = Ok (Left e) state e
    getError _ (Ok a s e) = Ok (Right a) s e

errorRecover :: Monad m => ParsecT s u m a  -> (ParseError -> a) -> ParsecT s u m a
errorRecover parser f = recover <$> errorExtract parser
  where
    recover (Left e) =  f e
    recover (Right a) = a
