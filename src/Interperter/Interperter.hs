module Main
where

import System.Console.Haskeline
import Data.Either.Unwrap
import Text.PrettyPrint.ANSI.Leijen
import Control.Monad.IO.Class

import Parser
import TypeCheck
import BruijnTerm
import Eval
import Type
import TypeError

main :: IO ()
main = runInputT defaultSettings loop
 where
    loop :: InputT IO ()
    loop = do
        minput <- getInputLine "% "
        case minput of
            Nothing -> return ()
            Just "quit" -> return ()
            Just input -> readEvalPrint input >> loop


readEvalPrint :: String -> InputT IO ()
readEvalPrint input = outPutDoc $ merge $ do
    ast <- mapLeft (text . show)$ parseString input 
    bruij <- mapLeft (text . show) $ lam2Bruijn ast
    t <-  mapLeft (showError input) $ solver bruij
    return  $ text$  tShow t-- $ pShow $ bruijn2Lam bruij

merge :: Either a a -> a
merge (Right a) = a
merge (Left a) = a

outPutDoc :: Doc -> InputT IO ()
outPutDoc doc =liftIO $ putDoc $  doc <> line
