module Main where

import System.Console.Haskeline
import Data.Bifunctor
import Control.Monad.Except
import Text.PrettyPrint.ANSI.Leijen as ANSI

import Info
import Parser
import TypeCheck
import BruijnTerm
import Eval
import qualified Type
import TypeError

main :: IO ()
main = runInputT defaultSettings loop
 where
    loop :: InputT IO ()
    loop = do
        input <- getInputLine "% "
        case input of
            Nothing -> return ()
            Just "quit" -> return ()
            Just expression -> readEvalPrint expression >> loop

readEvalPrint :: String -> InputT IO ()
readEvalPrint input = outPutDoc $ merge $ do
    ast <- first (text . show) $ parseString input
    bruijn <- first (text . show) $ lam2Bruijn ast
    t <-  first(showErrors input) $ solver bruijn
    let result = fullEval $ removeInfo bruijn
    return $ text ( Type.pShow t) ANSI.<$> text ( BruijnTerm.pShow result)

merge :: Either a a -> a
merge (Right a) = a
merge (Left a) = a

-- TODO this will give no color on Windows
-- https://hackage.haskell.org/package/ansi-wl-pprint-0.6.7.3/docs/Text-PrettyPrint-ANSI-Leijen.html
outPutDoc :: Doc -> InputT IO ()
outPutDoc = outputStrLn . show
