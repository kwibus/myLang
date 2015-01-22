module Main
where
import Expresion 
import Parser
import BruijnTerm
import Eval
import System.Console.Haskeline

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
readEvalPrint input = case parseString input of
    Right ast -> outputStrLn $ pShow $ bruijn2Lam $ fullEval $ lam2Bruijn ast
    Left e -> outputStrLn $ show e

