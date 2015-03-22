module Main
where
import Expresion 
import Parser
import BruijnTerm
import Eval
import System.Console.Haskeline
import Data.Either.Unwrap
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
readEvalPrint input = outputStrLn $ merge $ do
    ast <- mapLeft show $ parseString input 
    bruij <- mapLeft show $ lam2Bruijn ast
    return  $ pShow $ bruijn2Lam bruij

merge :: Either a a -> a
merge (Right a) = a
merge (Left a) = a
