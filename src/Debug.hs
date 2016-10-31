module Debug where

import BruijnTerm
import PrettyPrint

import Debug.Trace

traceShowId' :: Show a => String -> a -> a
traceShowId' str a = trace (str ++ show a) a

traceShowM' :: (Show a ,Monad m) => String -> a ->  m ()
traceShowM' str a= trace (str ++ show a) return ()

printBrujin :: BruijnTerm () -> String
printBrujin t = either (const (show t)) PrettyPrint.pShow $ bruijn2Lam t
