module Main (main ) where

import ArbitraryQuickcheck (genTyped)
import Test.QuickCheck
import Expresion
import Lambda
import Names

main :: IO ()
main = do
    list <- mapM f [0, 2 .. 100]
    mapM_ (print . pShow) list
        where f i = generate $ resize i (genTyped :: Gen (LamTerm () Name ))
