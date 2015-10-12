module Main (main ) where

import Test.QuickCheck

import ArbitraryLambda (genTyped)

import PrettyPrint
import Lambda
import Name

main :: IO ()
main = do
    list <- mapM f [0, 2 .. 100]
    mapM_ (\ l -> putStrLn (pShow l ++ "\n") ) list
        where f i = generate $ resize i (genTyped :: Gen (LamTerm () Name ))
