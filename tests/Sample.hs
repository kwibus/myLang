module Main (main ) where

import Test.QuickCheck

import ArbitraryLambda (genTyped, defaultConf)

import PrettyPrint
import Lambda

main :: IO ()
main = do
    list <- mapM f [0, 2 .. 100]
    mapM_ (\ l -> putStrLn (pShow l ++ "\n") ) list
        where f i = generate $ resize i (genTyped defaultConf :: Gen (LamTerm () () Name ))
