module Main (main
    ) where

import Test.Tasty

import TestBruijn
import TestArbitrary
import TestEval
import TestLambda
import TestParser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ testLambda
    , testBruijn
    , testBruijn
    , testEval
    , testParser
    , testArbitrary
    ]
