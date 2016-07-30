module Main (main
    ) where

import Test.Tasty
import TestBruijnEnv
import TestTypeCheker
import TestBruijn
import TestArbitrary
import TestEval
import TestPrettyPrintLambda
import TestParser
import TestType
import TestLogic
import TestSearchTree

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ testSearchTree
    , testLogic
    , testArbitrary
    , testBruijnEnv
    , testTypeChecker
    , testType
    , testPrettyPrintLambda
    , testBruijn
    , testEval
    , testParser
    ]
