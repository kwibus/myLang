module Main (main
    ) where

import Test.Tasty

import TestTypeCheker
import TestBruijn
import TestArbitrary
import TestEval
import TestPrettyPrintLambda
import TestParser
import TestType
import TestLogic
-- import TestBackList
-- import TestBackListT
import TestSearchTree
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ testSearchTree
    -- , testBackList
    -- , testBackListT
    , testLogic
    , testArbitrary
    , testTypeChecker
    , testType
    , testPrettyPrintLambda
    , testBruijn
    , testEval
    , testParser
    ]
