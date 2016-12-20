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
import TestSearchTree
import TestTopologicalSort
import TestModificationTags

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ testSearchTree
    , testLogic
    , testArbitrary
    , testModificationTags
    , testTypeChecker
    , testType
    , testPrettyPrintLambda
    , testBruijn
    , testTopologicalSort
    , testEval
    , testParser
    ]
