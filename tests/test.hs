module Main (main
    ) where

import Test.Tasty

import TestTypeCheker
import TestBruijn
import TestArbitrary
import TestEval
import TestLambda
import TestParser
import TestType
-- import SmallCheck
-- import Type
-- import Key
-- import Logic
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ testTypeChecker
    , testType
    , testLambda
    , testBruijn
    , testBruijn
    , testEval
    , testParser
    , testArbitrary
    ]
