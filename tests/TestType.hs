module TestType (testType ) where

import Test.Tasty
import Test.Tasty.HUnit

import Environment
import Type

testType :: TestTree
testType = testGroup "testType"
    [ testCase "print type Double" $
        tShow (TVal TDouble :: Type Bound) @?= "Double"
    -- , testCase "print Type "
    ]
