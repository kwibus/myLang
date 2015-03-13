module TestType ( testType)where

import Type
    
import Test.Tasty
import Test.Tasty.HUnit

testType :: TestTree
testType = testGroup "testType"
    [ testCase "print type Double" $
        tShow (TVal TDouble) @?= "Double"
    -- , testCase "print Type "
    ]

