module TestType (testType ) where

import Test.Tasty
import Test.Tasty.HUnit

import Type
import MakeType

testType :: TestTree
testType = testGroup "testType"
  [ testCase "print type Double" $
      pShow (TVal TDouble) @?= "Double"

  , testCase "Double -> Double -> Double" $
      pShow (TAppl (TVal TDouble) (TAppl (TVal TDouble) (TVal TDouble)))
      @?= "Double -> Double -> Double"

  , testCase "(Double -> Double) -> Double" $
      pShow (TAppl (TAppl (TVal TDouble) (TVal TDouble)) (TVal TDouble))
      @?= "(Double -> Double) -> Double"

  , testCase "a -> b" $
      pShow (TAppl (tVar 1) (tVar 2))
      @?= "a -> b"
  ]
