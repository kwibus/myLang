module TestType (testType ) where

import Test.Tasty
import Test.Tasty.HUnit

import FreeEnvironment
import Type

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
      pShow (TAppl (TVar (Free 1)) (TVar (Free 2)))
      @?= "a -> b"
  ]
