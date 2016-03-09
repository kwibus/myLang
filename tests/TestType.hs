module TestType (testType ) where

import Test.Tasty
import Test.Tasty.HUnit

import Type
import MakeType

testType :: TestTree
testType = testGroup "testType"
  [ testCase "print type Double" $
      pShow tDouble @?= "Double"

  , testCase "Double -> Double -> Double" $
      pShow (tDouble ~> tDouble ~>tDouble)
      @?= "Double -> Double -> Double"

  , testCase "(Double -> Double) -> Double" $
      pShow ((tDouble ~> tDouble) ~> tDouble)
      @?= "(Double -> Double) -> Double"

  , testCase "a -> b" $
      pShow (tVar 1 ~> tVar 2)
      @?= "a -> b"
  ]
