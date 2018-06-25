module TestType (testType ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import ArbitraryType ()
import Properties

import Type
import MakeType

testType :: TestTree
testType = testGroup "Type"
  [ testPShow
  , testNormalise
  ]

testNormalise :: TestTree
testNormalise = testGroup "normalise"
    [testProperty "welformd normalise" $ welFormdType . normalise ]

testPShow:: TestTree
testPShow= testGroup "pShow"
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
