module TestLambda (testLambda) where

import Test.Tasty
import Test.Tasty.HUnit

import PrettyPrint
import Lambda
import Name
import TestSetParseShow

-- Todo consider rename TestExpresion  or TestPrintExpresion

testLambda :: TestTree
testLambda = testGroup "pShow" [testPShowBasic, testPShowMath, testPShowAdvanced]

testPShowMath :: TestTree
testPShowMath = testGroup "Math" $ map testPShowExample math

testPShowBasic :: TestTree
testPShowBasic = testGroup "Basix" $ map testPShowExample basic

testPShowAdvanced :: TestTree
testPShowAdvanced = testGroup "Advanced" $ map testPShowExample advanced

testPShowExample :: (String, LamTerm () Name) -> TestTree
testPShowExample (expected, inputTerm) = testCase expected $ pShow inputTerm @?= expected
