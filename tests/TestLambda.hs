module TestLambda (testLambda) where

import Test.Tasty
import Test.Tasty.HUnit

import Lambda

testLambda :: TestTree
testLambda = testGroup "Lambda"
  [ testCase "drop outer parentheses" $
      pShow (Appl (var "a") (var "b")) @?= "a b"
  , testCase "left associative" $
      pShow (Appl (Appl (var "a") (var "b")) (var "c")) @?= "a b c"
  , testCase "not right associative" $
      pShow (Appl (var "a") (Appl (var "b") (var "c"))) @?= "a(b c)"
  , testCase " body of an abstraction extends as far right as possible" $
      pShow ( Lambda "x" (Appl (var "m") (var "n"))) @?= "\\x.m n"
  , testCase "apply lambda  " $
      pShow (Appl ( Lambda "x" (var "m")) (var "n")) @?= "(\\x.m)n"
  ]
