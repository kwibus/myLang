module TestLambda (testLambda) where

import Test.Tasty
import Test.Tasty.HUnit

import Lambda

testLambda :: TestTree
testLambda = testGroup "Lambda"
  [ testCase "drop outer parentheses" $
      pShow (Appl (Var "a") (Var "b")) @?= "a b"
  , testCase "left associative" $
      pShow (Appl (Appl (Var "a") (Var "b")) (Var "c")) @?= "a b c"
  , testCase "not right associative" $
      pShow (Appl (Var "a") (Appl (Var "b") (Var "c"))) @?= "a(b c)"
  , testCase " body of an abstraction extends as far right as possible" $
      pShow ( Lambda "x" (Appl (Var "m") (Var "n"))) @?= "\\x.m n"
  , testCase "apply lambda  " $
      pShow (Appl ( Lambda "x" (Var "m")) (Var "n")) @?= "(\\x.m)n"
  ]
