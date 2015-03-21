module TestLambda (testLambda) where

import Test.Tasty
import Test.Tasty.HUnit

import Expresion
import Opperator
import MakeTerm

-- Todo consider rename TestExpresion  or TestPrintExpresion

testLambda :: TestTree
testLambda = testGroup "Lambda"
  [ testCase "drop outer parentheses" $
      pShow (appl (var "a") (var "b")) @?= "a b"
  , testCase "left associative" $
      pShow (appl (appl (var "a") (var "b")) (var "c")) @?= "a b c"
  , testCase "not right associative" $
      pShow (appl (var "a") (appl (var "b") (var "c"))) @?= "a(b c)"
  , testCase " body of an abstraction extends as far right as possible" $
      pShow ( lambda "x" (appl (var "m") (var "n"))) @?= "\\x.m n"
  , testCase "apply lambda  " $
      pShow (appl ( lambda "x" (var "m")) (var "n")) @?= "(\\x.m)n"
  , testCase "+" $
     pShow (val plus ) @?= "+"
  , testCase "1 + " $
     pShow (appl (val plus) (double 1.0)) @?= "1.0 +"
  , testCase "\\a.1.0+ " $
     pShow (lambda "a" (appl (val plus) (double 1.0))) @?= "\\a.1.0 +"
  , testCase "\\a.a(+)(*) " $
        pShow (lambda "a" (appl (appl (var "a") (val plus)) (val multiply)))
        @?= "\\a.a(+)(*)"
  ]
