module Test (
)	where

import Test.Tasty
import Test.Tasty.HUnit
import Lambda

main :: IO()
main = defaultMain $
    testGroup "test" 
        [
        testGroup "Lambda"
            [ testCase "drop outer parentheses" $
                show (Appl (Var "a") (Var "b")) @?= "ab"
            , testCase "left associative" $
                show (Appl (Appl (Var "a") (Var "b")) (Var "c")) @?= "abc"
            , testCase "not right associative" $
                show (Appl (Var "a") (Appl (Var "b") (Var "c"))) @?= "a(bc)"
            , testCase " body of an abstraction extends as far right as possible" $
                show ( Lambda "x"(Appl (Var "m") (Var "n"))) @?= "\\x.mn"
            , testCase "apply lambda  " $
                show (Appl ( Lambda "x"(Var "m")) (Var "n")) @?= "(\\x.m)n"
            ] 
        ]
