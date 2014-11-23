module Main (main
)	where
import Prelude hiding (id)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Lambda
import ArbitraryQuickcheck()

main :: IO()
main = defaultMain tests

tests :: TestTree
tests= testGroup "test" [testLambda,testBruijn , testEval ]

testLambda :: TestTree
testLambda = testGroup "Lambda"
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
testBruijn :: TestTree
testBruijn = testGroup "bruijn index"
            [ testCase "test S combinator from lambda  S=\\x.\\y.x"$
                lam2Bruijn lambdaS  @?= bruijnS
            , testCase "test S combinator from bruijn S=\\\\1"$
                bruijn2Lam bruijnS@?= lambdaS
            ,testProperty "inverse test" $
                \t  -> bruijn2Lam (lam2Bruijn t) == t
            ,testProperty "keep normalisation under eval"$
                \t -> fmap (lam2Bruijn . bruijn2Lam) (eval t) == eval t
            ]

testEval :: TestTree
testEval =  testGroup "eval"
            [ testCase "eval id id = Just id"$
                 eval (BAppl id id ) @?= Just id
            , testCase "call by vallu termination" $
                eval (BLambda "z" (BAppl id  (Bound 1 )))@?= Nothing
            ]

id :: BruijnTerm
id = BLambda "a" (Bound 0)

bruijnS :: BruijnTerm
bruijnS = BLambda "x" (BLambda "y" (Bound 1))

lambdaS ::LamTerm  
lambdaS = Lambda "x" (Lambda "y" (Var "x"))
