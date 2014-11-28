module Main (main
    ) where
import Prelude hiding (id)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck ()

import Lambda
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "test" [testLambda, testBruijn , testEval, testParser]

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
testBruijn :: TestTree
testBruijn = testGroup "bruijn index"
  [ testCase "test S combinator from lambda  S=\\x.\\y.x" $
      lam2Bruijn lambdaS @?= bruijnS
  , testCase "test S combinator from bruijn S=\\\\1" $
      bruijn2Lam bruijnS @?= lambdaS
  , testProperty "inverse test" $
      \ t -> bruijn2Lam (lam2Bruijn t) == t
  , testProperty "keep normalisation under eval" $
      \ t -> fmap (lam2Bruijn . bruijn2Lam) (eval t) == eval t
  ]

testEval :: TestTree
testEval = testGroup "eval"
  [ testCase "eval id id = Just id" $
       eval (BAppl id id ) @?= Just id
  , testCase "call by vallu termination" $
      eval (BLambda "z" (BAppl id (bvar 1 ))) @?= Nothing
  ]

id :: BruijnTerm
id = BLambda "a" (bvar 0)

bruijnS :: BruijnTerm
bruijnS = BLambda "x" (BLambda "y" (bvar 1))

lambdaS :: LamTerm
lambdaS = Lambda "x" (Lambda "y" (var "x"))

testParser :: TestTree
testParser = testGroup "parser"
  [
    testCase "id id id" $
     let ididid = "(\\a.a)(\\b.b)\\c.c"
     in (pShow (right (parseString ididid)) @?= ididid)
  , testProperty "parse pShow arbitrary " $
        \ t -> isRight (parseString (pShow (t :: LamTerm )))
  , testProperty "pShow parse = id " $
        \ t -> pShow (right (parseString (pShow (t :: LamTerm )))) == pShow t
  ]

isRight :: Show a => Either a b -> Bool
isRight (Right _) = True
isRight (Left a) = error $ show a


right :: Show a => Either a b -> b
right (Right b) = b
right (Left a) = error $ show a
