module Main (main
    ) where
import Prelude hiding (id)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck ()
import TestArbitrary
import Data.Maybe

import Lambda
import Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
    [ testLambda
    , testBruijn
    , testBruijn
    , testEval
    , testParser
    , testArbitrary
    ]

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
  [ testCase "bruijn2Lam id " $
      bruijn2Lam bruijnId @?= lambdaId
  , testCase "lam2Bruijn id " $
      lam2Bruijn lambdaId @?= bruijnId
  , testCase "test S combinator from lambda  S=\\x.\\y.x" $
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
  [ testCase "omega omega" $
      eval (BAppl bruijOmega bruijOmega) @?= Just (BAppl bruijOmega bruijOmega)
  , testCase "eval id id = Just id" $
       eval (BAppl bruijnId bruijnId ) @?= Just bruijnId
  , testCase "call by vallu termination" $
      eval (BLambda "z" (BAppl bruijnId (bvar 1 ))) @?= Nothing
  , testProperty "welformd presevation eval" $
      \ t -> let result = fmap welFormd $ eval t
            in isNothing result || fromJust result
  ]

welFormd :: BruijnTerm -> Bool
welFormd t0 = go t0 0
    where go (BLambda _ t) dept = go t (dept + 1)
          go (BAppl t1 t2) dept = go t1 dept && go t2 dept
          go (BVar (Bound i )) dept = i <= dept
          go (BVar {}) _ = True

bruijOmega :: BruijnTerm
bruijOmega = BLambda "a" $ BAppl (bvar 0) (bvar 0)

bruijnId :: BruijnTerm
bruijnId = BLambda "a" $ bvar 0

lambdaId :: LamTerm
lambdaId = Lambda "a" $ var "a"

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
        \ t -> (right (parseString (pShow (t :: LamTerm )))) == t
  ]

isRight :: Show a => Either a b -> Bool
isRight (Right _) = True
isRight (Left a) = error $ show a


right :: Show a => Either a b -> b
right (Right b) = b
right (Left a) = error $ show a
