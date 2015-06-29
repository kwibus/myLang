module TestBruijn (testBruijn) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Names
import qualified ExampleBruijn as B
import qualified ExampleLambda as L
import BruijnTerm
import ArbitraryQuickcheck ()
import MakeTerm
import Lambda
import Enviroment

testBruijn :: TestTree
testBruijn = testGroup "bruijn index"
  [ testCase "bruijn2Lam id " $
      bruijn2Lam B.id @?= return (L.id "a")
  , testCase "lam2Bruijn id " $
      lam2Bruijn (L.id "a") @?= return B.id
  , testCase "test S combinator from lambda  S=\\x.\\y.x" $
      lam2Bruijn L.s @?= return B.s
  , testCase "test S combinator from bruijn S=\\\\1" $
      bruijn2Lam B.s @?= return L.s
  , testCase "lam2Bruijn a "$
      lam2Bruijn (var "a")  @?= Left (UndefinedVar () (Name "a"))
  , testCase "bruijn2Lam 0 "$
      bruijn2Lam (bvar 0)  @?= Left (UndefinedVar () (Bound 0))
    , testCase "bruijn2Lam \\a\\a1 "$
      bruijn2Lam (lambda "a" (lambda "a"(bvar 1)))  @?= Left (RefShadow () (Bound 1) (Name "a"))
  , testProperty "inverse test" $
      \ t -> fmap bruijn2Lam (lam2Bruijn t) == return ( return (t :: LamTerm () Name))
  ]
