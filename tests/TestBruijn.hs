module TestBruijn (testBruijn) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Names
import qualified ExampleBruijn as B
import qualified ExampleLambda as L
import BruijnTerm
import ArbitraryQuickcheck ()

import Lambda

testBruijn :: TestTree
testBruijn = testGroup "bruijn index"
  [ testCase "bruijn2Lam id " $
      bruijn2Lam B.id @?= L.id
  , testCase "lam2Bruijn id " $
      lam2Bruijn L.id @?= return B.id
  , testCase "test S combinator from lambda  S=\\x.\\y.x" $
      lam2Bruijn L.s @?= return B.s
  , testCase "test S combinator from bruijn S=\\\\1" $
      bruijn2Lam B.s @?= L.s
  , testProperty "inverse test" $
      \ t -> fmap bruijn2Lam (lam2Bruijn t) == return (t :: LamTerm () Name)
  ]
