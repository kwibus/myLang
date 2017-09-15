module TestBruijn (testBruijn) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified ExampleBruijn as B
import qualified ExampleLambda as L
import MakeTerm
import ArbitraryLambda

import BruijnTerm
import Name

testBruijn :: TestTree
testBruijn = testGroup "bruijn index"
  [ testCase "bruijn2Lam id " $
      bruijn2Lam B.id @?= return (L.id "a")

  , testCase "lam2Bruijn id " $
      lam2Bruijn (L.id "a") @?= return B.id

  , testCase "test k combinator from lambda  k=\\x.\\y.x" $
      lam2Bruijn L.k @?= return B.k

  , testCase "test k combinator from bruijn k=\\\\1" $
      bruijn2Lam B.k @?= return L.k

  , testCase "lam2Bruijn \\a .\\ a.a " $
      lam2Bruijn (lambda "a" (lambda "a" (var "a")))
      @?= return ( lambda "a" (lambda "a" (bvar 0)))

  , testCase "lam2Bruijn \\a.let a = b; b=1 in a " $
      lam2Bruijn (lambda "a" (mkLet [("a", var "b"), ("b", double 1)] (var "a")))
        @?= return (lambda "a" (mkLet [("a", bvar 0), ("b", double 1.0)] (bvar 1)))

  , testCase "bruijn2Lam \\a.let a = 0; b=1 in 1 " $
      bruijn2Lam (lambda "a" (mkLet [("a", bvar 0), ("b", double 1.0)] (bvar 1)))
        @?= return (lambda "a" (mkLet [("a0", var "b"), ("b", double 1)] (var "a0")))

  , testCase "bruijn2Lam let a = 1.0; a = 2.0; in a" $
       bruijn2Lam (mkLet [("a",double 1),("a",double 2)] $ bvar 1 ) @?=
       return (mkLet [("a",double 1),("a0",double 2)] $ var "a" )

  , testCase "bruijn2Lam let a = 1.0; a0 = 2.0; in a0" $
       bruijn2Lam (mkLet [("a",double 1),("a0",double 2)] $ bvar 0 ) @?=
       return (mkLet [("a",double 1),("a0",double 2)] $ var "a0")

  , testCase "lam2Bruijn a " $
      lam2Bruijn (var "a") @?= Left (UndefinedVar () (Name "a"))

  , testCase "bruijn2Lam 0 " $
      bruijn2Lam (bvar 0) @?= Left (UndefinedVar () (Bound 0))

  , testProperty "inverse test" $
      forAllUnTypedBruijn $ \ t -> fmap lam2Bruijn (bruijn2Lam t) == return ( return t)
  ]
