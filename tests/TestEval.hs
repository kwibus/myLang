module TestEval (testEval
)	where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Maybe

import Eval
import BruijnTerm
import Lambda
import qualified ExampleBruijn as B
import TestUtils
import ArbitraryQuickcheck ()

testEval :: TestTree
testEval = testGroup "eval"
  [ testCase "eval id(id(\\z.id z))=id(\\z.id z)" $
      eval (Appl B.id ( Appl B.id (Lambda "z" (Appl B.id (Var 0))))) @?=
         Just ( Appl B.id ( Lambda "z" (Appl B.id (Var 0))))
  , testCase "omega omega" $
      eval (Appl B.omega B.omega) @?= Just (Appl B.omega B.omega)
  , testCase "eval id id = Just id" $
       eval (Appl B.id B.id) @?= Just B.id
  , testCase "call by vallu termination" $
      eval (Lambda "z" (Appl B.id (Var 1 ))) @?= Nothing
  , testProperty "welformd presevation eval" $
      \ t -> let result = fmap welFormd $ eval t
            in isNothing result || fromJust result
  , testProperty "keep normalisation under eval" $
      \ t -> fmap (lam2Bruijn . bruijn2Lam) (eval t) == eval t
  ]
