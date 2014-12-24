module TestEval (testEval
)	where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Maybe

import Eval
import BruijnTerm
import qualified ExampleBruijn as B
import TestUtils
import ArbitraryQuickcheck ()

testEval :: TestTree
testEval = testGroup "eval"
  [ testCase "eval id(id(\\z.id z))=id(\\z.id z)" $
      eval (BAppl B.id ( BAppl B.id (BLambda "z" (BAppl B.id (Bound 0))))) @?=
         Just ( BAppl B.id ( BLambda "z" (BAppl B.id (Bound 0))))
  , testCase "omega omega" $
      eval (BAppl B.omega B.omega) @?= Just (BAppl B.omega B.omega)
  , testCase "eval id id = Just id" $
       eval (BAppl B.id B.id) @?= Just B.id
  , testCase "call by vallu termination" $
      eval (BLambda "z" (BAppl B.id (Bound 1 ))) @?= Nothing
  , testProperty "welformd presevation eval" $
      \ t -> let result = fmap welFormd $ eval t
            in isNothing result || fromJust result
  , testProperty "keep normalisation under eval" $
      \ t -> fmap (lam2Bruijn . bruijn2Lam) (eval t) == eval t
  ]
