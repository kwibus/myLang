module TestEval (testEval
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Either
import Data.Maybe

import Eval
import TypeCheck
import BruijnTerm
import qualified ExampleBruijn as B
import TestUtils
import ArbitraryQuickcheck ()
import Enviroment
import MakeTerm
import Type (bound2Free)

testEval :: TestTree
testEval = testGroup "eval"
  [ testCase "eval id(id(\\z.id z))=id(\\z.id z)" $
      eval (appl B.id ( appl B.id (lambda "z" (appl B.id (bvar 0))))) @?=
         Just ( appl B.id ( lambda "z" (appl B.id (bvar 0))))
  , testCase "omega omega" $
      eval (appl B.omega B.omega) @?= Just (appl B.omega B.omega)
  , testCase "eval id id = Just id" $
       eval (appl B.id B.id) @?= Just B.id
  , testCase "call by vallu termination" $
      eval (lambda "z" (appl B.id (bvar 1))) @?= Nothing
  , testProperty "welformd presevation eval" $
      \ t -> let result = fmap welFormd $ eval (t :: BruijnTerm ())
            in isNothing result || fromJust result
  , testProperty "keep normalisation under eval" $
      \ t -> fmap (lam2Bruijn . bruijn2Lam ) (eval t) == fmap return (eval (t :: BruijnTerm ()))
  , testProperty "keep type under eval" $
      \ e -> let result = eval (e :: BruijnTerm ())
            in isJust result ==> case eval e of
               Nothing -> True
               Just expr2 -> isRight $ do
                    t1 <- solver expr2
                    t2 <- solver e
                    return $ unifys ((),bound2Free t1) ((),bound2Free t2) fEmtyEnv
  ]
