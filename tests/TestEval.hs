module TestEval (testEval
)where

import Data.Coerce

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Either
import Data.Maybe

import Eval
import Vallue
import TypeCheck
import BruijnTerm
import Lambda
import qualified ExampleBruijn as B
import TestUtils
import ArbitraryQuickcheck ()
import Enviroment

testEval :: TestTree
testEval = testGroup "eval"
  [ testCase "eval id(id(\\z.id z))=id(\\z.id z)" $
      eval (Appl B.id ( Appl B.id (Lambda "z" (Appl B.id (Var(Bound 0)))))) @?=
         Just ( Appl B.id ( Lambda "z" (Appl B.id (Var(Bound 0)))))
  , testCase "omega omega" $
      eval (Appl B.omega B.omega) @?= Just (Appl B.omega B.omega)
  , testCase "eval id id = Just id" $
       eval (Appl B.id B.id) @?= Just B.id
  , testCase "call by vallu termination" $
      eval (Lambda "z" (Appl B.id (Var( Bound 1) ))) @?= Nothing
  , testCase "eval long expresion" $
    eval (Appl
            (Lambda "n" (Appl
                (Lambda "d" (Lambda "y" (Var (Bound 2))))
                (Lambda "a" (Appl
                    (Appl
                        (Var (Bound 1))
                        (Appl
                              (Lambda "c" (Val (MyDouble ( 12.211578327506952))))
                              (Lambda "g" (Val (MyDouble ( 19.667726379140998))))))
                    (Var (Bound 0))))))
            (Lambda "i" (Lambda "o" (Lambda "k" (Var (Bound 2))))))
    @?=
    return (Appl
        (Lambda "d" (Lambda "y" (Lambda "i" (Lambda "o" (Lambda "k" (Var (Bound 2)))))))
        (Lambda "a" (Appl
            (Appl
                (Lambda "i" (Lambda "o" (Lambda "k" (Var (Bound 2)))))
                (Appl
                    (Lambda "c" (Val (MyDouble ( 12.211578327506952))))
                    (Lambda "g" (Val (MyDouble ( 19.667726379140998))))))
            (Var (Bound 0)))))
  , testProperty "welformd presevation eval" $
      \ t -> let result = fmap welFormd $ eval t
            in  isNothing result || fromJust result
  , testProperty "keep normalisation under eval" $
      \ t -> fmap (lam2Bruijn . bruijn2Lam ) (eval t) == eval t
  , testProperty "keep type under eval" $
      \ e -> let result = eval e 
            in isJust result ==> case eval e of
               Nothing -> True
               Just expr2 -> isRight $ do 
                    t1 <- solver expr2 
                    t2 <- solver e
                    return $ unifys (coerce t1) (coerce t2) fEmtyEnv
  ]
