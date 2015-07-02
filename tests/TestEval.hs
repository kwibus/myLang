module TestEval (testEval
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Data.Maybe

import Eval (apply, eval, fullEval)
import TypeCheck
import BruijnTerm
import qualified ExampleBruijn as B
import TestUtils
import ArbitraryQuickcheck ()
import Enviroment
import MakeTerm
import Type (typeBound2Free)
import Opperator
import Vallue

testEval :: TestTree
testEval = testGroup "eval" [testEvalBasic, testEvalBuildin]

testEvalBasic :: TestTree
testEvalBasic = testGroup "basic"
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
      \ t -> let result = welFormd <$> eval (t :: BruijnTerm ())
            in isNothing result || fromJust result
  -- , testProperty "keep normalisation under eval" $
  --     \ t -> isRight (bruijn2Lam t)==> fmap ((fmap lam2Bruijn) . bruijn2Lam ) (eval t) == fmap (return .return) (eval (t :: BruijnTerm ()))
  , testProperty "keep type under eval" $
      \ e -> let result = eval (e :: BruijnTerm ())
            in isJust result ==> case eval e of
               Nothing -> True
               Just expr2 -> eitht2bool $ do
                    t1 <- solver expr2
                    t2 <- solver e
                    return $ unifys (typeBound2Free t1) (typeBound2Free t2) fEmtyEnv
  ]

eitht2bool :: Either e Bool -> Bool
eitht2bool (Right bool ) = bool
eitht2bool (Left _ ) = False

testEvalBuildin :: TestTree
testEvalBuildin = testGroup "Buildin"
    [ testCase "+1" $
        eval (lambda "#" (appl (appl (val plus) (bvar 0)) (double 1.0))) @?= Nothing
    , testCase "1+" $
        eval (appl (val plus) (double 1.0)) @?= return (val (Eval.apply plus (MyDouble 1.0)))
    , testCase "1+2" $
        eval (appl (appl (val plus) ( double 1.0)) (double 2.0)) @?=
         return (appl (val (Eval.apply plus (MyDouble 1.0))) (double 2.0) )
    , testCase "1+2*3" $
        fullEval (appl (appl (val plus)
                             (appl (appl (val multiply )
                                         (double 2.0))
                                         (double 3.0))
                             )
                             (double 1.0))
        @?= double 7.0
    ]
