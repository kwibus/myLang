module TestEval (testEval
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe

import TypeCheck
import BruijnTerm
import qualified ExampleBruijn as B
import TestUtils
import ArbitraryLambda

import Eval
import FreeEnvironment
import MakeTerm
import Operator
import Value
import PrettyPrint
import ErrorCollector
import qualified Type as T

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

  , testProperty "everystep a change" $ forAllTypedBruijn  $
        hasRelation (/=) .take 10. evalSteps

  , testProperty "welformd presevation eval" $ forAllTypedBruijn $
        all welFormd . take 10 . evalSteps

  , testProperty "keep normalisation under eval" $ forAllTypedBruijn $
            all normalised . take 10 . evalSteps

  , testProperty "keep type under eval" $ forAllTypedBruijn $ \ e
        -> let result = eval e
            in isJust result ==>
                let expr2 = fromJust result
                in errorCol2Bool $ do
                    t2 <- solver expr2
                    t1 <- solver e
                    return $ counterexample (
                          "\neval:" ++ show expr2 ++
                        "\n\npShow     : " ++ show (fmap pShow (bruijn2Lam e)) ++
                          "\n\t::" ++ T.pShow t1 ++
                         "\n\npShow eval: " ++ show (fmap pShow (bruijn2Lam expr2)) ++
                          "\n\t::" ++ T.pShow t2)
                        $ unifys t1
                                 (T.mapVar (\ (Free i) -> Free (i + 10000)) t2)
-- you  can`t use t1 == t2  because
-- "(\\g.(\\y.g)    ::(a -> b -> a) -> Double  evals to:    "(\\y f.1.0)        ::a -> Double
--       (g                                   ============>   ((\\f.1.0)
--        (\\x u.x)))                                            \\x u.x)"
--   \\f.1.0"
-- so you have to use unifys
-- but name me overlap; so a hack solution is to add a large numer to the var one side.
-- because is generated type's would normally have a big numbers in it; so no overlap
  ]

errorCol2Bool :: ErrorCollector e Property -> Property
errorCol2Bool (Result p ) = p
errorCol2Bool (Error _ ) = property False

hasRelation  :: (a -> a -> Bool) -> [a] -> Bool
hasRelation _ [] = True
hasRelation _ [_] = True
hasRelation relation (a:b:rest )
    | relation a b  = hasRelation relation  (b:rest)
    | otherwise = False

testEvalBuildin :: TestTree
testEvalBuildin = testGroup "Buildin"
  [ testCase "+1" $
      eval (lambda "#" (appl (appl (val plus) (bvar 0)) (double 1.0))) @?= Nothing
  , testCase "1+" $
      eval (appl (val plus) (double 1.0)) @?= return (val (Eval.applyValue plus ( Prim $ MyDouble 1.0)))
  , testCase "1+2" $
      eval (appl (appl (val plus) ( double 1.0)) (double 2.0)) @?=
       return (appl (val (Eval.applyValue plus (Prim $ MyDouble 1.0))) (double 2.0) )
  , testCase "1+2*3" $
      fullEval (appl (appl (val plus)
                           (appl (appl (val multiply )
                                       (double 2.0))
                                       (double 3.0))
                           )
                           (double 1.0))
      @?= double 7.0

  , testCase "fullEval1 let a = 1.0 in 0 " $
      fullEval (mkLet [("a", double 1.0) ] (bvar 0))
      @?= double 1.0

  , testCase "fullEval2 let a = 0 ;b=1.0 in 1 " $
      fullEval (mkLet [("a", bvar 0), ("b", double 1.0)] (bvar 1))
      @?= double 1.0

  , testCase "fullEval3 let a = (let b = 1.0 in 0) ;in 0 " $
      fullEval (mkLet [("a", mkLet [("b", double 1.0)] (bvar 0))] (bvar 0))
      @?= double 1.0
  ]
