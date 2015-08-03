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
import ArbitraryQuickcheck
import Enviroment
import MakeTerm
import Opperator
import Vallue
import Expresion
import Type

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
      forAllTypedBruijn $ \ t -> let result = welFormd <$> eval (t :: BruijnTerm ())
            in isNothing result || fromJust result
  , testProperty "keep normalisation under eval" $
     forAllUnTypedBruijn $ \ t ->
            let result = eval t
            in isJust result ==> normalised $ fromJust result
  , testProperty "keep type under eval" $
      forAllTypedBruijn $ \ e -> let result = eval (e :: BruijnTerm ())
            in isJust result ==>
                let expr2 = fromJust result
                in eitht2bool $ do
                    t2 <- solver expr2
                    t1 <- solver e
                    return $ counterexample (
                          "\neval:" ++ show expr2 ++
                        "\n\npShow     : " ++ show (fmap pShow (bruijn2Lam e)) ++
                          "\n\t::" ++ tShow t1 ++
                         "\n\npShow eval: " ++ show (fmap pShow (bruijn2Lam expr2)) ++
                          "\n\t::" ++ tShow t2
                            )
                        $ unifys (typeBound2Free t1)
                                 (mapVar (\ (Free i) -> Free (i + 10000)) (typeBound2Free t2))
                                 fEmtyEnv
-- you  can`t use t1 == t2  because
-- "(\\g.(\\y.g)    ::(a -> b -> a) -> Double  evals to:    "(\\y f.1.0)        ::a -> Double
--       (g                                   ============>   ((\\f.1.0)
--        (\\x u.x)))                                            \\x u.x)"
--   \\f.1.0"
-- so you have to use unifys
-- but name me overlap; so a hack solution is to add a large numer to the var of one type.
-- because is generated type's woult normalie have no big numbers in them; so no overlap
  ]

eitht2bool :: Either e Property -> Property
eitht2bool (Right p ) = p
eitht2bool (Left _ ) = property False

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
