module TestEval (testEval
) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe
import Data.DList

import TypeCheck
import BruijnTerm
import BruijnEnvironment
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
testEval = testGroup "eval"
    [ testSubstituteEnv
    , testUpdateEnv
    , testEvalBasic
    , testEvalBuildin
    , testEvalLet
    ]

testSubstituteEnv :: TestTree
testSubstituteEnv = testGroup "substituteEnv "
    [ testCase "[a/1.0,b/2.0,b/3.0] a = 1.0" $
        let env = bFromList [double 1, double 2, double 3]
        in substituteEnv env (bvar 1 ) @?= double 2

    , testCase "[a/1.0] \\b.a = \\b.1.0" $
        let env = bFromList [double 1.0]
        in substituteEnv env (lambda "b" $ bvar 1 ) @?= lambda "b" (double 1.0)

    , testCase "[a/c] \\b.a = \\b.c" $
        let env = bFromList [bvar 1]
        in substituteEnv env (lambda "b" $ bvar 1 ) @?= lambda "b" (bvar 2)
    ]

testUpdateEnv :: TestTree
testUpdateEnv = testGroup "updateEnv"
    [ testCase "update a [a/1.0] = {}"  $
        let env = bFromList [double 1.0]
        in updateEnv (Bound 0) env @?= empty

    , testCase "update a [a/a] = {}"  $
        let env = bFromList [bvar 0]
        in updateEnv (Bound 0) env @?= empty

    , testCase "update a [a/b ,b/1.0] = [a/1.0,b/1,0]"  $
        let env = bFromList [bvar 0 , double 1]
        in updateEnv (Bound 1) env @?= singleton  (bFromList [double 1, double 1])
    ]

testEvalBasic :: TestTree
testEvalBasic = testGroup "basic"
  [ testCase "eval 1.0" $
        eval (double 1) @?= Nothing

  , testCase "eval id" $
        eval B.id @?= Nothing

  , testCase "eval id(id(\\z.id z))=id(\\z.id z)" $

      eval (appl B.id ( appl B.id (lambda "z" (appl B.id (bvar 0))))) @?=
         Just ( appl B.id ( lambda "z" (appl B.id (bvar 0))))

  , testCase "omega omega" $
      eval (appl B.omega B.omega) @?= Just (appl B.omega B.omega)

  , testCase "eval id id = Just id" $
       evalSteps (appl B.id B.id) @?= return B.id

  , testCase "eval free" $
       eval (bvar 0) @?= Nothing

  , testCase "eval name capture" $
        eval (appl (lambda "b"$ lambda "c" $ bvar 1)(lambda "d" $ bvar 1))
        @?= Just (lambda "c" $ lambda "d" $ bvar 2)

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

--
--   TODO implement (this is only true if e1,e1 terminate and no self reference in e1 )
-- , testProperty"let a = e1 in e2 == (\\a.e2)e1"  forAllTypedBruijn \e1 -> $ forAllTypedBruijn e2
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
  ]
testEvalLet :: TestTree
testEvalLet = testGroup "let"
  [ testCase "fullEval let a = 1.0 in a " $
      fullEval (mkLet [("a", double 1.0) ] (bvar 0))
      @?= double 1.0

  , testCase "evalSteps let a = b ;b=1.0 in a " $
      evalSteps (mkLet [("a", bvar 0), ("b", double 1.0)] (bvar 1))
      @?= [mkLet [("a", double 1.0), ("b", double 1.0)] (bvar 1),double 1.0]

  , testCase "fullEval let a = \\b.b in a " $
      fullEval (mkLet [("a", B.id) ] (bvar 0))
      @?= B.id

  , testCase "fullEval let a = \\b.1.0 in \\c.a " $
      fullEval(mkLet [("a",lambda "b" (double 1)) ] $ lambda "c" (bvar 1))
      @?=  lambda "c" ( lambda "b"$ double 1 )

  , testCase "fullEval let a = (let b = 1.0 in b) ;in a " $
      fullEval (mkLet [("a", mkLet [("b", double 1.0)] (bvar 0))] (bvar 0))
      @?= double 1.0

  , testCase "fullEval let a = a; in a " $
      fullEval (mkLet [("a", bvar 0)] (bvar 0))
      @?= double 1.0

  , testCase "instantane substituion"  $
        take 1 ( evalSteps $ mkLet [("a",double 1)] $ appl (bvar 0) (bvar 0))
        @?= [appl (double  1) (double 1)]
  ]
