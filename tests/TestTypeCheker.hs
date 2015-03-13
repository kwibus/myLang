module TestTypeCheker (testTypeChecker) where
import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad.Error
-- import Control.Monad.Except

import qualified ExampleBruijn as B
import TypeCheck
import Lambda
-- import BruijnTerm
import Vallue
import Type
import Opperator
import Enviroment
import ArbitraryType()
import TestUtils
import ArbitraryQuickcheck()

testTypeChecker :: TestTree
testTypeChecker = testGroup "typeChecker"
                    [ testUnify 
                    , testSolver
                    , testClose
                    ]

testUnify :: TestTree
testUnify = testGroup "unify"
    [ testCase " unifys a b {a->b}" $
        let env = finsertAt (TVar (Free 2 ))( Free 1) fEmtyEnv 
        in  unifys (TVar (Free 1)) (TVar (Free 2)) env @?= True

    , testCase " unifys a b {b->a}" $
        let env = finsertAt (TVar (Free 1 ))( Free 2) fEmtyEnv 
        in  unifys (TVar (Free 1)) (TVar (Free 2)) env @?= True

    , testProperty "unify self" $ 
        \t -> unifys t t fEmtyEnv

    , testProperty "symetric " $
        \t1 t2 -> unifys t1 t2 fEmtyEnv == unifys t2 t1 fEmtyEnv

    , testProperty "idempotence" $
        \t1 t2 -> let u =unify t1 t2 fEmtyEnv 
                 in case u of 
                    Left _ -> True 
                    Right env ->  let u2 = unify t1 t2 env
                                 in case u2 of 
                                     Left _ -> False
                                     Right env2 -> env == env2   
    ]

testClose :: TestTree 
testClose = testGroup "close"
    [testProperty "welformd close" $ 
        welFormdType . close ]
testSolver :: TestTree
testSolver = testGroup "Solver"
   [ testCase "check Double" $
        solver (Val (MyDouble 1.0)) @?= return (TVal TDouble)
   , testCase "check (+1)" $
        solver (Appl (Val plus) (Val (MyDouble 1.0))) @?=
        return (TAppl (TVal TDouble) (TVal TDouble))
   , testCase "check id" $
        solver B.id @?= return (TAppl (TVar (Bound 0)) (TVar (Bound 0)))

   , testCase "check id 1.0" $
        solver (Appl B.id (Val (MyDouble 1.0))) @?= return (TVal TDouble )
    , testCase "check \\a.1+a" $
        solver (Lambda "a" (Appl
                    (Appl (Val plus ) (Val (MyDouble 1.0) ))
                    (Var (Bound 0))))
                @?=
        return (TAppl (TVal TDouble ) (TVal TDouble))
   , testCase "check \\a.a+1" $
        solver (Lambda "a" (Appl
                    (Appl (Val plus ) (Var (Bound 0)))
                    (Val (MyDouble 1.0) )))
                @?=
        return (TAppl (TVal TDouble ) (TVal TDouble))

   , testCase "check (\\a\\b.a) 1" $
        solver (Appl
                    (Lambda "a" (Lambda "b" (Var (Bound 1) )))
                    (Val (MyDouble 1))
               )
        @?=
        return (TAppl
                  (TVar (Bound 0 ))
                  (TVal TDouble))
   , testCase "check \\a\\b.b a" $
        solver (Lambda "a" (Lambda "b" (Appl
                    (Var (Bound 0) )
                    (Var (Bound 1))
                )))
        @?=
        return (TAppl
                  (TVar (Bound 0 ))
                  (TAppl (TAppl (TVar (Bound 0)) (TVar (Bound 1)))
                         (TVar (Bound 1))
                  )
                )

   , testCase "check \\a.a a" $
        solver (Lambda "a" (Appl
                    (Var (Bound 0) )
                    (Var (Bound 0))
                ))
        @?=
        throwError "infintType"

   , testCase "check (\\a.a (a 1.0))" $
        solver (Lambda "a" (Appl
                    (Var (Bound 0) )
                    (Appl (Var (Bound 0))
                          (Val (MyDouble 1.0))
                    )
                ))
        @?=
        return (TAppl (TAppl (TVal TDouble) (TVal TDouble)) (TVal TDouble))

   , testCase "check (\\f.\\a. f a a)" $
        solver (Lambda "f" (Lambda "a" (Appl
                    (Appl (Var (Bound 1))
                          (Var (Bound 0)))
                    (Var (Bound 0))
                 )))
        @?=
        return (TAppl (TAppl
                            (TVar (Bound 0))
                            (TAppl
                                 (TVar (Bound 0))
                                 (TVar (Bound 1))
                    )) (TAppl
                        (TVar (Bound 0))
                        (TVar (Bound 1))
               )   )
    , testCase "check (\\a.a)(\\b.\\c.b 1.0)"$
        solver (Appl (Lambda "a " (Var (Bound 0))) 
                     (Lambda "b"(Lambda "c" (
                            Appl (Var (Bound 1))
                            (Val (MyDouble 1.0))
               ) )) )
        @?=
        return (TAppl (TAppl (TVal TDouble )
                             (TVar (Bound 0)))
                      (TAppl (TVar (Bound 1))
                             (TVar (Bound 0)))
                )
    , testProperty "idempotence" $
        \e -> case solveWith e fEmtyEnv bEmtyEnv of
                Left _ -> False
                Right (t1,env1) -> case solveWith e env1 bEmtyEnv of
                    Left _ -> False  
                    Right (t2, _) -> close t1 == close t2 -- && env1 == env2
    , testProperty "typeable"$  isRight .solver
    ]
