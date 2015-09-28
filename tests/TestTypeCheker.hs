module TestTypeCheker (testTypeChecker) where

import Data.Either
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad.Except
import Data.IntMap

import qualified ExampleBruijn as B
import MakeTerm
import ArbitraryType ()
import TestUtils
import ArbitraryLambda

import TypeCheck
import Type
import Operator
import Environment
import TypeError

testTypeChecker :: TestTree
testTypeChecker = testGroup "typeChecker"
                    [ testApply
                    , testUnify
                    , testUnifyEnv
                    , testSolver
                    , testClose
                    ]

testApply :: TestTree
testApply = testGroup "apply"
    -- [ testCase " apply a {a-:a}}== a" $
    --     let t = TVar (Free 1)
    --         env = finsertAt t (Free 1) fEmtyEnv
    --     in apply t env @?= t

    [ testCase " apply a {a-:b}}== b" $
        let env = finsertAt (TVar (Free 2)) (Free 1) fEmtyEnv
        in apply (TVar (Free 1)) env @?= TVar (Free 2)

    , testCase " apply a {a-:b, b-:c}}== c" $
        let env = fFromList [(TVar (Free 2), Free 1), (TVar (Free 3), Free 2)]
        in apply (TVar (Free 1)) env @?= TVar (Free 3)
    ]
testUnify :: TestTree
testUnify = testGroup "unify"
    [ testCase " unifys a b {a-:b}" $
        let env = finsertAt (TVar (Free 2 )) ( Free 1) fEmtyEnv
        in unifys (TVar (Free 1)) (TVar (Free 2)) env @?= True

    , testCase " unifys a b {b-:a}" $
        let env = finsertAt (TVar (Free 1 )) ( Free 2) fEmtyEnv
        in unifys (TVar (Free 1)) (TVar (Free 2)) env @?= True

    , testCase " unifys a (b ->c)  {b-:a}" $
        let env = finsertAt (TVar (Free 1 )) ( Free 2) fEmtyEnv
        in unifys (TVar (Free 1)) (TAppl (TVar (Free 2)) (TVar (Free 3))) env @?= False

    , testCase " unifys a (b ->c)  {a-:b}" $
        let env = finsertAt (TVar (Free 2 )) ( Free 1) fEmtyEnv
        in unifys (TVar (Free 1)) (TAppl (TVar (Free 2)) (TVar (Free 3))) env @?= False

    , testCase " unifys a (a ->b) {a-:c}" $
        let env = finsertAt (TVar (Free 1 )) ( Free 3) fEmtyEnv
        in unifys (TVar (Free 1)) (TAppl (TVar (Free 1)) (TVar (Free 2))) env @?= False

    , testCase " unifys (b ->c) a {b-:a}" $
        let env = finsertAt (TVar (Free 1 )) ( Free 2) fEmtyEnv
        in unifys (TAppl (TVar (Free 2)) (TVar (Free 3))) (TVar (Free 1)) env @?= False

    , testCase " unifys (b ->c) a {a-:b}" $
        let env = finsertAt (TVar (Free 2 )) ( Free 1) fEmtyEnv
        in unifys (TAppl (TVar (Free 2)) (TVar (Free 3))) (TVar (Free 1)) env @?= False


    , testCase " unifys (a ->b) a {a-:c}" $
        let env = finsertAt (TVar (Free 1 )) ( Free 3) fEmtyEnv
        in unifys (TAppl (TVar (Free 1)) (TVar (Free 2))) (TVar (Free 1)) env @?= False

    , testProperty "unify self" $
        \ t -> unifys t t fEmtyEnv

    , testProperty "symetric " $
        \ t1 t2 -> unifys t1 t2 fEmtyEnv == unifys t2 t1 fEmtyEnv

    , testProperty "idempotence" $
        \ t1 t2 -> let u = unify t1 t2 fEmtyEnv
                 in case u of
                    Left _ -> True
                    Right env -> let u2 = unify t1 t2 env
                                 in case u2 of
                                     Left _ -> False
                                     Right env2 -> env == env2
    ]

testUnifyEnv :: TestTree
testUnifyEnv = testGroup " Unify Env "
    [testCase "unifyEnv error" $
        let env1 = fromList [(1, TVal TDouble) ]
            env2 = fromList [(1, TAppl (TVal TDouble) (TVal TDouble))]
        in isLeft (unifyEnv env1 env2) @?= True

    , testCase "unifyEnv error sum" $
        let env1 = fromList [ (1, TVal TDouble)
                            , (2, TVal TDouble)]
            env2 = fromList [ (1, TAppl (TVal TDouble) (TVal TDouble))
                            , (2, TAppl (TVal TDouble) (TVal TDouble))]
        in ( case unifyEnv env1 env2 of
                Left es -> length es == 2
                _ -> False
          ) @?= True
    ]

testClose :: TestTree
testClose = testGroup "close"
    [testProperty "welformd close" $
        welFormdType . close ]

testSolver :: TestTree
testSolver = testGroup "Solver"
   [ testCase "check Double" $
        solver (double 1.0) @?= return (TVal TDouble)
   , testCase "check (+1)" $
        solver (appl (val plus) (double 1.0)) @?=
        return (TAppl (TVal TDouble) (TVal TDouble))
   , testCase "check id" $
        solver B.id @?= return (TAppl (TVar (Bound 0)) (TVar (Bound 0)))

   , testCase "check id 1.0" $
        solver (appl B.id (double 1.0)) @?= return (TVal TDouble )
    , testCase "check \\a.1+a" $
        solver (lambda "a" (appl
                    (appl (val plus ) (double 1.0))
                    (bvar 0)))
                @?=
        return (TAppl (TVal TDouble ) (TVal TDouble))
   , testCase "check \\a.a+1" $
        solver (lambda "a" (appl
                    (appl (val plus ) (bvar 0))
                    (double 1.0)))
                @?=
        return (TAppl (TVal TDouble ) (TVal TDouble))

   , testCase "check (\\a\\b.a) 1" $
        solver (appl
                    (lambda "a" (lambda "b" (bvar 1)))
                    (double 1)
               )
        @?=
        return (TAppl
                  (TVar (Bound 0 ))
                  (TVal TDouble))
   , testCase "check \\a\\b.b a" $
        solver (lambda "a" (lambda "b" (appl
                    (bvar 0)
                    (bvar 1)
                )))
        @?=
        return (TAppl
                  (TVar (Bound 0 ))
                  (TAppl (TAppl (TVar (Bound 0)) (TVar (Bound 1)))
                         (TVar (Bound 1))
                  )
                )

   , testCase "check \\a.a a" $
        solver (lambda "a" (appl
                    (bvar 0)
                    (bvar 0)
                ))
        @?=
        throwError ( UnifyAp undefined undefined undefined (Infinit undefined undefined undefined))

   , testCase "check (\\a.a (a 1.0))" $
        solver (lambda "a" (appl
                    (bvar 0)
                    (appl (bvar 0)
                          (double 1.0)
                    )
                ))
        @?=
        return (TAppl (TAppl (TVal TDouble) (TVal TDouble)) (TVal TDouble))

   , testCase "check (\\f.\\a. f a a)" $
        solver (lambda "f" (lambda "a" (appl
                    (appl (bvar 1)
                          (bvar 0))
                    (bvar 0)
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
               ))
    , testCase "check (\\a.a)(\\b.\\c.b 1.0)" $
        solver (appl (lambda "a " (bvar 0))
                     (lambda "b" (lambda "c" (
                            appl (bvar 1)
                            (double 1.0)
               ) )) )
        @?=
        return (TAppl (TAppl (TVal TDouble )
                             (TVar (Bound 0)))
                      (TAppl (TVar (Bound 1))
                             (TVar (Bound 0)))
                )

    , testCase "fail (+)\\a.a" $
        solver (appl (val plus) B.id ) @?=
        throwError (UnifyAp undefined undefined undefined (Unify undefined undefined undefined))

    , testCase "fail \\a.a a" $
        solver (lambda "a" (appl (bvar 0 ) (bvar 0))) @?=
        throwError (UnifyAp undefined undefined undefined (Infinit undefined undefined undefined))
    , testCase " " $
        solver (appl (lambda "x" (appl (lambda "y" (bvar 1))
                                       (appl (lambda "z" (bvar 1))
                                             (appl (bvar 0)
                                                   (lambda "w" (bvar 0))))))
               B.id)
        @?=
        return ( TAppl (TAppl (TVar (Bound 0)) (TVar (Bound 0)))
                       (TAppl (TVar (Bound 0)) (TVar (Bound 0))))


    , testProperty "idempotence" $
        forAllTypedBruijn $ \ e -> case runInfer $ solveWith e fEmtyEnv bEmtyEnv of
                Left _ -> False
                Right (t1, env1) -> case runInfer $ solveWith e env1 bEmtyEnv of
                    Left _ -> False
                    Right (t2, _) -> close t1 == close t2 -- && env1 == env2

    , testProperty "typeable" $
        forAllTypedBruijn $ \ e -> isRight $ solver e
    ]
