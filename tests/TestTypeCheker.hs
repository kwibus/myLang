module TestTypeCheker (testTypeChecker) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Control.Monad
import qualified Data.IntMap as IM
import Data.Bifunctor

import qualified ExampleBruijn as B
import MakeTerm
import MakeType
import ArbitraryType ()
import TestUtils
import ArbitraryLambda

import BruijnTerm
import PrettyPrint
import Type
import Lambda
import TypeCheck
import Operator
import BruijnEnvironment
import TypeError
import ErrorCollector

testTypeChecker :: TestTree
testTypeChecker = testGroup "typeChecker"
                    [ testApply
                    , testUnify
                    , testUnifySubs
                    , testSolver
                    , testClose
                    ]

testApply :: TestTree
testApply = testGroup "apply"
    -- [ testCase " apply a {a-:a}}== a" $
    --     let t = tVar 1
    --         sub = finsertAt t (Free 1) fEmtyEnv
    --     in apply t sub @?= t

    [ testCase " apply a {a-:b}}== b" $
        let sub =IM.singleton 1 (tVar 2)
        in apply sub (tVar 1) @?= tVar 2

    , testCase " apply a {a-:b, b-:c}}== c" $
        let sub = IM.fromList [(1,tVar 2), (2,tVar 3)]
        in apply sub (tVar 1) @?= tVar 3
    ]
testUnify :: TestTree
testUnify = testGroup "unify"
    [ testCase " unifys a a -> " $
        unifys (tVar 1) ((tVar 1) ~> (tVar 2)) @?= False

    , testProperty "unify self" $
        \ t -> unifys t t

    , testProperty "symetric " $
        \ t1 t2 -> unifys t1 t2 == unifys t2 t1
    ]

testUnifySubs :: TestTree
testUnifySubs = testGroup " UnifySubs "
    [testCase "unifySubs error" $
        let sub1 = IM.fromList [(1, tDouble) ]
            sub2 = IM.fromList [(1, tDouble ~> tDouble)]
        in hasSucces (unifySubs sub1 sub2) @?= False

    , testCase "unifySubs error sum" $
        let sub1 = IM.fromList [ (1, tDouble)
                            , (2, tDouble)]
            sub2 = IM.fromList [ (1, tDouble ~> tDouble)
                            , (2, tDouble ~> tDouble)]
        in (case unifySubs sub1 sub2 of
                Error es -> length es == 2
                _ -> False
           ) @?= True

    , testCase "unifySubs [a/b] [b/a]" $
        let sub1 = IM.fromList [(1, tVar 2)]
            sub2 = IM.fromList [(2, tVar 1)]
        in unifySubs sub1 sub2 @?= return sub1

    , testCase " unifysSubs [a/(b ->c)]  [b/a] failse" $
        let sub1 = IM.fromList [(1,tVar 2 ~> tVar 3)]
            sub2 = IM.fromList [(2,tVar 1)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/(b ->c)]  [a/b] fails" $
        let sub1 = IM.fromList [(1,tVar 2 ~> tVar 3)]
            sub2 = IM.fromList [(1,tVar 2)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [b/a] [a/(b ->c)] failse" $
        let sub2 = IM.fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = IM.fromList [(2,tVar 1)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/b] [a/(b ->c)] fails" $
        let sub2 = IM.fromList [(1,(tVar 2) ~>(tVar 3))]
            sub1 = IM.fromList [(1,tVar 2)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/b->c] [b/(a ->c)] fails" $
        let sub2 = IM.fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = IM.fromList [(2,tVar 1 ~> tVar 3)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False
    ]

testClose :: TestTree
testClose = testGroup "close"
    [testProperty "welformd close" $
        welFormdType . close ]

testSolver :: TestTree
testSolver = testGroup "Solver"
    [ testGroup "testCases" (map (uncurry testCaseSolver) cassesSolver)
    , testProperty "idempotence" $
        forAllTypedBruijn $ \ e -> case runInfer $ solveWith e IM.empty bEmtyEnv of
                Error _ -> False
                Result (t1, sub1) -> case runInfer $ solveWith e sub1 bEmtyEnv of
                    Error _ -> False
                    Result (t2, _) -> close t1 == close t2 -- && sub1 == sub2

    , testProperty "typeable" $
        forAllTypedBruijn $ \ e -> hasSucces $ solver e
    ]
cassesSolver :: [(LamTerm () Bound, ErrorCollector [TypeError ()] Type)]
cassesSolver =
    [ (double 1.0, return tDouble)

    , (appl (val plus) (double 1.0), return (tDouble ~> tDouble))

    , (B.id, return (tVar 0 ~> tVar 0))

    , (appl B.id (double 1.0), return tDouble)

    , (lambda "a" (appl
            (appl (val plus ) (double 1.0))
                  (bvar 0))
      , return (tDouble ~> tDouble))
    , (lambda "a" (appl
                    (appl (val plus ) (bvar 0))
                    (double 1.0))
      , return (tDouble ~> tDouble))

    , ((appl (lambda "a" (lambda "b" (bvar 1)))
            (double 1))
       , return (tVar 0 ~> tDouble))

    , (lambda "a" (lambda "b" (appl
                    (bvar 0)
                    (bvar 1)
                ))
        , return ( tVar 0 ~> (tVar 0 ~> tVar 1) ~> tVar 1 ))

    , (lambda "a" (appl
                    (bvar 0)
                    (bvar 0)
                )
          , throw [ UnifyAp undefined undefined undefined [Infinit undefined undefined ]])

    , (lambda "a" (appl
                    (bvar 0)
                    (appl (bvar 0)
                          (double 1.0)
                    )
                )
       , return ((tDouble ~> tDouble) ~> tDouble))

    , (lambda "f" (lambda "a" (appl
                    (appl (bvar 1)
                          (bvar 0))
                    (bvar 0)
                 ))
        ,return (( tVar 0 ~> (tVar 0 ~> tVar 1)) ~> (tVar 0 ~>tVar 1)))

    , (appl (lambda "a " (bvar 0))
                     (lambda "b" (lambda "c" (
                            appl (bvar 1)
                            (double 1.0)
               ) ))
        , return ((tDouble ~>tVar 0) ~> (tVar 1 ~> tVar 0)))

    , (appl (val plus) B.id
       , throw [UnifyAp undefined undefined undefined [Unify undefined undefined ]])

    , (lambda "a" (appl (bvar 0 ) (bvar 0))
        , throw [UnifyAp undefined undefined undefined [Infinit undefined undefined ]])

    , (appl (lambda "x" (appl (lambda "y" (bvar 1))
                              (appl (lambda "z" (bvar 1))
                                    (appl (bvar 0)
                                          (lambda "w" (bvar 0))))))
            B.id
           , return ( (tVar 0 ~> tVar 0) ~> (tVar 0 ~>tVar 0)))

    , (mkLet [("id",lambda "a" (bvar 0))] (appl (bvar 0) (bvar 0))
        ,return (tVar 0 ~> tVar 0))

    , (appl (lambda "id "( lambda "c" (appl (appl
                            (lambda "a" (lambda "b"(bvar 1)))
                            (appl (bvar 0) (double 1.0)))
                        (appl (bvar 0) (val plus)))))
                (lambda "a" (bvar 0))

         ,throw [UnifySubs undefined  [Unify undefined undefined ] ])

    , (mkLet [("id",B.id)] (appl (appl
                    (lambda "a" (lambda "b"(bvar 1)))
                    (appl (bvar 0) (double 1.0)))
               (appl (bvar 0) (val plus))
            )
        , return tDouble)
    ]

testCaseSolver :: LamTerm () Bound -> ErrorCollector [TypeError ()] Type -> TestTree
testCaseSolver lam expected = testCase (text ++ "solver " ++ lambdaDiscription ) $
        unless (actualType == expected) (assertFailure errorMsg)
    where
      text = case expected of
                Error _ -> "fail:"
                _ -> ""
      lambdaDiscription = either (const "coult not prinrt lambda") PrettyPrint.pShow (bruijn2Lam lam)
      actualType = solver lam
      errorMsg = "expected: " ++ show( second Type.pShow expected) ++
               "\nbut got: " ++ show (second Type.pShow actualType)
