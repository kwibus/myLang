module TestTypeCheker (testTypeChecker) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.IntMap

import qualified ExampleBruijn as B
import MakeTerm
import MakeType
import ArbitraryType ()
import Properties
import ArbitraryLambda

import TypeCheck
import Operator
import BruijnEnvironment
import FreeEnvironment
import TypeError
import ErrorCollector

testTypeChecker :: TestTree
testTypeChecker = testGroup "typeChecker"
                    [ testApply
                    , testUnify
                    , testUnifySubs
                    , testClose
                    , testTypeCheck
                    ]

testApply :: TestTree
testApply = testGroup "apply"
    -- [ testCase " apply a {a-:a}}== a" $
    --     let t = tVar 1
    --         sub = finsertAt t (Free 1) fEmtyEnv
    --     in apply t sub @?= t

    [ testCase " apply a {a-:b}}== b" $
        let sub = finsertAt (tVar 2) (Free 1) fEmtyEnv
        in apply sub (tVar 1) @?= tVar 2

    , testCase " apply a {a-:b, b-:c}}== c" $
        let sub = fFromList [(tVar 2, Free 1), (tVar 3, Free 2)]
        in apply sub (tVar 1) @?= tVar 3
    ]
testUnify :: TestTree
testUnify = testGroup "unify"
    [ testCase "fail:  unifys a (a -> b)" $
        unifys (tVar 1) (tVar 1 ~> tVar 2) @?= False

    , testCase "fail: unify (Bool -> a) (a -> Double)" $
        unifys (tBool ~> tVar 1) (tVar 1 ~> tDouble) @?= False

    , testProperty "unify self" $
        \ t -> unifys t t

    , testProperty "symetric " $
        \ t1 t2 -> unifys t1 t2 == unifys t2 t1

    ]

testUnifySubs :: TestTree
testUnifySubs = testGroup " UnifySubs "
    [testCase "unifySubs error" $
        let sub1 = fromList [(1, tDouble) ]
            sub2 = fromList [(1, tDouble ~> tDouble)]
        in hasSucces (unifySubs sub1 sub2) @?= False

    , testCase "unifySubs error sum" $
        let sub1 = fromList [ (1, tDouble)
                            , (2, tDouble)]
            sub2 = fromList [ (1, tDouble ~> tDouble)
                            , (2, tDouble ~> tDouble)]
        in (case unifySubs sub1 sub2 of
                Error es -> length es == 2
                _ -> False
           ) @?= True

    , testCase "unifySubs [a/b] [b/a]" $
        let sub1 = fromList [(1, tVar 2)]
            sub2 = fromList [(2, tVar 1)]
        in unifySubs sub1 sub2 @?= return sub1

    , testCase " unifysSubs [a/(b ->c)]  [b/a] failse" $
        let sub1 = fromList [(1,tVar 2 ~> tVar 3)]
            sub2 = fromList [(2,tVar 1)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/(b ->c)]  [a/b] fails" $
        let sub1 = fromList [(1,tVar 2 ~> tVar 3)]
            sub2 = fromList [(1,tVar 2)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [b/a] [a/(b ->c)] failse" $
        let sub2 = fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = fromList [(2,tVar 1)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/b] [a/(b ->c)] fails" $
        let sub2 = fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = fromList [(1,tVar 2)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/b->c] [b/(a ->c)] fails" $
        let sub2 = fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = fromList [(2,tVar 1 ~> tVar 3)]
        in hasSucces (unifySubs sub1 sub2 ) @?= False
    ]

testClose :: TestTree
testClose = testGroup "close"
    [testProperty "welformd close" $
        welFormdType . close ]

testTypeCheck :: TestTree
testTypeCheck = testGroup "Type checker"
  [ testBasic
  , testCheckerProperty
  ]

-- TODO consistend names
testBasic :: TestTree
testBasic = testGroup "Solver"
   [ testCase "check Double" $
        solver (double 1.0) @?= return tDouble
   , testCase "check (+1)" $
        solver (appl (val plus) (double 1.0)) @?=
        return (tDouble ~> tDouble)
   , testCase "check id" $
        solver B.id @?= return (tVar 0 ~> tVar 0)

   , testCase "check id 1.0" $
        solver (appl B.id (double 1.0)) @?= return tDouble
    , testCase "check \\a.1+a" $
        solver (lambda "a" (appl
                    (appl (val plus ) (double 1.0))
                    (bvar 0)))
                @?=
        return (tDouble ~> tDouble)
   , testCase "check \\a.a+1" $
        solver (lambda "a" (appl
                    (appl (val plus ) (bvar 0))
                    (double 1.0)))
                @?=
        return (tDouble ~> tDouble)

   , testCase "check (\\a\\b.a) 1" $
        solver (appl
                    (lambda "a" (lambda "b" (bvar 1)))
                    (double 1)
               )
        @?=
        return (tVar 0 ~> tDouble)

   , testCase "check \\a\\b.b a" $
        solver (lambda "a" (lambda "b" (appl
                    (bvar 0)
                    (bvar 1)
                )))
        @?=
        return ( tVar 0 ~> (tVar 0 ~> tVar 1) ~> tVar 1 )

   , testCase "check (\\a.a (a 1.0))" $
        solver (lambda "a" (appl
                    (bvar 0)
                    (appl (bvar 0)
                          (double 1.0)
                    )
                ))
        @?=
        return ((tDouble ~> tDouble) ~> tDouble)

   , testCase "check (\\f.\\a. f a a)" $
        solver (lambda "f" (lambda "a" (appl
                    (appl (bvar 1)
                          (bvar 0))
                    (bvar 0)
                 )))
        @?=
        return (( tVar 0 ~> (tVar 0 ~> tVar 1)) ~> (tVar 0 ~>tVar 1))

    , testCase "check (\\a.a)(\\b.\\c.b 1.0)" $
        solver (appl (lambda "a " (bvar 0))
                     (lambda "b" (lambda "c" (
                            appl (bvar 1)
                            (double 1.0)
               ) )) )
        @?=
        return ((tDouble ~>tVar 0) ~> (tVar 1 ~> tVar 0))

    , testCase "fail (+)\\a.a" $
        solver (appl (val plus) B.id ) @?=
        throw [UnifyAp undefined undefined undefined [Unify undefined undefined ]]

    , testCase "fail \\a.a a" $
        solver (lambda "a" (appl (bvar 0 ) (bvar 0))) @?=
        throw [UnifyAp undefined undefined undefined [Infinit undefined undefined ]]

    , testCase "(\\y.(\\x y)((\\z y)y(\\w.w)) )\\a.a " $
        solver (appl (lambda "x" (appl (lambda "y" (bvar 1))
                                       (appl (lambda "z" (bvar 1))
                                             (appl (bvar 0)
                                                   (lambda "w" (bvar 0))))))
               B.id)
        @?=
        return ( (tVar 0 ~> tVar 0) ~> (tVar 0 ~>tVar 0))

  , testCase "let id = \\a .a in id id" $
        solver (mkLet [("id",lambda "a" (bvar 0))] (appl (bvar 0) (bvar 0)))
        @?= return (tVar 0 ~> tVar 0)

  , testCase "let a = a in a +" $
      solver (mkLet [("a",bvar 0)] $ appl (val plus)(bvar 0))
      @?= return (tDouble ~> tDouble)

    , testCase "let a = 1.0; b = a in b" $
        solver (mkLet [("a",double 1),
        ("b",bvar 1)] $bvar 0)
        @?= return tDouble

  , testCase "let a = true; b = a +; in b" $
      solver (mkLet [("a",true),("b",appl (val plus) (bvar 1))] $bvar 0 )
      @?= throw [UnifyDef tBool tDouble undefined]

  , testCase "let f a = true; b = f True; in b" $
      solver (mkLet [("f",lambda "a" true),("b",appl (bvar 1) true)] $bvar 0 )
      @?= return (tVar 0)

  , testCase "let f a = id 1; id a = a in f 2" $
      solver (mkLet [("f", lambda "a" $ appl (bvar 1) (double 1)), ("id", B.id)] $ appl (bvar 1) (double 2))
      @?= return  tDouble
  ]

testCheckerProperty :: TestTree
testCheckerProperty = testGroup "propertys"
    [ testProperty "idempotence" $
        forAllTypedBruijn $ \ e -> case runInfer $ solveWith e fEmtyEnv bEmtyEnv of
                Error _ -> False
                Result (t1, sub1) -> case runInfer $ solveWith e sub1 bEmtyEnv of
                    Error _ -> False
                    Result (t2, _) -> close t1 == close t2 -- && sub1 == sub2

    , testProperty "typeable" $
        forAllTypedBruijn $ \ e -> hasSucces $ solver e
    ]
