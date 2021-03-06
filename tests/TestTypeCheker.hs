module TestTypeCheker (testTypeChecker) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.IntMap
import Data.Either

import qualified ExampleBruijn as B

import Info
import qualified MakeTypedTerm as T
import MakeTerm
import MakeType
import ArbitraryType ()
import ArbitraryLambda

import Type (Type,normalise)
import TypeCheck
import Operator
import FreeEnvironment
import TypeError

testTypeChecker :: TestTree
testTypeChecker = testGroup "typeChecker"
                    [ testApply
                    , testUnify
                    , testUnifySubs
                    , testTypeCheck
                    , testAnnotate
                    , testReadType
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
        \ t -> unifys t (t::Type)

    , testProperty "symetric " $
        \ t1 t2 -> unifys t1 t2 == unifys t2 (t1::Type)

    ]

testUnifySubs :: TestTree
testUnifySubs = testGroup " UnifySubs "
    [testCase "unifySubs error" $
        let sub1 = fromList [(1, tDouble) ]
            sub2 = fromList [(1, tDouble ~> tDouble)]
        in isRight (unifySubs sub1 sub2) @?= False

    , testCase "unifySubs error sum" $
        let sub1 = fromList [ (1, tDouble)
                            , (2, tDouble)]
            sub2 = fromList [ (1, tDouble ~> tDouble)
                            , (2, tDouble ~> tDouble)]
        in (case unifySubs sub1 sub2 of
                Left es -> length es == 2
                _ -> False
           ) @?= True

    , testCase "unifySubs [a/b] [b/a]" $
        let sub1 = fromList [(1, tVar 2)]
            sub2 = fromList [(2, tVar 1)]
        in unifySubs sub1 sub2 @?= return sub1

    , testCase " unifysSubs [a/(b ->c)]  [b/a] failse" $
        let sub1 = fromList [(1,tVar 2 ~> tVar 3)]
            sub2 = fromList [(2,tVar 1)]
        in isRight (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/(b ->c)]  [a/b] fails" $
        let sub1 = fromList [(1,tVar 2 ~> tVar 3)]
            sub2 = fromList [(1,tVar 2)]
        in isRight (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [b/a] [a/(b ->c)] failse" $
        let sub2 = fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = fromList [(2,tVar 1)]
        in isRight (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/b] [a/(b ->c)] fails" $
        let sub2 = fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = fromList [(1,tVar 2)]
        in isRight (unifySubs sub1 sub2 ) @?= False

    , testCase " unifysSubs [a/b->c] [b/(a ->c)] fails" $
        let sub2 = fromList [(1,tVar 2 ~> tVar 3)]
            sub1 = fromList [(2,tVar 1 ~> tVar 3)]
        in isRight (unifySubs sub1 sub2 ) @?= False
    ]

testTypeCheck :: TestTree
testTypeCheck = testGroup "Type checker"
  [ testBasic
  ]

-- TODO consistend names
-- TODO consistend indentation
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
        Left [UnifyAp undefined undefined undefined [Unify undefined undefined ]]

    , testCase "fail \\a.a a" $
        solver (lambda "a" (appl (bvar 0 ) (bvar 0))) @?=
        Left [UnifyAp undefined undefined undefined [Infinit undefined undefined ]]

    , testCase "(\\x.(\\y z. z)(x 1.0)(x True))\\a.a" $
        solver (appl (lambda "x" (appl (appl (lambda "y" $lambda "z"(bvar 0))
                                             (appl (bvar 0) (double 1)))
                                         (appl (bvar 0) true)))
               B.id)
        @?= Left [UnifyAp undefined undefined undefined  [Unify tDouble tBool]]


    , testCase "(\\x.(\\y.x)((\\z.x)(x(\\w.w))))\\a.a" $
        solver (appl (lambda "x" (appl (lambda "y" (bvar 1))
                                       (appl (lambda "z" (bvar 1))
                                             (appl (bvar 0)
                                                   (lambda "w" (bvar 0))))))
               B.id)
        @?=
        return ((tVar 0 ~> tVar 0) ~> (tVar 0 ~>tVar 0))

  , testCase "let a = 1 in a" $
        solver (mkLet [("a",double 1)
                      ] $ bvar 0)
        @?= return tDouble

  , testCase "let id = \\a .a in id id" $
        solver (mkLet [("id",lambda "a" (bvar 0))] (appl (bvar 0) (bvar 0)))
        @?= return (tVar 0 ~> tVar 0)

  , testCase "let id = \\a .a; idid = id id in idid" $
        solver (mkLet [("id",lambda "a" (bvar 0))
                      ,("idid", appl (bvar 1) (bvar 1))
                      ] $ bvar 0)
        @?= return (tVar 0 ~> tVar 0)

  , testCase "let idid = id id; id = \\a.a in idid" $
        solver (mkLet  [("idid", appl (bvar 0) (bvar 0))
                       ,("id",lambda "a" (bvar 0))
                       ] $ bvar 1)
        @?= return (tVar 0 ~> tVar 0)

  , testCase "let a = a in a +" $
      solver (mkLet [("a",bvar 0)] $ appl (val plus)(bvar 0))
      @?= return (tDouble ~> tDouble)

  , testCase "let a = 1.0; b = a in b" $
      solver (mkLet [("a",double 1),
      ("b",bvar 1)] $ bvar 0)
      @?= return tDouble

  , testCase "(\\b.let a = b; in a) true" $
      solver (appl (lambda "b" $ mkLet [("a",bvar 1)] $ bvar 0) true)
      @?= return tBool

  , testCase "let a = true; b = a +; in b" $
      solver (mkLet [("a",true),("b",appl (val plus) (bvar 1))] $bvar 0 )
      @?= Left [UnifyAp undefined undefined undefined [Unify tBool tDouble]]

  , testCase "let f a = true; b = f True; in b" $
      solver (mkLet [("f",lambda "a" true),("b",appl (bvar 1) true)] $ bvar 0 )
      @?= return tBool

  , testCase "let a =  true; b c = a in \\c.a" $
      solver (mkLet [("a", true), ("b", lambda "c" $ bvar 2)] $ lambda "c" $ bvar 2)
      @?= return ( tVar 0 ~> tBool)

  , testCase "let f a = id 1; id a = a in f 2" $
      solver (mkLet [("f", lambda "a" $ appl (bvar 1) (double 1)), ("id", B.id)] $ appl (bvar 1) (double 2))
      @?= return tDouble

  , testCase "let x = let y = p 1.0 2.0 in y; p = (+) in x" $
      solver (mkLet [("x", mkLet [("y",bvar 1 `appl` double 1 `appl` double 2 )] $ bvar 0), ("p", val plus)] $ bvar 1)
      @?= return tDouble

  , testCase "let ap f a = f a in ap (ap (+) 1) 2" $
      solver (mkLet [("ap",lambda "f" $ lambda "a" $ appl (bvar 1) (bvar 0))]
          $ bvar 0 `appl` (bvar 0 `appl` val plus `appl` double 1)`appl` double 2.0 )
      @?= return tDouble

  , testCase "let f a = a (f a) in f" $
      solver (mkLet [("f", lambda "a" $ appl (bvar 0) (appl (bvar 1) (bvar 0)))] $ bvar 0)
      @?= return ((tVar 0 ~> tVar 1) ~> tVar 1)

  , testCase "let f a = g; g = f in f" $
      solver (mkLet [("f", lambda "a" $ bvar 1 ), ("g", bvar 1)] $ bvar 1)
      @?= Left [UnifyAp undefined undefined undefined [Infinit (Free 1)(tVar 3 ~> tVar 1) ]]

  , testCase "let g f x = f (f f) ; c a b = a in g (c true) 1"  $
      solver (mkLet [("twice", lambda "f" $ lambda "x" $ appl (bvar 1) (appl (bvar 0)(bvar 1)))
                    ,("const", lambda "a" $ lambda "b" $ bvar 1 )] $
                    appl (appl (bvar 1)(appl (bvar 0) true)) (double 1))
      @?= Left [UnifyAp undefined undefined undefined [Unify undefined undefined ]]
  ]

testAnnotate :: TestTree
testAnnotate = testGroup "anotate"
  [ testProperty "anotate ast ~ ast" $ forAllTypedBruijn $ \ast  ->
    case annotate ast of
        (Right typedAst) -> removeInfo  typedAst === ast
        (Left e) -> property $ counterexample ("fail anotate: " ++show e) False

  , testProperty "readType anotate = TypeCheck ast " $ forAllNonCiculair $ \ast  ->
    let anotated = annotate ast in counterexample (show anotated ) $ (normalise <$> solver ast) === fmap (normalise . readType) anotated
  ]

testReadType :: TestTree
testReadType = testGroup "readType"
  -- TODO use types from typechck
  [ testCase "\\a.let b c = c in b a" $
    readType (T.lambda "a" (tVar 0) $ T.appl
        (T.mkLet [("b",tVar 1 ~> tVar 1, T.lambda "c" (tVar 1) (T.bvar 0 ))] $ T.bvar 0)
        (T.bvar 0))
      @?=  tVar 0 ~> tVar 0
  ]
