module TestEval (testEval
) where
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe
import Data.List

import TypeCheck
import BruijnTerm
import qualified ExampleBruijn as B
import Properties
import ArbitraryLambda

import TopologicalSort
import Eval
import FreeEnvironment
import MakeTerm
import Operator
import Value
import ErrorCollector
import qualified Type as T
import Modify

-- TODO test with free variables
testEval :: TestTree
testEval = testGroup "eval"
    [ testEvalBasic
    , testEvalBuildin
    , testEvalLet
    , testEvalProp
    ]

testEvalBasic :: TestTree
testEvalBasic = testGroup "basic"
  [ testCase "eval 1.0" $
        eval (double 1) @?= Nothing

  , testCase "eval id" $
        eval B.id @?= Nothing

  , testCase "eval id true" $
        eval (appl B.id true) @?= Just true

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
        eval (appl (lambda "b" $ lambda "c" $ bvar 1) (lambda "d" $ bvar 1))
        @?= Just (lambda "c" $ lambda "d" $ bvar 2)

  , testCase "call by vallu termination" $
      eval (lambda "z" (appl B.id (bvar 1))) @?= Nothing

  , testCase "(\\a. a True)\\b.1.0" $
      evalSteps (appl (lambda "a" (appl (bvar 0) true)) (lambda "b" $ double 1.0) )
      @?= [ appl (lambda "b" $ double 1) true
         , double 1]

  , testCase "(\\b.(\\a.a)b)1.0" $
      fullEval (appl (lambda "b" (appl B.id (bvar 0))) (double 1.0))
      @?= double 1

  , testCase "(\\b.id id b)1.0" $
      evalSteps (appl (lambda "b" (appl (appl B.id B.id) (bvar 0))) (double 1))
      @?= [ appl (appl B.id B.id) (double 1)
          , appl B.id (double 1)
          , double 1
          ]

  , testCase "(\\a.a(a true)) \\b.false" $
    evalSteps (appl (lambda "a" $ appl (bvar 0) (appl (bvar 0) true)) (lambda "b" false))
    @?= [ appl (lambda "b" false) (appl (lambda "b" false) true )
        , appl (lambda "b" false) false
        , false
        ]

  , testCase "(\\a.a(a true)) \\b.c" $
    evalSteps (appl (lambda "a" $ appl (bvar 0) (appl (bvar 0) true)) (lambda "b" $ bvar 1))
    @?= [ appl (lambda "b" $ bvar 1) (appl (lambda "b" $ bvar 1) true )
        , appl (lambda "b" $ bvar 1) $ bvar 0
        , bvar 0
        ]

  , testCase "(\\a b.a) true false" $
    evalSteps (appl (appl (lambda "a" $ lambda "b" $ bvar 1 ) true) false)
    @?= [ appl (lambda "b" true ) false
        , true
        ]
  ]

testEvalBuildin :: TestTree
testEvalBuildin = testGroup "Buildin"
  [ testCase "+1" $
      eval (lambda "#" (appl (appl (val plus) (bvar 0)) (double 1.0))) @?= Nothing
  , testCase "1+" $
      eval (appl (val plus) (double 1.0)) @?= return plus1
  , testCase "1+2" $
      eval (appl (appl (val plus) ( double 1.0)) (double 2.0))
      @?= return (appl plus1 (double 2.0) )
  , testCase "1+2*3" $
      fullEval (appl (appl (val plus)
                           (appl (appl (val multiply )
                                       (double 2.0))
                                       (double 3.0))
                           )
                           (double 1.0))
      @?= double 7.0
  ]
plus1 :: BruijnTerm ()
plus1 = val $ applyValue plus (Prim $ MyDouble 1)

-- TODO  sort and cleanup test description
testEvalLet :: TestTree
testEvalLet = testGroup "let"
  [ testCase "eval let a = 1.0 in a" $
      evalSteps (mkLet [("a", double 1) ] (bvar 0))
      @?= [ mkLet [("a", double 1) ] (double 1)
          , double 1.0]

  , testCase "eval let a = 1.0 in b" $
      eval (mkLet [("a", double 1.0) ] (bvar 1))
      @?= Nothing

  , testCase "evalSteps let a = 1.0 in \\b.a" $
      evalSteps (mkLet [("a", double 1) ] $ lambda "b" $ bvar 1)
      @?= []

  , testCase "eval let a = 1.0 + 2.0 in a" $
      evalSteps (mkLet [("a", appl (appl (val plus) (double 1)) (double 2))] (bvar 0))
      @?= [ mkLet [("a", appl plus1 (double 2))] $ bvar 0
          , mkLet [("a", double 3)] $ bvar 0
          , mkLet [("a", double 3)] $ double 3
          , double 3.0]

  , testCase "fullEval let  c = 1; a = let b = c in b;in a" $
      fullEval (mkLet [("c", double 1), ("a", mkLet [("b", bvar 1)] $ bvar 0)] $ bvar 1)
      @?= double 1

  , testCase "evalSteps let b=1.0; a = b in a " $
      evalSteps (mkLet [("b", double 1.0), ("a", bvar 1)] (bvar 0))
      @?= [ mkLet [("b", double 1.0), ("a", double 1.0)] $ bvar 0
          , mkLet [("b", double 1.0), ("a", double 1.0)] $ double 1
          , double 1]

  , testCase "evalSteps let a = true; b = c; c = false in true " $
      evalSteps (mkLet [("a", true), ("b", bvar 2), ("c", false)] true)
      @?= [ mkLet [("a", true), ("b", true), ("c", false)] true
          , true]

  , testCase "evalSteps let a=true; a = false; c = a in true " $
      evalSteps (mkLet [("a", true), ("b", false), ("c", bvar 2)] true)
      @?= [mkLet [ ("a", true), ("b", false), ("c", true)] true
          , true]

  , testCase "evalSteps let a = \\b.b in a " $
      evalSteps (mkLet [("a", B.id) ] (bvar 0))
      @?= [mkLet [("a", B.id)] B.id]

  , testCase "fullEval let a = \\b.1.0 in \\c.a " $
      eval (mkLet [("a", lambda "b" (double 1)) ] $ lambda "c" (bvar 1))
       @?= Nothing

  , testCase "evalSteps let a = \\c.b ;b=1.0 in a " $
      evalSteps (mkLet [("a", lambda "c" $ bvar 1), ("b", double 1.0)] (bvar 1))
      @?= [ mkLet [("a", lambda "c" $ bvar 1), ("b", double 1.0)] $ lambda "c" $ bvar 1 ]

  , testCase "evalSteps let a = let b = 1.0 in b ;in a " $
      evalSteps (mkLet [("a", mkLet [("b", double 1)] (bvar 0))] (bvar 0))
      @?= [ mkLet [("a", mkLet [("b", double 1)] $ double 1)] (bvar 0)
          , mkLet [("a", double 1)] (bvar 0)
          , mkLet [("a", double 1)] (double 1)
          , double 1.0
          ]

  , testCase "eval let a = 1 + in  a" $
        eval (mkLet [("a", appl (val plus) (double 1))] $ bvar 0)
        @?= Just (mkLet [("a", plus1)] $ bvar 0 )

  , testCase "eval let a = 1 + in let b = 2 in a" $
        eval (mkLet [("a", appl (val plus) (double 1))] $ mkLet [("b", double 2)] $ bvar 1)
        @?= Just (mkLet [("a", plus1)] $ mkLet [("b", double 2)] $ bvar 1)

  , testCase "evalSteps (let a = 1.0) ;in \\b.a+b) 2.0 " $
      evalSteps (appl (mkLet [("a", double 1)]
                          (lambda "b" $ appl (appl (val plus) (bvar 1))
                                             (bvar 0)))
                     (double 2))
      @?= [ mkLet [("a", double 1)] (appl (appl (val plus) (bvar 0)) (double 2))
          , mkLet [("a", double 1)] (appl (appl (val plus) (double 1)) (double 2))
          , mkLet [("a", double 1)] (appl plus1 (double 2))
          , mkLet [("a", double 1)] $ double 3
          , double 3]

  , testCase "fullEval (let b = 1.0) ;in \\a.a+b) 2.0 " $
      fullEval (appl (mkLet [("b", double 1)]
                            (lambda "a" $ appl (appl (val plus) (bvar 0))
                                             (bvar 1)))
                     (double 2))
      @?= double 3

  , testCase "let b = true; in id " $
      eval (mkLet [("b", true)] B.id)
      @?= Nothing

  , testCase "let a = \\b.a; in a " $
      evalSteps ( mkLet [("a", lambda "b" (bvar 1))] (bvar 0))
      @?= [ mkLet [("a", lambda "b" (bvar 1))] $ lambda "b" $ bvar 1]

  , testCase "(let a = false; in id ) true " $
      evalSteps (appl ( mkLet [("a", false)] B.id) true)
      @?= [mkLet [("a", false)] true, true ]

  , testCase "(let a = \\b.a; in a) true " $
      evalSteps (appl ( mkLet [("a", lambda "b" (bvar 1))] (bvar 0)) true)
      @?= [ appl ( mkLet [("a", lambda "b" (bvar 1))] $ lambda "b" $ bvar 1) true
          , mkLet [("a", lambda "b" $ bvar 1)] $ bvar 0
          , mkLet [("a", lambda "b" $ bvar 1)] $ lambda "b" $ bvar 1
          ]

  , testCase "(let a = \\b.b; in a) true " $
      evalSteps (appl ( mkLet [("a", lambda "b" (bvar 0))] (bvar 0)) true)
      @?= [ appl ( mkLet [("a", lambda "b" (bvar 0))] $ lambda "b" $ bvar 0) true
          , mkLet [("a", lambda "b" $ bvar 0)] true
          , true
          ]

  , testCase "let c = True ;a = (\\b.c) false;in a" $
      evalSteps (mkLet [("c", true), ("a", appl (lambda "b" (bvar 2)) false)] (bvar 0))
      @?= [ mkLet [("c", true), ("a", bvar 1)] $ bvar 0
          , mkLet [("c", true), ("a", true)] $ bvar 0
          , mkLet [("c", true), ("a", true)] true
          , true]

  , testCase "let a = id; b = a in b" $
      evalSteps ( mkLet [("a", B.id), ("b", bvar 1)] $ bvar 0)
      @?= [ mkLet [("a", B.id), ("b", B.id)] $ bvar 0
          , mkLet [("a", B.id), ("b", B.id)] B.id
          ]

  , testCase "(\\b.let a = b in false)true" $
      evalSteps (appl (lambda "b" $ mkLet [("a", bvar 1)] false) true)
      @?= [ mkLet [("a", true)] false
          , false]

  , testCase "(\\b.let a = b in a)true" $
      evalSteps (appl (lambda "b" $ mkLet [("a", bvar 1)] $ bvar 0) true)
      @?= [ mkLet [("a", true)] $ bvar 0
          , mkLet [("a", true)] true
          , true
          ]

  , testCase "(\\b.let a = b in b)true" $
      evalSteps (appl (lambda "b" $ mkLet [("a", bvar 1)] $ bvar 1) true)
      @?= [ mkLet [("a", true)] true
          , true]

  , testCase "let a = 1 in let b = true ; c = b in false  " $
      evalSteps (mkLet [("a", double 1)] $ mkLet [("b", true), ("c", bvar 1)] false )
      @?= [ mkLet [("a", double 1)] $ mkLet [("b", true), ("c", true)] false
          , mkLet [("a", double 1)] false
          , false]

  , testCase "let x = 1 + in (\\y.x)true" $
       evalSteps (mkLet [("x", appl (val plus) (double 1))] $ appl (lambda "y" $ bvar 1) true )
       @?= [ mkLet [("x", plus1)] $ appl (lambda "y" $ bvar 1) true
           , mkLet [("x", plus1)] $ bvar 0
           , mkLet [("x", plus1)] plus1
           , plus1 ]

  , testCase "let f = \\a.f 1 in f 2 " $
        take 4 (evalSteps (mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $ appl (bvar 0) (double 2)))
        @?= [ mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $
                    appl (lambda "a" $ appl (bvar 1) (double 1)) (double 2)
            , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $
                    appl (bvar 0) (double 1)
            , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $
                    appl (lambda "a" $ appl (bvar 1) (double 1)) (double 1)
            , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $ appl (bvar 0) (double 1)
            ]
  ]

testEvalProp :: TestTree
testEvalProp = testGroup "propertys" $
  let forAllNonCiculair prop = forAllTypedBruijn $ \ e -> case sortTerm e of
          Left {} -> property Discard
          (Right newT) -> prop $ applyModify newT
  in
  [ testProperty "evalSteps == unfold eval" $ forAllNonCiculair $ \ e ->
        take 10 (evalSteps e) === take 10 (unfoldr (\ e0 -> let nextE = eval e0
                                                                clone a = (a, a)
                                                            in fmap clone nextE) e)

  , testProperty "everystep a change" $ forAllNonCiculair $
        hasRelation (/=) . take 10 . evalSteps

  , testProperty "welformd presevation eval" $ forAllNonCiculair $
        conjoin . map welFormd . take 10 . evalSteps

  , testProperty "keep normalisation under eval" $ forAllNonCiculair $
        conjoin . map normalised . take 10 . evalSteps

  , testProperty "keep type under eval" $ forAllNonCiculair $ \ e
        -> let result = eval e
           in isJust result ==>
                let expr2 = fromJust result
                in errorCol2Bool $ do
                    t2 <- solver expr2
                    t1 <- solver e
                    return $ counterexample (
                          "\neval:" ++ show expr2 ++
                        "\n\npShow     : " ++ pShow e ++
                          "\n\t::" ++ T.pShow t1 ++
                         "\n\npShow eval: " ++ pShow expr2 ++
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

hasRelation :: (a -> a -> Bool) -> [a] -> Property
hasRelation _ [] = property True
hasRelation _ [_] = property True
hasRelation relation (a : b : rest )
    | relation a b = hasRelation relation (b : rest)
    | otherwise = property False
