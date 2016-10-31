module TestEval (testEval
) where
import Control.Monad.Writer
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe
import Data.List

import TypeCheck
import BruijnTerm
import BruijnEnvironment
import qualified ExampleBruijn as B
import TestUtils
import ArbitraryLambda

import Debug
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
    -- , testLazyeval
    , testEvalBasic
    , testEvalBuildin
    , testEvalLet
    , testEvalProp
    ]

testSubstituteEnv :: TestTree
testSubstituteEnv = testGroup "substituteEnv "
    [ testCase "[a/1.0,b/2.0,b/3.0] a = 1.0" $
        let env = bFromList [map Subst [double 1, double 2, double 3]]
        in substituteEnv env (bvar 1 ) @?= double 2

    , testCase "[a/1.0] \\b.a = \\b.1.0" $
        let env = bFromList [[Subst $ double 1.0]]
        in substituteEnv env (lambda "b" $ bvar 1 ) @?= lambda "b" (double 1.0)

    , testCase "[a/c] \\b.a = \\b.c" $
        let env = bFromList [[Subst $ bvar 1]]
        in substituteEnv env (lambda "b" $ bvar 1 ) @?= lambda "b" (bvar 2)

    , testCase "[s a/1.0, k b/2.0,] b = b" $
        let env = bFromList [[Subst $ double 1.0],[ Keep $double 2]]
        in substituteEnv env (bvar 1)  @?= bvar 0

    , testCase "[k a/1.0, s b/2.0, k c/3] abc = a2b" $
        let env = bFromList [[Keep $ double 1.0, Subst $double 2, Keep $ double 3]]
        in substituteEnv env (appl (bvar 0) $ appl (bvar 1) (bvar 2))  @?= appl (bvar 0) ( appl (double 2) (bvar 1))
    -- , testCase "" $ --TODO check
    --     let env = bFromList [[ Subst $double 1, Subst $ bvar  0]]
    --     in substituteEnv env (bvar 1) @?= bvar 1

  , testCase "[s b/false, k c/true] \\a.c== \\a.c" $
        let env = bFromList [[Subst false],[Keep true]]
        in substituteEnv env (lambda "c" $ bvar 2) @?= (lambda "c" $ bvar 1)
    ]

testUpdateEnv :: TestTree
testUpdateEnv = testGroup "updateEnv"
    [ testCase "update a [a/1.0] = {}"  $
        let env = bFromList [[Keep $ double 1.0]]
        in updateEnv (Bound 0) env @?= return env

    -- , testCase "update a [a/a] = {}"  $
    --     let env = bFromList [bvar 0]
    --     in take 5 ( toList (updateEnv (Bound 0) env )) @?= replicate 5 env

    , testCase "update a [a/b ,b/1.0] = [a/1.0,b/1,0]"  $
        let env = bFromList [map Keep [bvar 1 , double 1]]
        in updateEnv (Bound 0) env @?= produce (bFromList [map Keep [double 1, double 1]])

    -- , testCase "update a [a/1.0+a] = {[a/1.0,],}"  $
    --     let env = bFromList [appl (appl (val plus) (double 1))(bvar 0)]
    --     in take 2 ( toList  (updateEnv (Bound 0) env)) @?= []
    , testCase "update a [a/((\\x.b)true),b/c,c=false]" $
        updateEnv (Bound 0) (bFromList [[Keep $ appl (lambda "x" $ bvar 2) true, Keep $ bvar 2, Keep false]])
        @?= do tell [ bFromList [[Keep $ bvar 1, Keep $ bvar 2, Keep false]]
                    , bFromList [[Keep $ bvar 1, Keep false   , Keep false]]
                    ]
               produce  $ bFromList [[Keep false, Keep false, Keep false]]
    ]

-- testLazyeval :: TestTree
-- testLazyeval = testGroup "lazy"
--   [ testCase "deref" $
--         (snd $ runWriter ((tell [(true,bEmtyEnv)]>> return undefined) >>= deref))
--         @?= [(true,bEmtyEnv)]
--
--   , testCase "retell" $
--       head (snd $ runWriter $ retell (map (+1))(tell [1:: Int,undefined] >> tell [undefined] >> return undefined))
--       @?= 2
--   ]

testEvalBasic :: TestTree
testEvalBasic = testGroup "basic"
  [ testCase "eval 1.0" $
        eval (double 1) @?= Nothing

  , testCase "eval id" $
        eval B.id @?= Nothing

  , testCase "id true" $
        eval (appl B.id true) @?= Just true

  , testCase "eval id(id(\\z.id z))=id(\\z.id z)" $

      take 1 ( evalSteps  (appl B.id $ appl B.id $ lambda "z" $ appl B.id $ bvar 0)) @?=
         [ appl B.id $ lambda "z" $ appl B.id $bvar 0]

  , testCase "omega omega" $
      eval (appl B.omega B.omega) @?= Just (appl B.omega B.omega)

  , testCase "eval id id = Just id" $
       evalSteps (appl B.id B.id) @?= return B.id
  , testCase "eval free" $
       eval (bvar 0) @?= Nothing

  , testCase "eval name capture" $
        eval (appl (lambda "b" $ lambda "c" $ bvar 1)(lambda "d" $ bvar 1))
        @?= Just (lambda "c" $ lambda "d" $ bvar 2)

  , testCase "call by vallu termination" $
      eval (lambda "z" (appl B.id (bvar 1))) @?= Nothing

  , testCase "(\\a. a True)\\b.1.0" $
      fullEval (appl (lambda "a" (appl (bvar 0) true))(lambda "b" $ double 1.0) ) @?= double 1

  , testCase "(\\b.(\\a.a)b)1.0" $
      fullEval (appl (lambda "b" (appl B.id (bvar 0)))(double 1.0) ) @?= double 1

  , testCase "(\\b.(\\a.a)b)1.0" $
      fullEval (appl (lambda "b" (appl B.id (bvar 0)))(double 1.0) ) @?= double 1

  , testCase "(\\b.id id b)1.0" $
      evalSteps (appl (lambda "b" (appl (appl B.id  B.id) (bvar 0)))(double 1))
      @?= [appl (appl B.id B.id) (double 1)
          ,appl B.id (double 1)
          ,double 1
          ]

  , testCase "(\\a.a(a true)) \\b.false" $
    evalSteps (appl (lambda "a" $ appl (bvar 0)(appl (bvar 0) true))(lambda "b" false))
    @?= [appl (lambda "b" false) (appl (lambda "b" false) true )
        ,appl (lambda "b" false) false
        ,false]

  , testCase "(\\a.a(a true)) \\b.c" $
    evalSteps (appl (lambda "a" $ appl (bvar 0)(appl (bvar 0) true))(lambda "b" $ bvar 1))
    @?= [appl (lambda "b" $bvar 1) (appl (lambda "b"$ bvar 1) true )
        ,appl (lambda "b" $bvar 1) $ bvar 0
        ,bvar 0]

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
      @?= [lambda "b" $ mkLet [("a", double 1.0) ] $ bvar 0]


  , testCase "eval let a = 1.0 + 2.0 in a" $
      evalSteps (mkLet [("a", appl (appl (val plus) (double 1))(double 2))] (bvar 0))
      @?= [mkLet [("a", appl (val $ applyValue plus (Prim $ MyDouble 1)) (double 2.0))] $ bvar 0
          ,mkLet [("a", double 3)] $ bvar 0
          ,mkLet [("a", double 3)] $ double 3
          ,double 3.0]

    , testCase "fullEval let a = let b = c in b; c = 1 in a" $
          fullEval (mkLet [("a", mkLet [("b", bvar 1)] $ bvar 0), ("c", double 1)] $ bvar 1)
          @?= double 1

  -- , testCase "evalWithEnv [a/1.0] a = []" $
  --       evalWithEnv (bFromList [[Keep $ double 1]] )( bvar 0)
  --       @?= return (double 1, bFromList [[Keep $ double 1]] )
  --
  -- , testCase "evalWithEnv [a/b,b/1.0] a = ([a/1.0,b/1.0],a)" $
  --       evalWithEnv (bFromList [map Keep [bvar 1,double 1]] )( bvar 0)
  --       @?= writer ((double 1,bFromList [map Keep [double 1, double 1]]), [(bvar 0, bFromList [map Keep [double 1, double 1]])] )
  --
  -- , testCase "evalWithEnv [5] let a = 1 in a == (1,[5])" $
  --       evalWithEnv (bFromList [[Subst $ double 5]]) (mkLet [("a",double 1)] $bvar 0)
  --       @?= produce (double 1,bFromList [[Keep $ double 1],[Subst $ double 5]] )

  , testCase "evalSteps let a = b ;b=1.0 in a " $
      evalSteps (mkLet [("a", bvar 0), ("b", double 1.0)] (bvar 1))
      @?= [ mkLet [("a", double 1.0), ("b", double 1.0)] $ bvar 1
          , mkLet [("a", double 1.0), ("b", double 1.0)] $ double 1
          ,double 1]

  , testCase "fullEval let a = \\b.b in a " $
      fullEval (mkLet [("a", B.id) ] (bvar 0))
      @?= lambda "a" (mkLet [("a", B.id) ] (bvar 1))

  , testCase "fullEval let a = \\b.1.0 in \\c.a " $
      fullEval (mkLet [("a",lambda "b" (double 1))] $ lambda "c" $ bvar 1)
       @?= lambda "c" ( mkLet [("a",lambda "b" (double 1)) ] $ bvar 0)

  , testCase "evalSteps let a = \\c.b ;b=1.0 in a " $
      evalSteps        (mkLet [("a", lambda "c"(bvar 1)), ("b", double 1.0)] $ bvar 1)
      @?= [ mkLet [("a", lambda "c"(bvar 1)), ("b", double 1.0)] $ lambda "c" $ bvar 1
          , lambda "c" $ mkLet [("a", lambda "c"(bvar 1)), ("b", double 1.0)] $ bvar 0]

  , testCase "fullEval let a = let b = 1.0 in b ;in a " $
      evalSteps (mkLet [("a", mkLet [("b", double 1)] (bvar 0))] (bvar 0))
      @?= [ mkLet [("a", mkLet [("b",double 1)] $ double 1)] $ bvar 0
          ,  mkLet [("a", double 1)] $ bvar 0
          ,  mkLet [("a", double 1)] $ double 1
          , double 1]

  , testCase "fullEval let a = 1 + in  a" $
        evalSteps (mkLet [("a",appl (val plus) (double 1))] $ bvar 0)
        @?= [ mkLet [("a",val $ applyValue plus (Prim $MyDouble 1))]  $ bvar 0
            , mkLet [("a",val $ applyValue plus (Prim $MyDouble 1))]  $ val $ applyValue plus (Prim $MyDouble 1)
            , val $ applyValue plus (Prim $MyDouble 1)
            ]


  , testCase "evalSteps let a = 1 + in let b = 2 in a" $
        evalSteps (mkLet [("a",appl (val plus) (double 1))] $ mkLet [("b",double 2)] $ bvar 1)
        @?= [ mkLet [("a",val $ applyValue plus (Prim $MyDouble 1))] $ mkLet [("b",double 2)] $ bvar 1
            , mkLet [("a",val $ applyValue plus (Prim $MyDouble 1))] $ mkLet [("b",double 2)] $ val $ applyValue plus (Prim $MyDouble 1) --TODO CHECk

            , mkLet [("a",val $ applyValue plus (Prim $MyDouble 1))] $ val $ applyValue plus (Prim $MyDouble 1) --TODO CHECk
            , val $ applyValue plus (Prim $MyDouble 1)]

  , testCase "fullEval (let a = 1.0) ;in \\b.a+b) 2.0 " $
      fullEval (appl (mkLet [("a", double 1)]
                            (lambda "b" $ appl (appl (val plus)(bvar 1))
                                             (bvar 0)))
                     (double 2))
      @?= double 3

  , testCase "let a = \\b.a; in a " $
      evalSteps ( mkLet [("a",lambda "b" (bvar 1))] (bvar 0))
      @?= [ mkLet [("a",lambda "b" (bvar 1))] $ lambda "b" $ bvar 1
          , lambda "b" ( mkLet [("a",lambda "b"(bvar 1))] $ bvar 0)]

  , testCase "(let a = \\b.a; in a) true " $
      fullEval (appl ( mkLet [("a",lambda "b" (bvar 1))] (bvar 0)) true)
      @?= lambda "b" ( mkLet [("a",lambda "b"(bvar 1))] $ bvar 0)


  , testCase "let a = (\\b.c) false;in a" $
      evalSteps ( mkLet [("a",appl (lambda "b" (bvar 2)) false)] (bvar 0))
      @?= [ mkLet [("a",bvar 1)] $ bvar 0
          , mkLet [("a",bvar 1)] $ bvar 1
          ]

  , testCase "(let a = (\\b.c) false;c = True in a)" $
      evalSteps ( mkLet [("a",appl (lambda "b" (bvar 1)) false),("c",true)] (bvar 1))
      @?= [ mkLet [("a",bvar 0),("c",true)] $ bvar 1
          , mkLet [("a",true),("c",true)] $ bvar 1
          , mkLet [("a",true),("c",true)] true
          , true]

  , testCase "let a = id; b = a in b" $
      evalSteps ( mkLet [("a",B.id),("b",bvar 1)] $ bvar 0)
      @?= [ mkLet [("a",B.id),("b",B.id)] $ bvar 0
          , mkLet [("a",B.id),("b",B.id)]  B.id
          , lambda "a" $mkLet [("a",B.id),("b",B.id)] $ bvar 2]

  , testCase "(\\b.let a = b in false)true" $
      evalSteps  (appl (lambda "b" $ mkLet [("a",bvar 1)]false)true)
      @?= [mkLet [("a",true)] false,false]

  , testCase "(\\b.let a = b in a)true" $
      evalSteps  (appl (lambda "b" $ mkLet [("a",bvar 1)] $ bvar 0) true)
      @?= [ mkLet [("a",true)] $ bvar 0
          , mkLet [("a",true)] $ true
          , true
          ]

  , testCase "(\\b.let a = b in b)true" $
      evalSteps  (appl (lambda "b" $ mkLet [("a",bvar 1)] $ bvar 1) true)
      @?= [mkLet [("a",true)]  true,true]


  , testCase "let a = 1 in let b = true ; c = b in false  " $
      evalSteps  (mkLet [("a",double 1)] $ mkLet [("b",true),("c",bvar 1)] false )
      @?= [mkLet [("a",double 1)] false ,false]

  , testCase "let x = 1 in (\\y.x)true" $
       evalSteps (mkLet [("x",double 1)] $ appl (lambda "y" $ bvar 1) true )
       @?= [ mkLet [("x",double 1)] $ bvar 0
           , mkLet [("x",double 1)] $ double 1
           , double 1]

  , testCase "let x = 1 + in (\\y.x)true" $
       evalSteps (mkLet [("x",appl (val plus) (double 1))] $ appl (lambda "y" $ bvar 1) true )
       @?= [ mkLet [("x",appl (val plus) (double 1))] $  bvar 0
           , mkLet [("x",val (applyValue plus $ Prim $ MyDouble 1))] $ bvar 0
           , mkLet [("x",val (applyValue plus $ Prim $ MyDouble 1))] $ val (applyValue plus (Prim $ MyDouble 1))
           , val (applyValue plus (Prim $ MyDouble 1)) ]

  , testCase "let x = y;y=1  in (\\a.x)true" $
        evalSteps (mkLet [("x",bvar 0),("y",double 1)] $ appl (lambda "a" $ bvar 2) true)
        @?= [ mkLet [("x",bvar 0),("y",double 1)] $ bvar 1
            , mkLet [("x", double 1),("y",double 1)] $ bvar 1
            , mkLet [("x", double 1),("y",double 1)] $ double 1
            , double 1
            ]

  , testCase "let f = \\a.f 1 in f 2 " $
        take 4 (evalSteps (mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $  appl (bvar 0) (double 2)))
        @?=  [ mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $ appl (lambda "a" $ appl (bvar 1) (double 1) ) (double 2)
             , mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $ appl (bvar 0) (double 1)
             , mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $ appl (lambda "a" $ appl (bvar 1) (double 1) ) (double 1)
             , mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $ appl (bvar 0) (double 1)
             ]

  , testCase "let f = \\a.f 1 in let g = f in f 2 " $
        take 2 (evalSteps (
            mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $
            mkLet [("g",bvar 1)] $
            appl (bvar 0) (double 2)))
        @?= [ mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $
                mkLet [("g", lambda "a" $ appl (bvar 2) (double 1))] $
                appl (bvar 0) (double 2)

            , mkLet [("f",lambda "a" $ appl (bvar 1)(double 1))] $
                mkLet [("g", lambda "a" $ appl (bvar 2) (double 1))] $
                appl (lambda "a" $ appl (bvar 2) (double 1)) (double 2)
            ]

  , testCase "let f = \\a.id 1;id = id in f 2 " $
        evalSteps (mkLet [("f",lambda "a" $ appl (bvar 1)(double 1)),("id",B.id)] $  appl (bvar 1) (double 2))
        @?= [ mkLet [("f",lambda "a" $ appl (bvar 1)(double 1)),("id",B.id)] $ appl (lambda "a" $ appl (bvar 1)(double 1))(double 2)
            , mkLet [("f",lambda "a" $ appl (bvar 1)(double 1)),("id",B.id)] $ appl (bvar 0)(double 1)
            , mkLet [("f",lambda "a" $ appl (bvar 1)(double 1)),("id",B.id)] $ appl (lambda "a" $ bvar 0)(double 1)
            , mkLet [("f",lambda "a" $ appl (bvar 1)(double 1)),("id",B.id)] $ double 1
            , double 1
            ]

  , testCase "let a = true; b = a in let c = a in b" $
        evalSteps (mkLet [("a",true),("b",bvar 1)] $ mkLet [("c",bvar 2)] $ bvar 1 )
             @?= [ mkLet [("a",true),("b",true  )] $ mkLet [("c",bvar 2)] $ bvar 1
                 , mkLet [("a",true),("b",true  )] $ mkLet [("c",bvar 2)] $ true
                 , mkLet [("a",true),("b",true  )] true
                 , true
                 ]
  --           ]
  , testCase "let a = b; b = true in let c = a in c" $
        evalSteps (mkLet [("a", bvar 0), ("b",true)] $ mkLet [("c",bvar 2)] $ bvar 0)
        @?= [ mkLet [("a", true), ("b",true)] $ mkLet [("c",bvar 2)] $ bvar 0
            , mkLet [("a", true), ("b",true)] $ mkLet [("c",true)] $ bvar 0
            , mkLet [("a", true), ("b",true)] $ mkLet [("c",true)] $ true
            , mkLet [("a", true), ("b",true)]  true
            , true
            ]

  , testCase "(\\a. let f b = a in f) true" $
        evalSteps (appl (lambda "a" $mkLet [("f",lambda "b" $ bvar 2)] $ bvar 0) true)
        @?= [ mkLet [("f",lambda "b" true)] $ bvar 0
            , mkLet [("f",lambda "b" true)] $ lambda "b" true
            , lambda "b" $ mkLet [("f",lambda "b" true)] true
            ]

  , testCase "let f a = a true 1 in let b = 2+ in f (\\c. b)" $
        fullEval ( mkLet [("f",lambda "a" $ appl (appl (bvar 0) true) (double 1))] $
                   mkLet [("b", appl (val plus) (double 2))] $
                   appl (bvar 1) (lambda "c" $ bvar 1 ) )
        @?= double 3

  , testCase "let a = true in (\\b c.a) (false)" $
        evalSteps (mkLet [("a", true)] $ appl (lambda "b"$ lambda "c" $ bvar 2) false)
        @?= [ mkLet [("a", true)] $ lambda "c" $ bvar 1
            , lambda "c" $ mkLet [("a", true)] $ bvar 0]

  , testCase "let f a = b; b = true in let c = f in c" $
        evalSteps (mkLet [("f",lambda "a" $ bvar 1),("b",true)] $ mkLet [("c",bvar 2)] $ bvar 0 )
        @?= [ mkLet [("f",lambda "a" $ bvar 1),("b",true)] $ mkLet [("c",lambda "a" $ bvar 2)] $ bvar 0
            , mkLet [("f",lambda "a" $ bvar 1),("b",true)] $ mkLet [("c",lambda "a" $ bvar 2)] $ lambda "a" $ bvar 2
            , mkLet [("f",lambda "a" $ bvar 1),("b",true)] $ lambda "a" $ mkLet [("c",lambda "a" $ bvar 3)] $ bvar 2
            , lambda "a" $ mkLet [("f",lambda "a" $ bvar 1),("b",true)] $  mkLet [("c",lambda "a" $ bvar 2)] $ bvar 1

            ]

  , testCase "let id = id; f b = id in (let x = True in f)id"$
        evalSteps (mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ appl (mkLet [("x",true)] $ bvar 1) (bvar 1))
        @?= [ mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ appl (mkLet [("x",true)] $ bvar 1) B.id
            , mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ appl (mkLet [("x",true)] $ lambda "b" $ bvar 3) B.id
            , mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ appl (lambda "b" $ mkLet [("x",true)] $ bvar 3) B.id

            , mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ mkLet [("x",true)] $ bvar 2

            , mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ mkLet [("x",true)] $ lambda "a"  $ bvar 0

            , mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ lambda "a" $ mkLet [("x",true)] $ bvar 1
            , lambda "a" $ mkLet [("id",B.id),("f",lambda "b" $ bvar 2)] $ mkLet [("x",true)] $ bvar 3
            ]

  ]
testEvalProp :: TestTree
testEvalProp = testGroup "propertys" $
  let forAllNonCiculair prop = forAllTypedBruijn $ \e -> not (isCirculair e) ==> prop e
  in
  [ testProperty "evalSteps == unfold eval" $ forAllNonCiculair$ \e ->
        take 10 (evalSteps e) ===  take 10 (unfoldr (\e0 -> let nextE = eval e0
                                                                clone a = (a,a)
                                                            in fmap clone nextE)  e)


  -- , testProperty "everystep a change" $ forAllNonCiculair  $
  --       hasRelation (/=) . take 10 . evalSteps

  , testProperty "welformd presevation eval" $ forAllNonCiculair $
        all welFormd . take 10 . evalSteps

  , testProperty "keep normalisation under eval" $ forAllNonCiculair $
        all normalised . take 10 . evalSteps

  , testProperty "keep type under eval" $ forAllNonCiculair $ \ e ->
        let result = eval e
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


-- , testCase "[a/\\c.b,b/1,0] a = \\c.1.0" $
    --     let env = bFromList [lambda "c" (bvar 1),double 1]
    --     in substituteEnv env  (bvar 1)  @?= lambda "c" (double 1.0)

    -- , testCase "[a/b.b/c] a = a" $
    --     let env = bFromList [bvar 0,bvar 2]
    --     in substituteEnv env  (bvar 1)  @?= bvar 2

  -- , testCase "eval let a = a in a" $
  --     eval (mkLet [("a",bvar 0)] $ bvar 0)
  --     @?=  Just (mkLet [("a",bvar 0)] $ bvar 0)
  --
  -- , testCase "evalSteps let a = a 1.0 in a" $
  --     eval (mkLet [("a", appl (bvar 0) (double 1.0)) ] $ bvar 0)
  --     @?= Nothing
  -- , testCase "fullEva let a = let b = a in b; c = 1 in a" $
  --     evalSteps (mkLet [("a", mkLet [("b", bvar 2)] $ bvar 0), ("c", double 1)] $ bvar 1)
  --     @?= []
  --
  -- , testCase "(let a = (\\b.a) false; in a)" $
  --     evalSteps ( mkLet [("a",appl (lambda "b" (bvar 1)) false)] (bvar 0))
  --     @?= [mkLet [("a",bvar 0)] (bvar 0)]
  --
  -- , testCase "(let a = (\\b.a) false; in a) true " $
  --     evalSteps (appl ( mkLet [("a",appl (lambda "b" (bvar 1)) false)] (bvar 0)) true)
  --     @?= [lambda "b" ( mkLet [("a",true),("c",true)] (bvar 0)),true]
  --
  -- , testCase "(let f = \a.f a in f) True" $ -- TODO add
  --       take 4 ( evalSteps (appl (mkLet [("f",lambda "a" $ appl (bvar 1)(bvar 0))] $ bvar 0) true  ))
  --       @?= [ appl (lambda "a" $ mkLet [("f",appl (bvar 1)(bvar 0))] $ bvar 0) true
  --           , mkLet [("f",appl (bvar 1)(bvar 0))] $ appl (bvar 0) true
  --           , mkLet [("f",appl (bvar 1)(bvar 0))] $ appl (lambda "a" $ appl (bvar 1) (bvar 0) ) true
  --           , mkLet [("f",appl (bvar 1)(bvar 0))] $ appl (bvar 0) true
  --
  --
  -- , testCase "let a = 1+a; in a " $
  --     head ( evalSteps (mkLet [("a",appl (appl (val plus) (double 1)) (bvar 0))] (bvar 0)))
  --     @?= double 1.0
