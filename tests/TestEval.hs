module TestEval
    ( testEval
    ) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Data.Maybe
import Data.Either
import Data.List

import TypeCheck
import BruijnTerm
import qualified ExampleBruijn as B
import Properties
import ArbitraryLambda

import TopologicalSort
import Eval hiding (Free)
import qualified Eval
import FreeEnvironment
import MakeTerm
import ModificationTags (applyModify)
import qualified Operator
import Value
import Name
import TestUtils
import qualified Type as T
import qualified SimpleEval as Simple
import Jit
-- import qualified ANormalForm as ANorm -- TODO remove
-- import qualified Data.ByteString.Char8 as ByteString
-- import LLVM.Analysis
-- import LLVM.Module
-- import CodeGen
-- TODO test with free variables

testEval :: TestTree
testEval = testGroup "eval"
    [ testFullEval
    , testEvalUtils
    , testEvalSteps
    , testEvalJit
    ]

-- TODO remove or make nices/faster
genD1 :: Gen D1
genD1= sized go
  where
    gen1s = [DVal <$> elements ((Prim $ MyDouble 1): (Prim $ MyBool True): map Func Operator.operators)
            ,Eval.Free . Bound <$>choose (100000,100010) ]
    genNames = Name . return <$> choose ('a','z')
    go n | n <= 1 = oneof gen1s
         | n <= 4 = oneof $ (Closure [] <$> genNames <*> (genTyped  defaultConf)):gen1s
         | otherwise = do
      nLets <- choose (1, min n 3)
      sizeLets<- uniformBucket nLets (n-1)
      defs <- mapM (\sizeLet-> do
          nDefs <- max 1 <$> choose (1,min sizeLet 3)
          sizeDefs <- uniformBucket (nDefs::Int) sizeLet
          mapM (\sizeDef -> resize sizeDef arbitraryDefs) sizeDefs
          ) sizeLets
      Closure defs <$> genNames <*> genUnTyped

    arbitraryDefs :: Gen (Def () D1)
    arbitraryDefs = def  <$> (return <$> elements ['a' ..'f']) <*> scale subtract1To0 genD1

    subtract1To0 :: Int -> Int
    subtract1To0 n | n > 0 = n - 1
                   | otherwise = 0

testEvalUtils :: TestTree
testEvalUtils = testGroup "eval utils"

  [ testProperty "d1ToBruijnl (incFreeD1 5 d1) == incFreeOfset 5 (d1ToBruijn d1)" $
      forAll genD1 $ \d1 ->
      d1ToBruijn (incFreeD1 5 (d1::D1)) === incFree 5 (d1ToBruijn d1)

  , testProperty "test GenD1" $
      forAll genD1 $ \d1 ->
      case d1 of
        (Closure defs _ _) -> all (\d -> 0 <length d )defs
        _ -> True
  , testCase "incFreeD1 5 (D1 [[01234,1234][01234,01234]] 01234)" $
    let app1234 = bvar 0 `appl`  bvar 1 `appl` bvar 2 `appl`  bvar 3 `appl` bvar 4
        d1app1234 = Closure [] (Name "a") app1234
        d1 = Closure [[def "a" d1app1234, def "b" d1app1234],[def "c" d1app1234,def "d" d1app1234]] (Name "a")app1234
    in d1ToBruijn (incFreeD1 5 d1 ) @?= incFree 5 ( d1ToBruijn d1)

  ]

testEvalJit :: TestTree
testEvalJit = testGroup "JIT"
    [ testGroup "basic" (map testFullEvalExample $ mkFullEvalCompatable basicSet)
    , testGroup "buildin" (map testFullEvalExample $ mkFullEvalCompatable buildinSet)
    , testGroup "let" (map testFullEvalExample $ mkFullEvalCompatable letSet)
    ]
  where
    testFullEvalExample :: (BruijnTerm() (), BruijnTerm() ()) -> TestTree
    testFullEvalExample (input,expected) = testCase (removeNewLines $ pShow input) $ do
        result <- evalWithJit input
        val (Prim result) @?= expected

mkFullEvalCompatable :: [(BruijnTerm() (), [BruijnTerm() ()])] -> [(BruijnTerm() (),BruijnTerm() ())]
mkFullEvalCompatable set = filter isValue $ map (\(input, expected) -> (input,last $input:expected)) set
  where
    isValue (_,t) = case t of
      Val {} -> True
      _ -> False

testFullEval :: TestTree
testFullEval = testGroup "full eval"
    [ testGroup "basic" (map testFullEvalExample $ mkFullEvalCompatable basicSet)
    , testGroup "buildin" (map testFullEvalExample $ mkFullEvalCompatable buildinSet)
    , testGroup "let" (map testFullEvalExample $ mkFullEvalCompatable letSet)
    ]
  where
    testFullEvalExample :: (BruijnTerm() () ,BruijnTerm() () ) -> TestTree
    testFullEvalExample (input,expected) =  testCase (removeNewLines $ pShow input) $ Simple.fullEval' input @?= expected

testEvalSteps :: TestTree
testEvalSteps = testGroup "evalSteps"
    [ testGroup "basic" (map testEvalStepsExample basicSet)
    , testGroup "buildin" (map testEvalStepsExample buildinSet)
    , testGroup "let" (map testEvalStepsExample letSet)
    , testGroup "free" (map testEvalStepsExample freeSet )
    , testconvergentSteps
    , testEvalProp
    ]
  where
    testEvalStepsExample :: (BruijnTerm() () ,[BruijnTerm() ()]) -> TestTree
    testEvalStepsExample (input,expected) = testCase (removeNewLines $ pShow input) $
        evalSteps input @?= expected

testconvergentSteps :: TestTree
testconvergentSteps = testGroup "convergent" $ map test convergingSet
  where
    test (input,expected) = testCase (removeNewLines $ pShow input) $
      (take (length expected) $ evalSteps input) @?= expected

basicSet :: [(BruijnTerm () () , [BruijnTerm () ()])]
basicSet =
  [ ( double 1, [])

  , ( B.id, [])

  , ( appl B.id true, [true])

  , ( appl B.id ( appl B.id (lambda "z" (appl B.id (bvar 0))))
    , [appl B.id ( lambda "z" (appl B.id (bvar 0)))
      ,lambda "z" (appl B.id (bvar 0)) ])

  , (appl B.id B.id, [B.id])

  , (appl (lambda "a" (appl (bvar 0) true)) (lambda "b" $ double 1.0)
      , [ appl (lambda "b" $ double 1) true
        , double 1])

  , (appl (lambda "b" (appl B.id (bvar 0))) (double 1.0)
      , [ appl B.id (double 1)
        , double 1])

  , (appl (lambda "b" (appl (appl B.id B.id) (bvar 0))) (double 1)
      , [ appl (appl B.id B.id) (double 1)
        , appl B.id (double 1)
        , double 1
        ])

  , (appl (lambda "a" $ appl (bvar 0) (appl (bvar 0) true)) (lambda "b" false)
      , [ appl (lambda "b" false) (appl (lambda "b" false) true )
        , appl (lambda "b" false) false
        , false
        ])

  , (appl (lambda "a" $ appl (bvar 0) (appl (bvar 0) true)) (lambda "b" $ bvar 1)
      , [ appl (lambda "b" $ bvar 1) (appl (lambda "b" $ bvar 1) true )
        , appl (lambda "b" $ bvar 1) $ bvar 0
        , bvar 0
        ])

  , (appl (appl (lambda "a" $ lambda "b" $ bvar 1 ) true) false
      , [ appl (lambda "b" true ) false
        , true
        ])

  , (appl (lambda "a" $ appl (lambda "b" $bvar 1 )(bvar 0)) true
      , [ appl (lambda "b" true )true
        , true
        ])
  ]

buildinSet :: [(BruijnTerm () () , [BruijnTerm () ()])]
buildinSet =
  [ (lambda "#" (appl (appl plus (bvar 0)) (double 1.0)) ,[])
  , (appl plus (double 1.0), [plus1])
  , (appl (appl plus ( double 1.0)) (double 2.0)
      , [appl plus1 (double 2)
      , double 3])
  , (appl (appl plus
                           (double 1.0))
                           (appl (appl multiply
                                       (double 2.0))
                                       (double 3.0))

      , [ appl (appl plus (double 1)) (appl (val $ applyValue (Func Operator.multiply) (Prim $ MyDouble 2)) (double 3))
        , appl (appl plus (double 1)) (double 6)
        , appl plus1 (double 6)
        , double 7
        ])
  ]

freeSet :: [(BruijnTerm () () , [BruijnTerm () ()])]
freeSet =
  [ (bvar 0, [])

  , (appl (lambda "b" $ lambda "c" $ bvar 1) (lambda "d" $ bvar 1)
      , [lambda "c" $ lambda "d" $ bvar 2])

  , (lambda "z" (appl B.id (bvar 1)), [])


  , (mkLet [("a", double 1.0) ] (bvar 1)
      , [bvar 0])
  ]

plus1 :: BruijnTerm () ()
plus1 = val $ applyValue (Func Operator.plus) (Prim $ MyDouble 1)

-- TODO  sort and cleanup test description
letSet ::  [(BruijnTerm () () , [BruijnTerm () ()])]
letSet =
  [ (mkLet [("a", double 1) ] (bvar 0)
      , [ mkLet [("a", double 1) ] (double 1)
        , double 1.0])

  , (mkLet [("a", double 1) ] $ lambda "b" $ bvar 1
      , [])

  , (mkLet [("a", appl (appl plus (double 1)) (double 2))] (bvar 0)
      , [ mkLet [("a", appl plus1 (double 2))] $ bvar 0
        , mkLet [("a", double 3)] $ bvar 0
        , mkLet [("a", double 3)] $ double 3
        , double 3.0])

  , (mkLet [("c", double 1), ("a", mkLet [("b", bvar 2)] $ bvar 0)] $ bvar 1
      , [ mkLet [("c", double 1), ("a", mkLet [("b", double 1 )] $ bvar 0)] $ bvar 1
        , mkLet [("c", double 1), ("a", mkLet [("b", double 1 )] $ double 1)] $ bvar 1
        , mkLet [("c", double 1), ("a", double 1)] $ bvar 1
        , mkLet [("c", double 1), ("a", double 1)] $ double 1
        , double 1])

  ,(mkLet [("b", double 1.0), ("a", bvar 1)] (bvar 0)
      , [ mkLet [("b", double 1.0), ("a", double 1.0)] $ bvar 0
        , mkLet [("b", double 1.0), ("a", double 1.0)] $ double 1
        , double 1])


  ,(mkLet [("a", true), ("b", bvar 2), ("c", false)] true
      , [ mkLet [("a", true), ("b", true), ("c", false)] true
        , true])

  ,(mkLet [("a", true), ("b", bvar 2), ("c",bvar 1)] true
    , [ mkLet [("a", true), ("b", true), ("c", bvar 1)] true
      , mkLet [("a", true), ("b", true), ("c", true)] true
      , true])

  , (mkLet [("a", true), ("b", false), ("c", bvar 2)] true
      , [ mkLet [ ("a", true), ("b", false), ("c", true)] true
        , true])

  , (mkLet [("a", B.id) ] (bvar 0)
      , [ mkLet [("a", B.id)] B.id])

  , (mkLet [("a", lambda "b" (double 1)) ] $ lambda "c" (bvar 1)
       , [])

  , (mkLet [("a", lambda "c" $ bvar 1), ("b", double 1.0)] (bvar 1)
      , [ mkLet [("a", lambda "c" $ bvar 1), ("b", double 1.0)] $ lambda "c" $ bvar 1 ])

  , ( mkLet [("a", mkLet [("b", double 1)] (bvar 0))] (bvar 0)
      , [ mkLet [("a", mkLet [("b", double 1)] $ double 1)] (bvar 0)
        , mkLet [("a", double 1)] (bvar 0)
        , mkLet [("a", double 1)] (double 1)
        , double 1.0
        ])

  , ( mkLet [("a", appl plus (double 1))] $ bvar 0
        , [ mkLet [("a", plus1)] $ bvar 0
          , mkLet [("a", plus1)] $ plus1
          , plus1])

  , ( mkLet [("a", appl plus (double 1))] $ appl (bvar 0) (double 2)
        , [ mkLet [("a", plus1)] $ appl (bvar 0) (double 2)
          , mkLet [("a", plus1)] $ appl plus1 (double 2)
          , mkLet [("a", plus1)] $ double 3
          , double 3])

  , ( mkLet [("a", appl plus (double 1))] $ mkLet [("b", double 2)] $ bvar 1
        , [ mkLet [("a", plus1)] $ mkLet [("b", double 2)] $ bvar 1
          , mkLet [("a", plus1)] $ mkLet [("b", double 2)] plus1
          , mkLet [("a", plus1)] plus1
          , plus1])

  , (appl (mkLet [("a", double 1)]
                          (lambda "b" $ appl (appl plus (bvar 1))
                                             (bvar 0)))
                     (double 2)
      , [ mkLet [("a", double 1)] (appl (appl plus (bvar 0)) (double 2))
        , mkLet [("a", double 1)] (appl (appl plus (double 1)) (double 2))
        , mkLet [("a", double 1)] (appl plus1 (double 2))
        , mkLet [("a", double 1)] $ double 3
        , double 3])

  , (appl (mkLet [("b", double 1)]
                            (lambda "a" $ appl (appl plus (bvar 0))
                                             (bvar 1)))
                     (double 2)
      , [ mkLet [("b", double 1)] $ appl (appl plus (double 2)) (bvar 0)
        , mkLet [("b", double 1)] $ appl (appl plus (double 2)) (double 1)
        , mkLet [("b", double 1)] $ appl (val $ applyValue (Func Operator.plus) (Prim $ MyDouble 2)) (double 1)
        , mkLet [("b", double 1)] $ double 3
        , double 3])

  , (mkLet [("b", true)] B.id
      ,[])

  , ( mkLet [("a", lambda "b" (bvar 1))] (bvar 0)
      , [ mkLet [("a", lambda "b" (bvar 1))] $ lambda "b" $ bvar 1])

  , ( appl ( mkLet [("a", false)] B.id) true
      , [mkLet [("a", false)] true, true ])

  , ( appl ( mkLet [("a", lambda "b" (bvar 1))] (bvar 0)) true
      , [ appl ( mkLet [("a", lambda "b" (bvar 1))] $ lambda "b" $ bvar 1) true
        , mkLet [("a", lambda "b" $ bvar 1)] $ bvar 0
        , mkLet [("a", lambda "b" $ bvar 1)] $ lambda "b" $ bvar 1
        ])

  , (appl ( mkLet [("a", lambda "b" (bvar 0))] (bvar 0)) true
      , [ appl ( mkLet [("a", lambda "b" (bvar 0))] $ lambda "b" $ bvar 0) true
        , mkLet [("a", lambda "b" $ bvar 0)] true
        , true
        ])

  , ( mkLet [("a", true), ("c", appl (lambda "b" $ bvar 2) false)] (bvar 0)
      , [ mkLet [("a", true), ("c", bvar 1)] $ bvar 0
        , mkLet [("a", true), ("c", true)] $ bvar 0
        , mkLet [("a", true), ("c", true)] true
        , true])

  , ( mkLet [("a", B.id), ("b", bvar 1)] $ bvar 0
      , [ mkLet [("a", B.id), ("b", B.id)] $ bvar 0
        , mkLet [("a", B.id), ("b", B.id)] B.id
        ])

  , ( appl (lambda "b" $ mkLet [("a", bvar 1)] false) true
      , [ mkLet [("a", true)] false
        , false])

  , ( appl (lambda "b" $ mkLet [("a", bvar 1)] $ bvar 0) true
      , [ mkLet [("a", true)] $ bvar 0
        , mkLet [("a", true)] true
        , true
        ])

  , ( appl (lambda "b" $ mkLet [("a", bvar 1)] $ bvar 1) true
      , [ mkLet [("a", true)] true
        , true])

  , (mkLet [("a", double 1)] $ mkLet [("b", true), ("c", bvar 1)] false
      , [ mkLet [("a", double 1)] $ mkLet [("b", true), ("c", true)] false
        , mkLet [("a", double 1)] false
        , false])

  , (mkLet [("x", double 1)] $ appl (lambda "y" $ bvar 1) true
       , [ mkLet [("x", double 1)] $ bvar 0
         , mkLet [("x", double 1)] $ double 1
         , double 1])

  , ( mkLet [("x", appl plus (double 1))] $ appl (lambda "y" $ bvar 1) true
       , [ mkLet [("x", plus1)] $ appl (lambda "y" $ bvar 1) true
         , mkLet [("x", plus1)] $ bvar 0
         , mkLet [("x", plus1)] plus1
         , plus1
         ])

  , (mkLet [("x", double 1), ("y", bvar 1)] $ appl (lambda "a" $ bvar 1) true
        , [ mkLet [("x", double 1), ("y", double 1)] $ appl (lambda "a" $ bvar 1) true
          , mkLet [("x", double 1), ("y", double 1)] $ bvar 0
          , mkLet [("x", double 1), ("y", double 1)] $ double 1
          , double 1
          ])

  , (mkLet [("f", lambda "a" $ appl (bvar 1) (double 1)), ("id", B.id)] $
                         appl (bvar 1) (double 2)
        , [ mkLet [("f", lambda "a" $ appl (bvar 1) (double 1)), ("id", B.id)] $
                  appl (lambda "a" $ appl (bvar 1) (double 1)) (double 2)
          , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1)), ("id", B.id)] $
                  appl (bvar 0) (double 1)
          , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1)), ("id", B.id)] $
                  appl (lambda "a" $ bvar 0) (double 1)
          , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1)), ("id", B.id)] $
                   double 1
          , double 1
          ])

  , (mkLet [("a", true), ("b", bvar 1)] $ mkLet [("c", bvar 1)] $ bvar 1
     , [ mkLet [("a", true), ("b", true )] $ mkLet [("c", bvar 1)] $ bvar 1
       , mkLet [("a", true), ("b", true )] $ mkLet [("c", true)] $ bvar 1
       , mkLet [("a", true), ("b", true )] $ mkLet [("c", true)] true
       , mkLet [("a", true), ("b", true )] true
       , true
       ])

  , ( mkLet [("g",true)] $ mkLet [("a",mkLet [("c",plus)] $ lambda "e"$ bvar 3 )]$ mkLet [("x", multiply)] $ appl (bvar 1) multiply
      , [mkLet [("g",true)] $ mkLet [("a",mkLet [("c",plus)] $ lambda "e" $ bvar 3 )] $ mkLet [("x",multiply)] $ appl (mkLet [("c",plus)] $ lambda "e" $ bvar 4) multiply
          ,mkLet [("g",true)] $ mkLet [("a",mkLet [("c",plus)] $ lambda "e" $ bvar 3 )] $ mkLet [("x", multiply)] $ mkLet [("c",plus)] $ bvar 3
          ,mkLet [("g",true)] $ mkLet [("a",mkLet [("c",plus)] $ lambda "e" $ bvar 3 )] $ mkLet [("x", multiply)] $ mkLet [("c",plus)] true
          ,mkLet [("g",true)] $ mkLet [("a",mkLet [("c",plus)] $ lambda "e" $ bvar 3 )] $ mkLet [("x", multiply)] true
          ,mkLet [("g",true)] $ mkLet [("a",mkLet [("c",plus)] $ lambda "e" $ bvar 3 )] true
          ,mkLet [("g",true)] true
          ,true
      ])

  , (appl (lambda "a" $ mkLet [("f", lambda "b" $ bvar 2)] $ bvar 0) true
        , [ mkLet [("f", lambda "b" true)] $ bvar 0
          , mkLet [("f", lambda "b" true)] $ lambda "b" true
          ])

  , --"let f a = a true 2 in let b = 1+ in f (\\c. b)" $
        ( mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
          mkLet [("b", appl plus (double 1))] $ appl (bvar 1) (lambda "c" $ bvar 1 )
        , [ mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
            mkLet [("b", plus1)] $ appl (bvar 1) (lambda "c" $ bvar 1 )

          , mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
            mkLet [("b", plus1)] $ appl (lambda "a" $ appl (appl (bvar 0) true) (double 2)) (lambda "c" $ bvar 1 )

          , mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
            mkLet [("b", plus1)] $ appl (appl (lambda "c" $ bvar 1 ) true) (double 2)

          , mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
            mkLet [("b", plus1)] $ appl (bvar 0 ) (double 2)

          , mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
            mkLet [("b", plus1)] $ appl plus1 (double 2)

          , mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
            mkLet [("b", plus1)] $ double 3

          , mkLet [("f", lambda "a" $ appl (appl (bvar 0) true) (double 2))] $
            double 3

          ,double 3])

  , (mkLet [("a", true)] $ appl (lambda "b" $ lambda "c" $ bvar 2) false
      ,[ mkLet [("a", true)] $ lambda "c" $ bvar 1])

  , (appl (mkLet [("b",multiply), ("a",bvar 1)] plus)(double 1)
      , [ appl (mkLet [("b",multiply), ("a",multiply)] plus)(double 1)
        , appl ( plus)(double 1)
        , plus1 ])

  , (mkLet [("id'",mkLet [("a",double 1)] $ lambda "b" $bvar 0)] $appl (bvar 0) true
      , [ mkLet [("id'",mkLet [("a",double 1)] $ lambda "b" $bvar 0)] $ appl (mkLet [("a",double 1)]$ lambda "b" $ bvar 0) true

        , mkLet [("id'",mkLet [("a",double 1)] $ lambda "b" $bvar 0)] $ mkLet [("a",double 1)] true
        , mkLet [("id'",mkLet [("a",double 1)] $ lambda "b" $bvar 0)] true
        , true
        ])

  , (appl B.id (mkLet [("b",true),("c",bvar 1)] $ lambda "d" $ bvar 1)
      , [ appl B.id (mkLet [("b",true),("c",true)] $ lambda "d" $ bvar 1)
        , mkLet [("b",true),("c",true)] $ lambda "d" $ bvar 1
        ])

  , (appl (mkLet [("a",plus )] $ appl (lambda "b" $ bvar 1)$ bvar 0)(double 1)
      , [appl (mkLet [("a",plus)] $ appl (lambda "b" $ bvar 1) plus)(double 1)
       ,appl (mkLet [("a", plus)] $ bvar 0)(double 1)
       ,appl (mkLet [("a", plus)] plus)(double 1)
       ,appl plus(double 1)
       ,plus1
       ])

  , (appl(mkLet [("id",B.id)] $ appl (mkLet [("b",false)] $ lambda "c" $ bvar 2) $ double 1)true
      , [ appl (mkLet [("id",B.id)] $ mkLet [("b",false)] $ bvar 1) true
        , appl (mkLet [("id",B.id)] $ mkLet [("b",false)] $ lambda "a" $ bvar 0) true
        , mkLet [("id",B.id)] $ mkLet [("b",false)] true
        , mkLet [("id",B.id)] true
        , true
        ])

  , (appl (mkLet [("a",false)] $ mkLet [("b", plus)] $ bvar 0) $ double 1
      , [ appl (mkLet [("a",false)] $ mkLet [("b",plus)] plus) (double 1)
        , appl (mkLet [("a",false)] plus) (double 1)
        , appl plus (double 1)
        , plus1
        ])

  , (mkLet [("y", lambda "b" $ lambda "c" multiply)
           ,("l",mkLet [("p", plus)]
                       $ lambda "i"$ bvar 1)
           ] $ mkLet [("u", plus)
                     ,("x", double 1)] $
                     bvar 2
    ,[mkLet [("y", lambda "b" $ lambda "c" multiply)
           ,("l",mkLet [("p", plus)]
                       $ lambda "i"$ bvar 1)
           ] $ mkLet [("u", plus)
                     ,("x", double 1)] $
                     mkLet [("p", plus)] $
                           lambda "i"$ bvar 1
    ])
  , (mkLet [("a", lambda "b" $ multiply)] $
           appl (mkLet [("c", multiply)] B.id)
                (lambda "d"$ bvar 1)
    , [mkLet [("a", lambda "b" $ multiply)] $
           mkLet [("c",multiply)] (lambda "d"$ bvar 2)
      ])

  , (appl (appl (mkLet [("id",B.id)] $ bvar 0)(lambda "b" plus)) true
    ,[ appl (appl (mkLet [("id",B.id)] B.id)(lambda "b" plus)) true
     , appl (mkLet [("id",B.id)] $ lambda "b"  plus) true
     , mkLet [("id",B.id)] plus
     , plus
     ])

  ,(appl (lambda "a" $ appl (mkLet [("b",true)] $ lambda "c" $ bvar 2)(bvar 0)) multiply
   ,[ appl (mkLet [("b",true)] $ lambda "c" multiply) multiply
    , mkLet [("b",true)] multiply
    , multiply
    ])
  ]

convergingSet :: [(BruijnTerm () () , [BruijnTerm () () ])]
convergingSet =
  [(appl B.omega B.omega ,
  [appl B.omega B.omega])

  , (mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $ appl (bvar 0) (double 2)
        , [ mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $
                  appl (lambda "a" $ appl (bvar 1) (double 1)) $ double 2
          , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $ appl (bvar 0) (double 1)
          , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $
                  appl (lambda "a" $ appl (bvar 1) (double 1)) $ double 1
          , mkLet [("f", lambda "a" $ appl (bvar 1) (double 1))] $ appl (bvar 0) (double 1)
          ])

  ,  (appl (mkLet [("f", lambda "a" $ appl (bvar 1) (bvar 0))] $ bvar 0) true
        , [ appl (mkLet [("f", lambda "a" $ appl (bvar 1) (bvar 0))] $
              lambda "a" $ appl (bvar 1) (bvar 0)) true
          , mkLet [("f", lambda "a" $ appl (bvar 1) (bvar 0))] $ appl (bvar 0) true
          , mkLet [("f", lambda "a" $ appl (bvar 1) (bvar 0))] $
              appl (lambda "a" $ appl (bvar 1) (bvar 0)) true
          , mkLet [("f", lambda "a" $ appl (bvar 1) (bvar 0))] $ appl (bvar 0) true
          ])

  , (mkLet [("f",lambda "a" $ appl (bvar 1) (bvar 0))] $ appl (bvar 0 ) multiply
      , [mkLet [("f",lambda "a" $ appl (bvar 1) (bvar 0))] $ appl (lambda "a" $ appl (bvar 1) (bvar 0)) multiply
      ,mkLet [("f",lambda "a" $ appl (bvar 1) (bvar 0))] $ appl (bvar 0) multiply
      ,mkLet [("f",lambda "a" $ appl (bvar 1) (bvar 0))] $ appl (lambda "a" $ appl (bvar 1) (bvar 0)) multiply
      ,mkLet [("f",lambda "a" $ appl (bvar 1) (bvar 0))] $ appl (bvar 0) multiply
      ])
  ]
testEvalProp :: TestTree
testEvalProp = testGroup "propertys" $
      -- TODO dont why again is need here but otwersie test will stop after discard
  let forAllNonCiculair prop = again $ forAllTypedBruijn $ \ e -> case sortTerm e of
          Left {} -> discard
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
                in property . isRight $ do
                    t2 <- solver expr2
                    t1 <- solver e
                    return $ counterexample (
                          "\neval\n:" ++ show expr2 ++
                        "\n\npShow:\n " ++ pShow e ++
                          "\n\t::" ++ T.pShow t1 ++
                         "\n\npShow eval:\n" ++ pShow expr2 ++
                          "\n\t::" ++ T.pShow t2)
                        $ unifys t1
                                 (T.mapFree (\ (Free i) -> Free (i + 10000)) t2)
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

hasRelation :: (a -> a -> Bool) -> [a] -> Property
hasRelation _ [] = property True
hasRelation _ [_] = property True
hasRelation relation (a : b : rest )
    | relation a b = hasRelation relation (b : rest)
    | otherwise = property False
