module TestTopologicalSort where

import Test.Tasty
import Test.Tasty.HUnit

import BruijnEnvironment
import MakeTerm
import TopologicalSort
import BruijnTerm
import Modify
import Properties
import TestUtils

testTopologicalSort :: TestTree
testTopologicalSort = testGroup "topologicalSort"
  [testTopological, testNonCirculair, testCirculair, testSort]

testNonCirculair :: TestTree
testNonCirculair = testGroup "noncircular" $ map
    (\ t -> testCase (removeNewLines $ pShow t) $ isCirculair t @?= False)
    noncircular

testCirculair :: TestTree
testCirculair = testGroup "circular" $ map
    (\ t -> testCase (removeNewLines $ pShow t) $ isCirculair t @?= True)
    circular

testSort :: TestTree
testSort = testGroup "sortTerm" $ map
    (\ (t1, t2) -> testCase (removeNewLines $ pShow t1) $ fmap applyModify (sortTerm t1) @?= t2 )
    sortTermExample

testTopological :: TestTree
testTopological = testGroup "topologicalSort" $ map
   (\ (d1, d2, r) -> testCase (show d1 ++ show d2) $ topologicalSort d1 d2 @?= r) topologicalSortExample

circular :: [BruijnTerm () ]
circular =
  [ mkLet [("a", bvar 0)] $ bvar 0
  , mkLet [("a", bvar 0), ("b", bvar 1)] $ bvar 0
  , mkLet [("a", appl (bvar 1) (bvar 0))] $ bvar 0
  , mkLet [("a", mkLet [("b", bvar 1)] $ bvar 0)] $ bvar 0
  , mkLet [("a", bvar 0 ), ("b", bvar 0)] false
  , mkLet [("f", appl (bvar 0) true), ("g", lambda "a" $ bvar 2)] $ bvar 0
  , mkLet [("f", appl (lambda "a" true) (bvar 0))] $ bvar 0
  , mkLet [("f", appl (lambda "a" (bvar 1)) true)] $ bvar 0
  , mkLet [("f", lambda "x" (bvar 1)), ("y", bvar 0)] $ bvar 0
  , mkLet [("f", lambda "x" (bvar 1)), ("y", appl (bvar 1) true)] $ bvar 0
  , mkLet [("y", appl (bvar 0) true), ("f", lambda "x" (bvar 2))] $ bvar 0
  , mkLet [("y", appl (bvar 1) true), ("f", lambda "x" (bvar 3)), ("z", bvar 1)] $ bvar 0
  , mkLet [("x", mkLet [("y", bvar 1)] $ bvar 0)] true
  ]

noncircular :: [BruijnTerm () ]
noncircular =
    [ mkLet [("f", lambda "a" $ bvar 0), ("g", lambda "a" $ bvar 1)] $ bvar 0
    , mkLet [("f", lambda "x" $ lambda "y" (bvar 2)), ("y", true)] false
    , lambda "a" $ mkLet [("b", bvar 0), ("c", true), ("d", mkLet [("e", false)] (bvar 2))] (bvar 2)
    , mkLet [("f", lambda "a" $ appl (bvar 1) (bvar 0)), ("g", lambda "a" $ appl (bvar 2) (bvar 0))] $ bvar 0
    , mkLet [("f", lambda "x" $ mkLet [("y", bvar 2)] $ bvar 0)] true
    ]

sortTermExample :: [(BruijnTerm (), Either (DataCycle ()) (BruijnTerm ()))]
sortTermExample =
  [ ( mkLet [("a", bvar 0), ("b", true)] $ bvar 0
    , return $ mkLet [("b", true), ("a", bvar 1)] $ bvar 1)

  , ( mkLet [("a", appl (bvar 0) (bvar 1)), ("b", bvar 3 ), ("c", bvar 1)] $ bvar 2
    , return $ mkLet [("b", bvar 3), ("c", bvar 2), ("a", appl (bvar 1) (bvar 2))] $ bvar 0)

  , ( mkLet [("b", bvar 3), ("a", appl (bvar 0) (bvar 2)), ("c", bvar 2)] $ bvar 1
    , return $ mkLet [("b", bvar 3), ("c", bvar 2), ("a", appl (bvar 1) (bvar 2))] $ bvar 0)

  , ( mkLet [("c", bvar 1), ("b", bvar 3), ("a", appl (bvar 2) (bvar 1))] $ bvar 0
    , return $ mkLet [("b", bvar 3), ("c", bvar 2), ("a", appl (bvar 1) (bvar 2))] $ bvar 0)

  , ( mkLet [("a", bvar 0), ("b", true), ("c", false)] $ bvar 1
    , return $ mkLet [("c", false), ("b", true), ("a", bvar 2)] $ bvar 1)

  , ( mkLet [("a", bvar 0)] $ mkLet [("b", bvar 0)] $ bvar 0
    , Left $ DataCycle (mkLet [("b", bvar 0)] $ bvar 0 )
                        [Bound 0, Bound 0])
  ]

topologicalSortExample :: [([(Char, [Char])], [(Char, [Char])] , Either [Char] [Char])]
topologicalSortExample =
    [ ([]          , []                       , return "")
    , ([('a', "a")], []                       , Left "aa")
    , ([]          , [('a', "a")]             , return "a")
    , ([('a', "b")], []                       , return "ba")
    , ([]          , [('a', "b") , ('b', "a")], return "ba")
    , ([('b', "a")], [('a', "a")]             , return "ab")
    , ([('b', "a")], [('a', "b")]             , Left "bab" )
    , ([('c', "a")], [('a', "b") , ('b', "a")], return "bac")
    ]
