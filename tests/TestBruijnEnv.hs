module TestBruijnEnv where

import Test.Tasty.HUnit
import Test.Tasty
import TestFail
import BruijnEnvironment

testBruijnEnv :: TestTree
testBruijnEnv = testGroup "BruijnEnvironment" [testbAppend,testMaybeLookup,testReplace, testLevel ,testSplitAt,testAux]

testbAppend :: TestTree
testbAppend = testGroup "bAppend"
    [ testCase "bAppend [[1],[2]] [[3],[4]]"$ bAppend (bFromList [[1::Int],[2]]) (bFromList [[3],[4]]) @?= bFromList [[1],[2],[3],[4]]
    -- , testCase "bAppend [[1],[2]] [[]]"$ bAppend (bFromList [[1],[2]]) (bFromList [[]]) @?= bFromList [[1],[2]]
    , testCase "bAppend [[1],[2]] []"$ bAppend (bFromList [[1::Int],[2]]) (bFromList []) @?= bFromList [[1],[2]]
    ]

testMaybeLookup :: TestTree
testMaybeLookup = testGroup "bMaybeLookup"
    [ testCase "bmaybeLookup 1 [[1,2,3]] == 2" $
        bMaybeLookup  (Bound 1) ( bFromList [[1,2,3::Int]] ) @?= Just 2

    , testCase "bmaybeLookup 2 [[1,2]] == Nothing " $
        bMaybeLookup  (Bound 2) ( bFromList [[1,2::Int]] ) @?= Nothing

    , testCase "bmaybeLookup 2 [[1,2],[3,4]] == 3" $
        bMaybeLookup  (Bound 2) ( bFromList [[1,2],[3,4::Int]] ) @?= Just 3

    , testCase "bmaybeLookup 2 [[1,2],[]] == Nothing" $
        bMaybeLookup  (Bound 2) ( bFromList [[1,2::Int],[]] ) @?= Nothing

    , testCase "bmaybeLookup 0 [[]] == Nothing" $
        bMaybeLookup  (Bound 0) ( bFromList [[]::[Int]] ) @?= Nothing
    ]

testReplace :: TestTree
testReplace = testGroup "replace"
    [ testCase "replace 1 0 [[1,2,3]] == [[1,0,3]]" $
        bReplace (Bound 1) 0 ( bFromList [[1,2,3::Int]] ) @?= bFromList [[1,0,3]]

    , testCase "replace 2 0 [[1,2],[3,4]] == 3" $
        bReplace(Bound 2) 0 ( bFromList [[1,2],[3,4::Int]] ) @?= bFromList [[1,2],[0,4]]
    ]


testSplitAt :: TestTree
testSplitAt = testGroup "bSplitAt"
    [ testCase "splitAt 0 [[1,2] [3,4]] = [[1,2] [3,4]]" $
        bSplitAt (Bound 0) (bFromList [[1,2],[3,4::Int]]) @?= (bFromList [], bFromList [[1,2],[3,4]])

    , testCase "splitAt 1 [[1,2],[3,4]] == ([],[[1,2],[3,4]])" $
        bSplitAt (Bound 1) ( bFromList [[1,2],[3,4::Int]] ) @?= (bFromList [],bFromList [[1,2],[3,4]])

    , testCase "splitAt 2 [[1,2],[3,4]] == ([[1,2]],[[3,4]])" $
        bSplitAt (Bound 2) ( bFromList [[1,2],[3,4::Int]] ) @?= (bFromList [[1,2]],bFromList [[3,4]])
    ]

testLevel :: TestTree
testLevel = testGroup "level"
    [ assertFailWith "level 1  [a] = fail" ( getDepth (Bound 1) (bFromList [[1::Int]] )) "not in scope"
    , testCase "level 2 [[1],[2,3]] = 1"  $ getDepth (Bound 2) (bFromList [[1],[2,3::Int]]) @?= 1
    , testCase "matchLevels [1] [[2][3]]" $ matchLevels (bFromList [[1]] ) (bFromList [[2],[3::Int]]) @?= bFromList [[3]]
    ]

testAux :: TestTree
testAux= testGroup "aux"
    [ testCase "bfoldl + 0 [1,2][3,4]=10" $ bfoldl (+) 0 (BEnv [[1,2],[3,4::Int]])  @?= 10
    , testCase "transforml b+a 0 [[1,2][3,4]]" $ transforml (\b a -> (b+1,b+a)) 0 (BEnv [[1,2],[3,4::Int]])
        @?= (4,BEnv [[1,3],[5,7]])
    ]
