module TestBruijnEnv where

import Test.Tasty.HUnit
import Test.Tasty

import BruijnEnvironment

testBruijnEnv :: TestTree
testBruijnEnv = testGroup "BruijnEnvironment" [testMaybeLookup,testReplace, testSplitAt]

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
    [ testCase "splitAt 1 [[1,2],[3,4]] == ([],[[1,2],[3,4]])" $
        bSplitAt (Bound 1) ( bFromList [[1,2],[3,4::Int]] ) @?= (bFromList [],bFromList [[1,2],[3,4]])


    , testCase "splitAt 2 [[1,2],[3,4]] == ([[1,2]],[[3,4]])" $
        bSplitAt (Bound 2) ( bFromList [[1,2],[3,4::Int]] ) @?= (bFromList [[1,2]],bFromList [[3,4]])
    ]
