module TestArbitrary ( testArbitrary) where

import Test.Tasty
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck ()

import Lambda
import BruijnTerm

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [testProperty "keep falid shrink BruijnTerm" $
        \ t -> seq (shrink (t :: BruijnTerm)) True
    , testProperty "keep falid LamTerm" $
        \ t -> seq (shrink (t :: LamTerm)) True
    ]
