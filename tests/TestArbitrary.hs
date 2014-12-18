module TestArbitrary ( testArbitrary) where
import Test.Tasty

import Lambda

import Test.Tasty.QuickCheck

import ArbitraryQuickcheck ()

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [testProperty "keep falid shrink BruijnTerm" $
        \ t -> seq (shrink (t :: BruijnTerm)) True
    , testProperty "keep falid LamTerm" $
        \ t -> seq (shrink (t :: LamTerm)) True
    ]
