module TestArbitrary (testArbitrary, size) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Maybe

import TestUtils
import ArbitraryLambda
import Lambda
import Name
import MakeType
import TypeCheck
import ErrorCollector
import BruijnEnvironment

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testGeneration, testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [ testProperty "all normalised untyped" $
       forAllTypedBruijn $ \ e -> conjoin (map welFormd (shrinkUntypedBruijn e ))

    , testProperty "all normalised typed " $
       forAllUnTypedBruijn $ \ e -> conjoin (map welFormd (shrinkTypedBruijn e ))

    , testProperty "all typeable" $
       noShrinking $ forAllTypedBruijn $ \ e -> conjoin (map (\en ->counterexample (printBrujin en) $ hasSucces $ solver en ) $ shrinkTypedBruijn  e )
    ]
testGeneration :: TestTree
testGeneration = testGroup "genration"
    [ testProperty "corect size type" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
            (forAll (resize n genTyped ) (\ t -> size (t :: LamTerm Name () Bound) == n)))

    , testProperty "corect size untype" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
            (forAll (resize n genUnTyped ) (\ t -> size (t :: LamTerm Name () Bound) == n)))

    , testProperty "typeable" $
        forAllTypedBruijn $ \ e -> hasSucces $ solver e

    , testProperty "corect type" $
         forAll ( genTerm (Just tDouble ))
                (\ e -> isJust e ==> case solver (fromJust (e :: Maybe (LamTerm Name () Bound))) of
                    (Result t) -> unifys t tDouble
                    _ -> False
                )
   ]
