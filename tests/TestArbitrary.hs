module TestArbitrary (testArbitrary, size) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Maybe

import TestUtils
import ArbitraryLambda

import BruijnTerm
import qualified Type as T
import TypeCheck
import ErrorCollector

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testGeneration, testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [ testProperty "all normalised untyped" $
       forAllTypedBruijn $ \ e -> all (== True) (map welFormd (shrinkUntypedBruijn e ))

    , testProperty "all normalised typed " $
       forAllUnTypedBruijn $ \ e -> all (== True) (map welFormd (shrinkTyped e ))

    , testProperty "all typeable" $
       forAllTypedBruijn $ \ e -> all hasSucces (map solver (shrinkTyped e ))

    ]
testGeneration :: TestTree
testGeneration = testGroup "genration"
    [ testProperty "corect size type" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
            (forAll (resize n genTyped ) (\ t -> size (t :: BruijnTerm ()) == n)))

    , testProperty "corect size untype" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
            (forAll (resize n genUnTyped ) (\ t -> size (t :: BruijnTerm ()) == n)))

    , testProperty "typeable" $
        forAllTypedBruijn $ \ e -> hasSucces $ solver e

    , testProperty "corect type" $
         forAll ( genTerm (Just (T.TVal T.TDouble )))
                (\ e -> isJust e ==> case solver (fromJust (e :: Maybe (BruijnTerm ()))) of
                    (Result t) -> unifys t (T.TVal T.TDouble )
                    _ -> False
                )
   ]
