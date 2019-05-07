module TestArbitrary (testArbitrary, size) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Maybe
import Data.Either

import Properties
import ArbitraryLambda

import BruijnTerm
import MakeType
import TypeCheck
import ShrinkLambda

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testGeneration, testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [ testProperty "all normalised untyped" $
       forAllUnTypedBruijn $ \ e -> conjoin (map welFormd (shrinkUntypedBruijn e ))

    , testProperty "all normalised typed " $
       forAllTypedBruijn $ \ e -> conjoin (map welFormd (shrinkTypedBruijn e ))

    , testProperty "all typeable" $
       noShrinking $ forAllTypedBruijn $ \ e -> conjoin (map (\en ->counterexample (pShow en) $ isRight $ solver en ) $ shrinkTypedBruijn  e )
    ]

testGeneration :: TestTree
testGeneration = testGroup "genration"
    [ testProperty "corect size type" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n -> -- TODO dont use arbitrary
            (forAll (resize n $ genTyped defaultConf) (\ t -> size (t :: BruijnTerm () () ) == n)))

    , testProperty "corect size untype" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
            (forAll (resize n genUnTyped ) (\ t -> size (t :: BruijnTerm () ()) == n)))

    , testProperty "typeable" $
        forAllTypedBruijn $ \ e -> isRight $ solver e

    , testProperty "corect type" $
         forAll ( genTerm defaultConf (Just tDouble ))
                (\ e -> isJust e ==> case solver (fromJust (e :: Maybe (BruijnTerm () ()))) of
                    (Right t) -> unifys t tDouble
                    _ -> False
                )
   , testProperty "noncircular" $
        forAllNonCiculair $ not . isCirculair
   ]
