module TestArbitrary (testArbitrary, size) where

import Test.Tasty
import Test.Tasty.QuickCheck
import Data.Either
import Data.Maybe

import TestUtils
import ArbitraryLambda

import Environment
import BruijnTerm
import Lambda
import Type
import TypeCheck

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testGeneration, testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [ testProperty "all normalised untyped" $
       forAllTypedBruijn $ \ e -> all (== True) (map welFormd (shrinkUntypedBruijn e ))

    , testProperty "all normalised typed " $
       forAllUnTypedBruijn $ \ e -> all (== True) (map welFormd (shrinkTyped e ))

    , testProperty "all typeable" $
       forAllTypedBruijn $ \ e -> all isRight (map solver (shrinkTyped e ))

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
        forAllTypedBruijn $ \ e -> isRight $ solver e

    , testProperty "corect type" $
         forAll ( genTerm (Just (TVal TDouble )))
                (\ e -> isJust e ==> case solver (fromJust (e :: Maybe (BruijnTerm ()))) of
                    (Right t) -> unifys (typeBound2Free t) (TVal TDouble ) fEmtyEnv
                    _ -> False
                )
   ]

-- TODO move to different file
size :: LamTerm a i -> Int
size (Lambda _ _ e ) = size e + 1
size (Appl _ e1 e2) = size e1 + size e2
size Val {} = 1
size Var {} = 1
