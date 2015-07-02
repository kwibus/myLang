module TestArbitrary ( testArbitrary,size) where

import Test.Tasty
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck (myArbitraryTerm)
import Data.Maybe

import Names
import Lambda
import BruijnTerm
import Type
import TypeCheck
import TestUtils
import Enviroment

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testGeneration, testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [testProperty "all normalised" $
       \ e -> all (== True) (map welFormd (shrink (e :: BruijnTerm () )))

    , testProperty "keep falid shrink BruijnTerm" $
        \ t -> seq (shrink (t :: BruijnTerm ())) True
    , testProperty "keep falid LamTerm" $
        \ t -> seq (shrink (t :: LamTerm () Name)) True
    ]
testGeneration :: TestTree
testGeneration = testGroup "genration"
    [ testProperty "corect size" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
            (forAll (resize n (arbitrary :: Gen (BruijnTerm ()) ))
                 (\ t -> size t == n)))

    , testProperty "corect type" $
            (\ n -> forAll ( myArbitraryTerm n (TVal TDouble ))
                (\ e -> isJust e ==> case solver (fromJust e) of
                    (Right t) -> unifys (typeBound2Free t) (TVal TDouble ) fEmtyEnv
                    _ -> False
                )
            )
   ]
--
-- TODO move to different file
size :: LamTerm a i -> Int
size (Lambda _ _ e ) = (size e) + 1
size (Appl _ e1 e2) = size e1 + size e2
size Val {} = 1
size Var {} = 1
