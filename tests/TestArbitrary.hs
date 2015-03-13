module TestArbitrary ( testArbitrary) where

import Test.Tasty
import Test.Tasty.QuickCheck
import ArbitraryQuickcheck (myArbitraryTerm)
import Data.Maybe

import Vallue
import Lambda
import BruijnTerm
import Type
import TypeCheck
import TestUtils
import Enviroment
import Data.Coerce

testArbitrary :: TestTree
testArbitrary = testGroup "arbitrary" [testshrink]

testshrink :: TestTree
testshrink = testGroup "shrink"
    [testProperty "all normalised" $
       welFormd
    , testProperty "corect size" $
       forAll (suchThat (arbitrary :: Gen Int) (> 1)) (\ n ->
            (forAll (resize n (arbitrary :: Gen (BruijnTerm Vallue) ))
                 (\ t -> size t == n)))

    , testProperty "corect type" $
            (\ n -> forAll ( myArbitraryTerm n (TVal TDouble ))
                (\ e -> isJust e ==> case solver (fromJust e) of
                    (Right t) -> unifys (coerce t) (TVal TDouble ) fEmtyEnv
                    _ -> False
                )
            )
    , testProperty "keep falid shrink BruijnTerm" $
        \ t -> seq (shrink (t :: BruijnTerm Vallue )) True
    , testProperty "keep falid LamTerm" $
        \ t -> seq (shrink (t :: LamTerm Vallue Name)) True
    ]

-- TODO move to different file
size :: LamTerm a i -> Int
size (Lambda _ e ) = (size e) + 1
size (Appl e1 e2) = size e1 + size e2
size Val {} = 1
size Var {} = 1
