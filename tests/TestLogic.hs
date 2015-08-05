module TestLogic where

import Data.Maybe
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.Providers
import Control.Monad

import GenState
import Logic

testLogic :: TestTree
testLogic = testGroup "Logic"
    [ allJust "1" $ do
        i <- elementsLogic [1 .. 100 :: Int]
        if i < 90 then mzero else return i
    , allJust "2" $ do
        i <- chooseLogic (1, 100 :: Int)
        if i < 90 then mzero else return i
    , allJust "3" $ do
        i <- oneOfLogic $ replicate 90 mzero ++ map return [1 .. 10 :: Int]
        if i < 9 then mzero else return i
    , allJust "4" $ do
        i <- oneOfLogic [elementsLogic [], return 1, return (10 :: Int)]
        if i < 9 then mzero else return i
    , allJust "5" $ do
        i <- oneOfLogic [chooseLogic (100, 1), return (10 :: Int)]
        if i < 9 then mzero else return i
    , allJust "6" $
        let f i n m = do
                j <- chooseLogic (1, i :: Int)
                if j + n > 5 then mzero
                    else if m < 0
                        then return (1 :: Int)
                        else f i (n + 1) (m - 1)
        in do
            i <- chooseLogic (1, 10 :: Int)
            n <- chooseLogic (1, 10 :: Int)
            m <- chooseLogic (1, 10 :: Int)
            f i n m
    , allJust "7" $
        oneOfLogic
            [ newFreeVar >> newFreeVar >> newFreeVar >> mzero
            , do
                i <- getMax
                if i == 0 then return (1 :: Int) else mzero
            ]
    ]

allJust :: Show a => TestName -> Generater a -> TestTree
allJust name g = testPropertyWith name (runGenerartor g ) isJust
--
testPropertyWith :: (Testable prop, Show a) => TestName -> Gen a -> (a -> prop) -> TestTree
testPropertyWith name gen test = singleTest name $ QC $ forAll gen test
