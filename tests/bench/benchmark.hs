module Main where

import Criterion.Main
import ArbitraryLambda
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

import NFDataInstances()
import PrettyPrint
import Parser
import BruijnTerm
import TypeCheck
fixandsized  :: Int -> Gen a -> IO a
fixandsized size (MkGen g) = return (g fixedseed size)
    where fixedseed = QCGen (mkTheGen 0)

untypeString:: Int -> IO String
untypeString size = fmap PrettyPrint.pShow (fixandsized size genUnTyped)

typedAST :: Int -> IO (BruijnTerm ())
typedAST size =  fixandsized size genTyped

main :: IO()
main = defaultMain
 [ bgroup "parser"
    [ env (untypeString 100) $ \str -> bench "100"  (nf parseString str)

    , env (untypeString 10000) $ \str -> bench "10000"  (nf parseString str)
    ]
 , bgroup "TypeCheck"

    [ env (typedAST 100) $ \str -> bench "100"  (nf solver str)

    , env (typedAST 10000) $ \str -> bench "10000"  (nf solver str)
    ]
 ]
