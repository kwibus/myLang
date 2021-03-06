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
import MakeTerm
import ExampleBruijn (pair)

fixandsized  :: Int -> Gen a -> IO a
fixandsized size (MkGen g) = return (g fixedseed size)
    where fixedseed = QCGen (mkTheGen 0)

untypeString:: Int -> IO String
untypeString size = fmap PrettyPrint.pShow (fixandsized size genUnTyped)

typedAST :: Int -> IO (BruijnTerm () ())
typedAST size =  fixandsized size (genTyped defaultConf)

main :: IO()
main = defaultMain
 [ bgroup "parser"
    [ env (untypeString 100) $ \str -> bench "random 100"  (nf parseString str)

    , env (untypeString 10000) $ \str -> bench "random 10000"  (nf parseString str)
    ]
 , bgroup "TypeCheck"

    [ env (typedAST 100) $ \ast -> bench "random 100"  (nf solver ast)

    , env (typedAST 10000) $ \ast -> bench "random 10000"  (nf solver ast)

    , bench "dub dub ... " $ nf solver (mkLet [("duplicate",lambda "a" $ appl (appl pair (bvar 0)) (bvar 0))] ( foldr1 appl (replicate 12 (bvar 0) )))
    ]
 ]
