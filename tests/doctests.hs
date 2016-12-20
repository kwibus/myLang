-- {-# OPTIONS_GHC -F -pgmF doctest-discover #-}
module Main where

import Test.DocTest

main :: IO ()
main =  doctest ["-itest","-isrc","tests/MakeTerm.hs","src/Operator","src/PrettyPrint.hs","tests/Properties.hs","src/TypeCheck.hs"]

