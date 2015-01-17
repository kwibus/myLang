module Main (main ) where

import ArbitraryQuickcheck ()
import Test.QuickCheck
import Lambda

main :: IO ()
main = do
    list <- mapM f [0, 2 .. 100]
    mapM_ (print . pShow) list
        where f i = generate $ resize i arbitrary
