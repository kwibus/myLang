module ArbitraryVallue where

import Test.QuickCheck

import Vallue
import Opperator
import Lambda

arbitraryVallue :: Gen LamTerm
arbitraryVallue = oneof [arbitraryMyDouble, arbitraryBuildIn ]

arbitraryBuildIn :: Gen LamTerm
arbitraryBuildIn = do
    operator <- elements operators
    a <- arbitraryMyDouble
    b <- arbitraryMyDouble
    return $ Appl (Appl (val operator) a )b



arbitraryMyDouble:: Gen LamTerm
arbitraryMyDouble = fmap (val . MyDouble) arbitrary
