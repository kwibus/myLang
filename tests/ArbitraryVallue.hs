module ArbitraryVallue where

import Test.QuickCheck

import Vallue
import Opperator
import Lambda

arbitraryVallue :: Gen (LamTerm Vallue Name)
arbitraryVallue = oneof [arbitraryMyDouble, arbitraryBuildIn ]

arbitraryBuildIn :: Gen (LamTerm Vallue Name)
arbitraryBuildIn = do
    operator <- elements operators
    a <- arbitraryMyDouble
    b <- arbitraryMyDouble
    return $ Appl (Appl (Val operator) a ) b

arbitraryMyDouble :: Gen (LamTerm Vallue Name)
arbitraryMyDouble = fmap (Val . MyDouble) arbitrary
