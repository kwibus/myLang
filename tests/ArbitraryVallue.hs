module ArbitraryVallue where

import Test.QuickCheck
import Vallue

arbitraryVallue :: Gen Vallue
arbitraryVallue = fmap MyDouble arbitrary
