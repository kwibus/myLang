module ArbitraryVallue where

import Control.Monad.Trans.Class
import Test.QuickCheck

import Vallue
import Opperator
import GenState
import Enviroment
import Type
import Lambda
import MakeTerm
import Logic
import ArbiRef

shrinkValue :: Vallue -> [Vallue]
shrinkValue (MyDouble n) = if n == 1.0 then [] else [MyDouble 1.0]
shrinkValue _ = []

arbitraryVallue :: ArbiRef n => Maybe (Type Free) -> Generater ( LamTerm () n)
arbitraryVallue t = oneOfLogic [ arbitraryMyDouble t
                               , arbitraryBuildIn t
                               ]

arbitraryBuildIn :: ArbiRef n => Maybe (Type Free) -> Generater ( LamTerm () n)
arbitraryBuildIn t = do
    operator <- elementsLogic operators
    unifyGen t (ftype operator )
    return $ val operator

arbitraryMyDouble :: ArbiRef n => Maybe (Type Free) -> Generater (LamTerm () n)
arbitraryMyDouble t = do
  unifyGen t (TVal TDouble)
  d <- lift arbitrary
  return (val (MyDouble d))
