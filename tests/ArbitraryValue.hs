module ArbitraryValue where

import Control.Monad.Trans.Class
import Test.QuickCheck

import MakeTerm
import Logic
import ArbiRef
import GenState

import Value
import Operator
import Environment
import Type
import Lambda

shrinkValue :: Value -> [Value]
shrinkValue (MyDouble n) = if n == 1.0 then [] else [MyDouble 1.0]
shrinkValue _ = []

arbitraryValue :: ArbiRef n => Maybe Type -> Generater ( LamTerm () n)
arbitraryValue t = oneOfLogic [ arbitraryMyDouble t
                               , arbitraryBuildIn t
                               ]

arbitraryBuildIn :: ArbiRef n => Maybe Type -> Generater ( LamTerm () n)
arbitraryBuildIn t = do
    operator <- elementsLogic operators
    unifyGen t (getType operator )
    return $ val operator

arbitraryMyDouble :: ArbiRef n => Maybe Type -> Generater (LamTerm () n)
arbitraryMyDouble t = do
  unifyGen t (TVal TDouble)
  d <- lift $ lift arbitrary
  return (val (MyDouble d))
