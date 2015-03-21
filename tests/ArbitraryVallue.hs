module ArbitraryVallue where


import Control.Monad.Logic
import Test.QuickCheck

import Vallue
import Opperator
import Lambda
import GenState
import Enviroment
import Type
import BruijnTerm

import TypeCheck
import Logic

arbitraryVallue :: Type Free -> FreeEnv (Type Free) -> GenState ->
                    LogicT Gen (FreeEnv (Type Free), BruijnTerm ())
arbitraryVallue t env _ = oneOfLogic [ arbitraryMyDouble t env
                                     , arbitraryBuildIn t env
                                     ]

arbitraryBuildIn :: Type Free -> FreeEnv (Type Free) ->
    LogicT Gen (FreeEnv (Type Free), BruijnTerm ())
arbitraryBuildIn t env = do
    operator <- elementsLogic operators
    case unify t (ftype operator) env of
        Left {} -> mzero
        Right env1 -> return (env1, Val () operator)

arbitraryMyDouble :: Type Free -> FreeEnv (Type Free) ->
    LogicT Gen (FreeEnv (Type Free), BruijnTerm ())
arbitraryMyDouble t env = do
  let u = unify t (TVal TDouble) env
  case u of
    Left {} -> mzero
    Right env1 -> do
      d <- lift arbitrary
      return (env1, Val () (MyDouble d ))
