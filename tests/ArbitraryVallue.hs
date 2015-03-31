module ArbitraryVallue where


import Control.Monad.Trans.Class
import Test.QuickCheck

import Vallue
import Opperator
import GenState
import Enviroment
import Type
import BruijnTerm
import MakeTerm
import Logic

arbitraryVallue :: Type Free -> Generater ( BruijnTerm ())
arbitraryVallue t  = oneOfLogic [ arbitraryMyDouble t
                                , arbitraryBuildIn t
                                ]

arbitraryBuildIn :: Type Free ->  Generater ( BruijnTerm ())
arbitraryBuildIn t = do
    operator <- elementsLogic operators
    unifyGen t (ftype operator )
    return $ val operator

arbitraryMyDouble :: Type Free -> Generater (BruijnTerm ())
arbitraryMyDouble t = do
  unifyGen t(TVal TDouble)
  d <- lift arbitrary
  return (val (MyDouble d))
