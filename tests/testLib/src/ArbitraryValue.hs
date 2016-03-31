module ArbitraryValue where

import Control.Monad.Trans.Class
import Test.QuickCheck

import MakeTerm
import Logic
import ArbiRef
import GenState

import Value
import Operator
import Type (Type)
import MakeType
import Lambda

shrinkValue :: Value -> [Value]
shrinkValue (Prim (MyDouble n)) = if n == 1.0 then [] else [Prim $ MyDouble 1.0]
shrinkValue _ = []

arbitraryValue :: ArbiRef n => Maybe Type -> Generater ( LamTerm () n)
arbitraryValue t = oneOfLogic [ arbitraryPrimatives t
                              , arbitraryBuildIn t
                              ]

arbitraryBuildIn :: ArbiRef n => Maybe Type -> Generater ( LamTerm () n)
arbitraryBuildIn t = do
    operator <- elementsLogic operators
    unifyGen t (getType operator )
    return $ val operator

arbitraryPrimatives :: ArbiRef n => Maybe Type -> Generater (LamTerm () n)
arbitraryPrimatives t = oneOfLogic $ map ($ t)
  [ mkArbitraryPrimative MyDouble
  , mkArbitraryPrimative MyBool
  ]

mkArbitraryPrimative :: (Arbitrary a ) => (a -> Primative ) -> Maybe Type -> Generater (LamTerm () n)
mkArbitraryPrimative constructor t = do
  unifyGen t $ getTypePrimative $ constructor (error "should never be used")
  d <- lift $ lift arbitrary
  return $ val $ Prim $ constructor d
