module MakeType where

import Type
import FreeEnvironment

infixr 9 ~>
(~>) :: Type -> Type -> Type
t1 ~> t2 = TAppl t1 t2

tVar :: Int -> Type
tVar = TVar . Free

tDouble :: Type
tDouble = TVal TDouble


tBool :: Type
tBool = TVal TBool
