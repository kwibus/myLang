module MakeType where

import Type
import FreeEnvironment

infixr 9 ~>
(~>) :: TypeA a -> TypeA a -> TypeA a
t1 ~> t2 = TAppl t1 t2

tVar :: Int -> Type
tVar i = TVar (Free i) ()

tPoly :: Int -> Type
tPoly i = TPoly (Free i )()

tDouble :: Type
tDouble = TVal TDouble

tBool :: Type
tBool = TVal TBool
