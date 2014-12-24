module Type where

type VarType = Int
data Type = SimpleType MonoType | Forall VarType Type
data MonoType = D MonoType | TDouble
