module Lambda where

type Name = String

data LamTerm= Lambda Name LamTerm 
            | Appl LamTerm LamTerm 
            | Var Name

instance Show LamTerm where
    show (Var n) = n
    show (Lambda n t) = "\\" ++ n ++ "." ++ show t
    show (Appl t1@ Lambda {} t2)  = parentheses t1 ++ show t2
    show (Appl t1@ Var {}    t2@ Appl  {}) = show t1 ++ parentheses t2
    show (Appl t1@ Var {}    t2) = show t1 ++ show t2
    show (Appl t1@ Appl {}   t2) = show t1 ++ show t2

parentheses :: Show a => a -> String
parentheses s = "(" ++ show s ++ ")"

