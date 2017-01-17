module Lambda
    ( module Lambda
    , Name ()
    , Value ()
    )
where

import Associativity
import Name
import Value

data LamTerm i n = Lambda i Name (LamTerm i n)
            | Appl (LamTerm i n) (LamTerm i n)
            | Var i n
            | Val i Value
            | Let i [Def i n] (LamTerm i n)
            deriving (Eq, Show)

data Def i n = Def i Name (LamTerm i n) deriving (Eq, Show)

implementation :: Def i n-> LamTerm i n
implementation (Def _ _ t) = t

mapImplementation :: (LamTerm i n1 -> LamTerm i n2) -> Def i n1-> Def i n2
mapImplementation f (Def i n t) = Def i n (f t)

isInfix :: LamTerm i Name -> Bool
isInfix (Val _ v ) = Value.isInfix v
isInfix _ = False

getPrec :: LamTerm i n -> (Precedence, Associativity)
getPrec (Val _ BuildIn {fixity = Infix p a}) = (p, a)
getPrec _ = highPrec

accumulateArgs :: LamTerm i n -> [LamTerm i n]
accumulateArgs = go []
  where go accuList (Appl t1 t2 ) = go (t2 : accuList) t1
        go accuList t = t : accuList
