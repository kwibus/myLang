module Lambda where

import Associativity
import Name
import Value

data LamTerm i n = Lambda i Name (LamTerm i n)
            | Appl (LamTerm i n) (LamTerm i n)
            | Var i n
            | Lit i Value
            | Let i [Def i n] (LamTerm i n)
            deriving (Eq, Show)

data Def i n = Def i Name (LamTerm i n) deriving (Eq, Show)

isInfix :: LamTerm i Name -> Bool
isInfix (Lit _ v ) = Value.isInfix v
isInfix _ = False

getPrec :: LamTerm i n -> (Precedence, Associativity)
getPrec (Lit _ BuildIn {fixity = Infix p a}) = (p, a)
getPrec _ = highPrec

accumulateVars :: LamTerm i Name -> ([Name], LamTerm i Name)
accumulateVars = go []
 where go names (Lambda _ name t ) = go (name : names) t
       go names t = (reverse $ filter (\e-> e/=DummyBegin && e/= DummyEnd) names, t)

accumulateArgs :: LamTerm i n -> [LamTerm i n]
accumulateArgs = go []
  where go accuList (Appl t1 t2 ) = go (t2 : accuList) t1
        go accuList t = t : accuList
