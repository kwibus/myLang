module Lambda where

import Associativity
import Name
import Value

data LamTerm lam i n = Lambda lam (LamTerm lam i n)
            | Appl (LamTerm lam i n) (LamTerm lam i n)
            | Var i n
            | Lit i Value
            | Let i [Def lam i n] (LamTerm lam i n)
            deriving (Eq, Show)

data Def lam i n = Def i Name (LamTerm lam i n) deriving (Eq, Show)

isInfix :: LamTerm lam i Name -> Bool
isInfix (Lit _ v ) = Value.isInfix v
isInfix _ = False

getPrec :: LamTerm lam i n -> (Precedence, Associativity)
getPrec (Lit _ BuildIn {fixity = Infix p a}) = (p, a)
getPrec _ = highPrec

accumulateVars :: (HasName lam) => LamTerm lam i Name -> ([Name], LamTerm lam i Name)
accumulateVars = go []
 where go names (Lambda var t ) = go (getName var: names) t
       go names t = (reverse $ filter (\e-> e/=DummyBegin && e/= DummyEnd) names, t)

accumulateArgs :: LamTerm lam i n -> [LamTerm lam i n]
accumulateArgs = go []
  where go accuList (Appl t1 t2 ) = go (t2 : accuList) t1
        go accuList t = t : accuList
