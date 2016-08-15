module Lambda where

import Associativity
import Name
import Value

data LamTerm v i n = Lambda v (LamTerm v i n)
            | Appl (LamTerm v i n) (LamTerm v i n)
            | Var i n
            | Lit i Value
            | Let i [Def v i n] (LamTerm v i n)
            deriving (Eq, Show)

data Def v i n = Def v (LamTerm v i n) deriving (Eq, Show)

isInfix :: LamTerm v i Name -> Bool
isInfix (Lit _ v ) = Value.isInfix v
isInfix _ = False

getPrec :: LamTerm v i n -> (Precedence, Associativity)
getPrec (Lit _ BuildIn {fixity = Infix p a}) = (p, a)
getPrec _ = highPrec

accumulateVars :: (HasName v ) => LamTerm v i Name -> ([Name], LamTerm v i Name)
accumulateVars = go []
 where go names (Lambda var t ) = go (getName var: names) t
       go names t = (reverse $ filter (\e-> e/=DummyBegin && e/= DummyEnd) names, t)

accumulateArgs :: LamTerm v i n -> [LamTerm v i n]
accumulateArgs = go []
  where go accuList (Appl t1 t2 ) = go (t2 : accuList) t1
        go accuList t = t : accuList
