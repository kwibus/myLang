module LambdaF where

import Name
import Value
import BruijnEnvironment
import Lambda

data LamTermF i n a = LambdaF i Name a
            | ApplF a a
            | VarF i n
            | ValF i Value
            | PtrF i Bound a --FIXME remove
            | LetF i [DefF i n a] a
            deriving (Eq, Show)

instance Functor (LamTermF i n) where
    fmap f (LambdaF i n t) = LambdaF i n $ f t
    fmap f (ApplF t1 t2) = ApplF (f t1) (f t2)
    fmap _ (VarF i n) = VarF i n
    fmap _ (ValF i v) = ValF i v
    -- fmap f (PtrF i b t) = PtrF i b (f t)
    fmap f (LetF i defs t ) = LetF i (map (fmap f) defs) $ f t

data DefF i n a = DefF i Name a deriving (Eq, Show)

instance Functor (DefF i n) where
    fmap f (DefF i_ n_ t_) = DefF i_ n_ $ f t_

unFDef :: DefF i n (LamTerm i n) -> Def i n
unFDef (DefF i n t) = Def i n t
