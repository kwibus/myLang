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
            | Let i [Def i (LamTerm i n)] (LamTerm i n)
            deriving (Eq, Show)

data Def i t = Def i Name t deriving (Eq, Show)

instance Functor (Def i) where
  fmap f (Def i n t) = Def i n $ f t

instance Traversable (Def i) where
  traverse f (Def i n t) = Def i n <$> f t

instance Foldable (Def i) where
  foldr f b (Def _ _ a) = f a b

implementation :: Def i t-> t
implementation (Def _ _ t) = t

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

