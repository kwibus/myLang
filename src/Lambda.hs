module Lambda
    ( module Lambda
    , Name ()
    , Value ()
    )
where

import Associativity
import Name
import Value

data LamTerm i j n = Lambda i Name (LamTerm i j n)
            | Appl (LamTerm i j n) (LamTerm i j n)
            | Var j n
            | Val j Value
            | Let j [Def i (LamTerm i j n)] (LamTerm i j n)
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

isInfix :: LamTerm i j Name -> Bool
isInfix (Val _ v ) = Value.isInfix v
isInfix _ = False

getPrec :: LamTerm i j n -> (Precedence, Associativity)
getPrec (Val _ BuildIn {fixity = Infix p a}) = (p, a)
getPrec _ = highPrec

accumulateArgs :: LamTerm i j n -> [LamTerm i j n]
accumulateArgs = go []
  where go accuList (Appl t1 t2 ) = go (t2 : accuList) t1
        go accuList t = t : accuList

accumulateVars :: LamTerm i j n -> ([Name], LamTerm i j n)
accumulateVars = go []
  where
    go names (Lambda _ name t ) = go (name : names) t
    go names t = (reverse names, t)

unsafeGetVal :: LamTerm i j n -> Value
unsafeGetVal (Val _ v) = v
unsafeGetVal _ = error "was not a vallue"
