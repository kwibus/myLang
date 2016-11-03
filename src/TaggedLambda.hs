module TaggedLambda where

import Name
import Value
import qualified Lambda as Lam

data LamTermT i n t = Lambda i Name (LamTermT i n t)
            | Appl (LamTermT i n t) (LamTermT i n t)
            | Var i n
            | Val i Value
            | Let i [DefT i n t] (LamTermT i n t)
            | Tag t (LamTermT i n t)
            deriving (Eq, Show)

data DefT i n t = DefT i Name (LamTermT i n t) deriving (Eq, Show)

tag :: Lam.LamTerm i n -> LamTermT i n t
tag (Lam.Lambda i n t) = Lambda i n $ tag t
tag (Lam.Appl t1 t2)  = Appl (tag t1)(tag t2)
tag (Lam.Var i n) = Var i n
tag (Lam.Val i v)  = Val i v
tag (Lam.Let i defs t ) = Let i (map tagDef defs) $ tag t
  where
    tagDef (Lam.Def  i' n' t')  = DefT i' n' $ tag t'
