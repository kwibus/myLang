module Lambda where

type Name = String

data LamTerm v i = Lambda Name (LamTerm v i)
            | Appl (LamTerm v i) (LamTerm v i)
            | Var i
            | Val v
            deriving (Eq, Show)
