module Lambda where

import Names
import Vallue


data LamTerm i n = Lambda i Name (LamTerm i n)
            | Appl i (LamTerm i n) (LamTerm i n)
            | Var i n
            | Val i Vallue
            deriving (Eq, Show)
