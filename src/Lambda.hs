module Lambda where

import Names
import Vallue


data LamTerm i n = Lambda i Name (LamTerm i n)
            | Appl i (LamTerm i n) (LamTerm i n)
            | Var i n
            | Val i Vallue
            -- | Let [Def i n] (LamTerm i n)
            deriving (Eq, Show)

-- data Def i n = Def i (VarDef i n) (LamTerm i n) deriving (Eq, Show)
-- data VarDef i n = VarDef Name deriving (Eq, Show)
