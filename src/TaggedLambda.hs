module TaggedLambda where

import Name
import Value
import qualified Lambda as Lam
import Unsafe.Coerce

data LamTerm i n t = Lambda i Name (LamTerm i n t)
            | Appl (LamTerm i n t) (LamTerm i n t)
            | Var i n
            | Val i Value
            | Let i [Def i n t] (LamTerm i n t)
            | Tag t (LamTerm i n t)
            deriving (Eq, Show)

data Def i n t = Def i Name (LamTerm i n t) deriving (Eq, Show)

tag :: Lam.LamTerm i n -> LamTerm i n t
tag = unsafeCoerce
-- tag (Lam.Lambda i n t) = Lambda i n $ tag t
-- tag (Lam.Appl t1 t2)  = Appl (tag t1)(tag t2)
-- tag (Lam.Var i n) = Var i n
-- tag (Lam.Val i v)  = Val i v
-- tag (Lam.Let i defs t ) = Let i (map tagDef defs) $ tag t
--   where
--     tagDef (Lam.Def  i' n' t')  = Def i' n' $ tag t'

mapImplementation :: (LamTerm i n1 t -> LamTerm i n2 t) -> Def i n1 t -> Def i n2 t
mapImplementation f (Def i n t) = Def i n (f t)
